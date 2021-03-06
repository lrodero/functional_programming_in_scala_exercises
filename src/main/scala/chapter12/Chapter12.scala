package chapter12

import scala.language.higherKinds

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] { self => // See 12_8 and 12_9
  def unit[A](a:A): F[A]
  def map2[A,B,C](fa:F[A], fb:F[B])(f:(A,B) => C): F[C]

  override def map[A,B](fa:F[A])(f: A => B): F[B] = 
    map2(fa,unit(()))((a,_) => f(a))

  // Exercise 12_1
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil => unit(List[A]())
    case fa :: tail => map2(fa, sequence(tail))(_ :: _)
  }
  def replicateM[A](n:Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa,fb)((_,_))

  // Exercise 12_2
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab,fa)((f2,a) => f2(a))
  def mapInTermsOfApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)
  def map2InTermsOfApplyAndUnit[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    map(apply(apply(unit((a:A) => ((b:B) => (a,b))))(fa))(fb))(f.tupled)

  // Exercise 12_3
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] =
    apply(apply(apply(unit(f.curried): F[A => (B => (C => D))])(fa))(fb))(fc) 
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried): F[A => (B => (C => (D => E)))])(fa))(fb))(fc))(fd)

  // Exercise 12_8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x]=(F[x],G[x])})#f] = {
    new Applicative[({type f[x]=(F[x],G[x])})#f] {
      override def unit[A](a:A): (F[A],G[A]) = (self.unit(a), G.unit(a))
      override def map2[A,B,C](fa:(F[A],G[A]), fb:(F[B],G[B]))(fu:(A,B) => C): 
        (F[C],G[C]) = {
        (self.map2(fa._1,fb._1)(fu), G.map2(fa._2,fb._2)(fu))
      }
    }
  }

  // Exercise 12_9
  def compose[G[_]](ag:Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    def unit2[A](a:A): G[A] = ag.unit(a)
    override def unit[A](a:A): F[G[A]] = self.unit(ag.unit(a))
    override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C): F[G[C]] = self.map2(fga,fgb){case (ga,gb) => ag.map(ag.product(ga,gb))(f.tupled)}
  }

  // Exercise 12_12
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldRight[F[Map[K,V]]](unit(Map.empty[K,V])){case ((k,fv),fm) =>
      map2[Map[K,V],V,Map[K,V]](fm,fv){case (m,v) => m + (k -> v)}
    }
  }


}

object Exercise_12_3 {
  def map3[F[_]:Applicative,A,B,C,D](fa:F[A],fb:F[B],fc:F[C])(f: (A,B,C) => D): F[D] = {
    val applicative = implicitly[Applicative[F]]
    val fcd: F[C => D] = applicative.map2(fa,fb)((a,b) => f.curried(a)(b))
    applicative.map2(fcd,fc)((func,c) => func(c))
  } 
  def map4[F[_]:Applicative,A,B,C,D,E](fa:F[A],fb:F[B],fc:F[C],fd:F[D])(f: (A,B,C,D) => E): F[E] = {
    val applicative = implicitly[Applicative[F]]
    val fde: F[D => E] = map3(fa,fb,fc)((a,b,c) => f.curried(a)(b)(c))
    applicative.map2(fde,fd)((func,d) => func(d))
  }
}

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](fa:F[A])(f:A => F[B]): F[B]

  override def map2[A,B,C](fa:F[A],fb:F[B])(f: (A,B) => C): F[C] =
    flatMap(fa){a => map(fb)(b => f(a,b)) }

}

object StreamApplicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a:A): Stream[A] = Stream(a)
    def map2[A,B,C](sa:Stream[A],sb:Stream[B])(f: (A,B) => C): Stream[C] =
      (sa zip sb) map f.tupled
  }
}

object Exercise_12_5 {
  def EitherMonad[E] = new Monad[({type f[x] = Either[E,x]})#f] {
    override def unit[A](a:A): Either[E,A] = Right(a)
    override def flatMap[A,B](a:Either[E,A])(f:A => Either[E,B]): Either[E,B] = a match {
      case Left(e) => new Left(e)
      case Right(a) => f(a)
    }
  }
}

sealed trait Validation[+E,+A]
case class Failure[E](e:E, es: List[E] = List.empty[E]) extends Validation[E,Nothing]
case class Success[A](a:A) extends Validation[Nothing,A]

object Exercise_12_6 {
  def ValidationApplicative[E]: Applicative[({type f[x]=Validation[E,x]})#f] =
    new Applicative[({type f[x]=Validation[E,x]})#f] {
      override def unit[A](a: A): Validation[E,A] = Success(a)
      override def map2[A,B,C](va: Validation[E,A], vb: Validation[E,B])(f:(A,B) => C): Validation[E,C] =
        (va,vb) match {
          case (Failure(e1, es1), Failure(e2, es2)) => Failure(e1, e2 :: es1 ::: es2)
          case (e@Failure(e1, es1), _) => e
          case (_, e@Failure(e2, es2)) => e
          case (Success(a), Success(b)) => Success(f(a,b))
        }
    }
}

object Exercises_12_13_And_12_14 {
  // Exercise 12_13
  trait Traversable[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
    def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
    // Exercise 12_14
    case class Id[A](a: A)
    implicit val appId = new Applicative[Id] {
      override def unit[A](a:A): Id[A] = Id(a)
      override def map2[A,B,C](ida: Id[A], idb: Id[B])(f: (A,B) => C): Id[C] = (ida,idb) match {
        case (Id(a),Id(b)) => Id(f(a,b))
      }
    }
    override def map[A,B](fa: F[A])(f: A => B): F[B] = traverse(fa)((a:A) => Id(f(a))) match {
      case Id(fb) => fb
    }
  }
  val traversableList: Traversable[List] = new Traversable[List] {
    override def map[A,B](la: List[A])(f: A => B): List[B] = la map f // Functor function, also required
    //override def traverse[G[_]: Applicative,A,B](as: List[A])(f: A => G[B]): G[List[B]] = ??? traverse is defined in terms of sequence, so no need to rewrite it
    override def sequence[G[_]: Applicative,A](lga: List[G[A]]): G[List[A]] = {
      val appg = implicitly[Applicative[G]]
      lga.foldRight(appg.unit(List.empty[A])){ (ga,gla) => appg.map2(ga,gla)((a,as) => a :: as) }
    }
  }
  val traversableOption: Traversable[Option] = new Traversable[Option] {
    override def map[A,B](oa: Option[A])(f: A => B): Option[B] = oa map f // Functor function, also required
    //override def traverse[G[_]: Applicative,A,B](as: Option[A])(f: A => G[B]): G[Option[B]] = ??? traverse is defined in terms of sequence, so no need to rewrite it
    override def sequence[G[_]: Applicative,A](lga: Option[G[A]]): G[Option[A]] = {
      val appg = implicitly[Applicative[G]]
      lga match {
        case None => appg.unit[Option[A]](None)
        case Some(ga) => appg.map(ga)((a:A) => Some(a))
      }
    }
  }
  case class Tree[+T](head: T, tail: List[Tree[T]])
  val traversableTree: Traversable[Tree] = new Traversable[Tree] {
    override def map[A,B](ta: Tree[A])(f: A => B): Tree[B] = ta match { // Functor function, also required
      case Tree(head, tail) => Tree(f(head), tail.map(tree => traversableTree.map(tree)(f)))
    }
    //override def traverse[G[_]: Applicative,A,B](as: Tree[A])(f: A => G[B]): G[Tree[B]] = ??? traverse is defined in terms of sequence, so no need to rewrite it
    override def sequence[G[_]: Applicative,A](tga: Tree[G[A]]): G[Tree[A]] = {
      val appg = implicitly[Applicative[G]]
      tga match {
        case Tree(ga, ltga) => {
          val lgta: List[G[Tree[A]]] = ltga.map((tga:Tree[G[A]]) => sequence(tga))
          val glta: G[List[Tree[A]]] = traversableList.sequence(lgta)
          appg.map2(ga, glta)( (a, lta) => Tree(a,lta) )
        }
      }
    }
  }
}
