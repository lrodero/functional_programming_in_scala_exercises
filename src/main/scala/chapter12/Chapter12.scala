package chapter12

import scala.language.higherKinds

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
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
    val self = this
    new Applicative[({type f[x]=(F[x],G[x])})#f] {
      override def unit[A](a:A): (F[A],G[A]) = (self.unit(a), G.unit(a))
      override def map2[A,B,C](fa:(F[A],G[A]), fb:(F[B],G[B]))(fu:(A,B) => C): 
        (F[C],G[C]) = {
        (self.map2(fa._1,fb._1)(fu), G.map2(fa._2,fb._2)(fu))
      }
    }
  }

  // Exercise 12_12
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldRight[F[Map[K,V]]](unit(Map.empty[K,V])){case ((k,fv),fm) =>
      map2[Map[K,V],V,Map[K,V]](fm,fv){case (m,v) => m + (k -> v)}
    }
  }

  // Exercise 12_13
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = 
    as.foldRight(unit(List.empty[B])){(a,fbs) => map2(f(a),fbs)(_ :: _) }
  def traverse[A,B](oa: Option[A])(f: A => F[B]): F[Option[B]] = oa match {
    case None => unit(None)
    case Some(a) => map(f(a))(Some(_))
  }
  case class Tree[+A](head: A, tail:List[Tree[A]])
  def traverse[A,B](ta: Tree[A])(f: A => F[B]): F[Tree[B]] = ta match {
    case Tree(h, t) => {
      val fh: F[B] = f(h)
      
      ???
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

object Listing_12_5 extends App {
  import java.text._
  import java.util.Date
  def validName(name: String): Validation[String, String] = {
    if (name != "") Success(name)
    else Failure("Name cannot be empty")
  }
  def validBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  }
  def validPhone(phoneNumber: String): Validation[String, String] = {
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")
  }
  case class WebForm(name: String, birthDate: String, phone: String)
  // Can't make it work!
//  def validWebForm(name: String, birthDate: String, phone: String): Validation[String, WebForm] = {
//    implicit val va: Applicative[({type f[x]=Validation[String,x]})#f] = Exercise_12_6.ValidationApplicative[String]

//    Exercise_12_3.map3(validName(name), validBirthdate(birthDate), validPhone(phone))(WebForm(_,_,_))
//  }
}
