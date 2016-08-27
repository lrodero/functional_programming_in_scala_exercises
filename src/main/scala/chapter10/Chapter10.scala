package chapter10

import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

trait Foldable[F[_]] {
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object Chapter10 {

  val stringMonoid = new Monoid[String] {
    override def op(s1: String, s2: String) = s1 + s2
    override def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(l1: List[A], l2: List[A]) = l1 ::: l2
    override def zero = Nil
  }

}

object Exercise_10_2 { 
  // Monoid for Option
  def optMonoid[A: Monoid] = new Monoid[Option[A]] {
    override def op(o1: Option[A], o2: Option[A]) = (o1, o2) match {
      case (Some(a1), Some(a2)) => Some(implicitly[Monoid[A]].op(a1,a2))
      case (None, None) => None
      case (_, None) => o1
      case (None, _) => o2
    }
    override def zero = None
  }
}

object Exercise_10_5 {
  // Implement foldMap
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as match {
    case Nil => m.zero
    case a :: tail => m.op(f(a), foldMap(tail,m)(f))
  }
}

object Exercise_10_7 {
  // Implement foldMap for IndexedSeq in a parallel fashion
  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case IndexedSeq() => m.zero
    case IndexedSeq(a) => f(a)
    case _ => {
      val half = as.size/2
      m.op(foldMapV(as.slice(0,half),m)(f), foldMapV(as.slice(half,as.size),m)(f))
    }
  }
}

object Exercise_10_13 {
  // Implement Foldable on a tree
  sealed abstract class Tree[+A]
  case class Leaf[A](a:A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  def foldableTree = new Foldable[Tree] {
    def foldLeft[A,B](t: Tree[A])(z: B)(f: (B,A) => B): B = t match {
      case Leaf(a) => f(z,a)
      case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    def foldRight[A,B](t: Tree[A])(z: B)(f: (A,B) => B): B = t match {
      case Leaf(a) => f(a,z)
      case Branch(r,l) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    def foldMap[A,B](t: Tree[A])(f: A => B)(m: Monoid[B]): B = t match {
      case Leaf(a) => f(a)
      case Branch(tl,tr) => m.op(foldMap(tl)(f)(m), foldMap(tr)(f)(m))
    } 
  }
}

object Exercise_10_15 {
  // Foldable to list
  def toList[F[_]: Foldable, A](fa: F[A]): List[A] =
    implicitly[Foldable[F]].foldLeft(fa)(List[A]())((l:List[A],a:A) => a :: l)
}
