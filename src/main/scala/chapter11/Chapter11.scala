package chapter11

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A,B)]): (F[A],F[B]) = 
    (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  def bind[A](mma: M[M[A]]): M[A] = flatMap(mma){(ma:M[A]) => ma}

  override def map[A,B](ma: M[A])(f: A => B): M[B] = 
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] = 
    flatMap(ma){a => 
      map(mb)(b => f(a,b))
    }
}

object Exercise_11_3 {
  def sequence[F[_]: Monad,A](la: List[F[A]]): F[List[A]] = {
    val monad = implicitly[Monad[F]]
    la match {
      case Nil => monad.unit(List.empty[A])
      case fa :: tail => monad.flatMap(sequence(tail)){ la =>
        monad.map(fa)(a => a :: la)
      }
    }
  }
  def traverse[F[_]: Monad,A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)
}

object Exercise_11_4 {
  def replicateM[F[_]: Monad,A](n: Int, ma: F[A]): F[List[A]] =
    Exercise_11_3.sequence(List.fill[F[A]](n)(ma))
}

object Exercise_11_7 {
  def compose[F[_]: Monad,A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    (a:A) => { 
      val monad = implicitly[Monad[F]]
      monad.flatMap(f(a))(g)
    }
}

object Exercise_11_12 {
  def join[F[_]: Monad,A](mma: F[F[A]]): F[A] = {
    val monad = implicitly[Monad[F]]
    monad.flatMap(mma){ma => monad.flatMap(ma)(a => monad.unit(a))}
  }
}

object Exercise_11_13 {
  def flatMap2[F[_]: Monad,A,B](ma: F[A])(f: A => F[B]): F[B] = {
    val monad = implicitly[Monad[F]]
    Exercise_11_12.join(monad.map(ma)(f))
  }
}

case class Id[A](a: A) {
  def map[B](f: A => B): Id[B] = Monad.idMonad.map(this)(f)
  def flatMap[B](f: A => Id[B]): Id[B] = Monad.idMonad.flatMap(this)(f)
}

case class State[S,A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] = State{ (s:S) =>
    val (a, newS) = run(s)
    (f(a),newS)
  }
  def flatMap[B](f: A => State[S,B]): State[S,B] = State { (s:S) =>
    val (a, newS) = run(s)
    val sb: State[S,B] = f(a)
    sb.run(newS)
  }
}

object Monad {

  val idMonad = new Monad[Id] {
    def unit[A](a: A) = Id(a)
    def flatMap[A,B](ia: Id[A])(f: A => Id[B]): Id[B] = ia match {
      case Id(a) => f(a)
    }
  }

  object IdMonad extends Monad[Id] {
    def unit[A](a:A) = Id(a)
    def flatMap[A,B](ia: Id[A])(f: A => Id[B]): Id[B] = ia match {
      case Id(a) => f(a)
    }
  }
  
  type IntState[A] = State[Int,A]

  val intStateMonad = new Monad[IntState] {
    def unit[A](a:A): IntState[A] = State{s => (a,s)}
    def flatMap[A,B](sa: IntState[A])(f: A => IntState[B]): IntState[B] = sa flatMap f
  }

  trait StateMonad[S] extends Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a,s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = 
      st flatMap f
  }

}


