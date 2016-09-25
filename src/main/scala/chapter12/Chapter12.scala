package chapter12

import scala.language.higherKinds

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a:A): F[A]
  def map2[A,B,C](fa:F[A], fb:F[B])(f:(A,B) => C): F[C]

  override def map[A,B](fa:F[A])(f: A => B): F[B] = 
    map2(fa,unit(()))((a,_) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = 
    as.foldRight(unit(List.empty[B])){(a,fbs) => map2(f(a),fbs)(_ :: _) }
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
