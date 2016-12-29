package chapter13

import chapter11.Monad
import chapter12.Applicative

import scala.language.higherKinds
import scala.language.implicitConversions

object Chapter13 {

  object FirstListings {
    case class Player(name: String, score: Int)

    def contest(p1: Player, p2: Player): Unit = {
      if(p1.score > p2.score) 
        println(s"${p1.name} is the winner")
      else if(p1.score < p2.score)
        println(s"${p2.name} is the winner")
      else
        println("It's a draw")
    }

    def winner(p1: Player, p2: Player): Option[Player] = {
      if(p1.score > p2.score) Some(p1)
      else if(p1.score < p2.score) Some(p2)
      else None
    }

    def contest2(p1: Player, p2: Player): Unit = winner(p1,p2) match {
      case Some(p) => println(s"${p.name} is the winner")
      case _ => println("It's a draw")
    }

    def winnerMsg(op: Option[Player]): String = op match {
      case Some(p) => s"${p.name} is the winner"
      case _ => "It's a draw"
    }

    def contest3(p1: Player, p2: Player): Unit =
      println(winnerMsg(winner(p1, p2)))


    trait IO { self =>
      def run: Unit 
      def ++(io: IO): IO = new IO {
        override def run: Unit = 
          self.run
        io.run
      }
    }

    object IO {
      def empty: IO = new IO {
        def run: Unit = ()
      }
    }

    def PrintLine(msg: String): IO = new IO {
      override def run: Unit = println(msg)
    }

    def contestPure(p1: Player, p2: Player): IO =
      PrintLine(winnerMsg(winner(p1,p2)))

    def fahrenheitToCelsius(fah: Double): Double =
      (fah - 32) * 5.0/9.0

    def converterImpure: Unit = {
      println("Enter a temperature in degrees Fahrenheit:")
      val d = scala.io.StdIn.readLine.toDouble
      println(fahrenheitToCelsius(d))
    }
  }

  object BasicIO {
    sealed trait IO[A] { self =>
      def run: A
      def map[B](f: A => B): IO[B] = new IO[B] {
        override def run: B = f(self.run)
      }
      def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
        override def run: B = f(self.run).run
      }
    }

    /**In fact monads are always applicatives so that 'with Applicative' shouldn't be needed.
     * But we didn't define the Monad trait in Chapter 11 with 'extends Applicative' of
     * Chapter 12. Maybe it can be fixed also with implicit conversions but honestly I didn't
     * find the way (and I tried!)
     */
    implicit object IO extends Monad[IO] with Applicative[IO] {
      override def unit[A](a:A): IO[A] = new IO[A] {
        override def run: A = a
      }
      override def flatMap[A,B](ioa: IO[A])(f: A => IO[B]): IO[B] =
        ioa flatMap f
      override def map2[A,B,C](ioa: IO[A], iob: IO[B])(f: (A,B) => C): IO[C] = flatMap(ioa)(a => map(iob)(b => f(a,b))) 
      def apply[A](a: => A): IO[A] = new IO[A] {
        override def run: A = a
      }
    }

    def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

    def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }

    val echo: IO[Unit] = for {
      s <- ReadLine
      _ <- PrintLine(s)
    } yield ()

    val readInt: IO[Int] = ReadLine.map(_.toInt)

    val readTenTimes: IO[List[String]] = IO.replicateM(10, ReadLine)

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter a temperature in degrees fahrenheit:")
      c <- ReadLine.map( ((s:String) => s.toDouble) andThen FirstListings.fahrenheitToCelsius )
      _ <- PrintLine(s"Temperature in celsius is $c")
    } yield ()
  }

  sealed trait IO[A] { self =>
    def flatMap[B](f: A => IO[B]): IO[B] = 
      FlatMap(self, f)
    def map[B](f: A => B): IO[B] = 
      flatMap(a => Return(f(a)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A,B](ioa: IO[A], f: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(ioa, f) => ioa match {
      case Return(a) => run(f(a))
      case Suspend(resume) => run(f(resume()))
      case FlatMap(iob, g) => run(iob flatMap (b => g(b) flatMap f))
    }
  }

}
