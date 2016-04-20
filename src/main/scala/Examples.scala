package algebras

import scalaz.{ Coproduct, Coyoneda, Free, Id, Inject, State, ~> }

import scala.util.Random

import Activity._
import Rng._
import Types._

object Types {
  type RandomReader[A] = Random => A
}

object ActivityInterpreter extends (ActivityOp ~> Id.Id) {
  def apply[A](fc: ActivityOp[A]) =
    fc match {
      case WalkCat(name) =>
        println(s"Walking ${name}")
      case WashDishes(count) =>
        println(s"Washing ${count} dishes")
        1
    }
}

// Natural transformation to (Random => A)
object RngInterpreter extends (RngOp ~> RandomReader) {
  def apply[A](fa: RngOp[A]) =
    fa match {
      case Rng.NextBoolean       => _.nextBoolean
      case Rng.NextDouble        => _.nextDouble
      case Rng.NextFloat         => _.nextFloat
      case Rng.NextGaussian      => _.nextGaussian
      case Rng.NextInt           => _.nextInt
      case Rng.NextIntInRange(n) => _.nextInt(n)
      case Rng.NextLong          => _.nextLong
      case Rng.NextPrintableChar => _.nextPrintableChar
      case Rng.NextString(n)     => _.nextString(n)
      case Rng.SetSeed(n)        => _.setSeed(n)
    }
}

object Combined extends App {
  def prog[F[_]](implicit R: Inject[RngOp, F], A: Inject[ActivityOp, F]): Free.FreeC[F, Int] =
    for {
      name <- Rng.nextAsciiString(6)
      _ <- WalkCat(name)
      hours <- Rng.choose(1, 2, 3)
    } yield hours

  type Store[A] = State[Random, A]
  object RandomReaderToStore extends (RandomReader ~> Store) {
    def apply[A](f: RandomReader[A]): Store[A] = State { r => (r, f(r)) }
  }

  object IdToStore extends (Id.Id ~> Store) {
    def apply[A](fa: Id.Id[A]): Store[A] = {
      State { s => (s, fa) }
    }
  }

  type PRG[A]  = Coproduct[ActivityOp, RngOp, A]
  val program: Free.FreeC[PRG, Int] = prog[PRG]
  val interpreter: PRG ~> Store = (IdToStore compose ActivityInterpreter) or (RandomReaderToStore compose RngInterpreter)
  val coyoInterpreter = Coyoneda.liftTF(interpreter)
  val result0: Store[Int] = program.foldMap(coyoInterpreter)

  val result: Int = result0.eval(initial = new Random(2L))

  println(result)
}
