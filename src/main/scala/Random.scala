package algebras

import scalaz.{ Coyoneda, Free, Inject, Monad }

import scala.util.Random

// An algebra of primitive operations in the context of a random number generator
sealed trait RngOp[A]

// Example usage of free monad over free functor
object Rng {
  type Program[A] = Free.FreeC[RngOp, A]
  type CoYoRngOp[A] = Coyoneda[RngOp, A]

  // Free monad over the free functor of RngOp. The instance is not inferrable.
  implicit val MonadRng: Monad[Program] =
    Free.freeMonad[CoYoRngOp]

  case object NextBoolean              extends RngOp[Boolean]
  case object NextDouble               extends RngOp[Double]
  case object NextFloat                extends RngOp[Float]
  case object NextGaussian             extends RngOp[Double]
  case object NextInt                  extends RngOp[Int]
  case class  NextIntInRange(max: Int) extends RngOp[Int]
  case object NextLong                 extends RngOp[Long]
  case object NextPrintableChar        extends RngOp[Char]
  case class  NextString(length: Int)  extends RngOp[String]
  case class  SetSeed(seed: Long)      extends RngOp[Unit]

  // You can of course derive new operations from the primitives
  def nextNonNegativeInt[F[_]](implicit R: Inject[RngOp, F]): Free.FreeC[F, Int] = {
    for { num <- NextInt } yield num.abs
  }

  def choose[F[_], A](h: A, tl: A*)(implicit R: Inject[RngOp, F]): Free.FreeC[F, A] = {
    val xs = (h +: tl)
    for { idx <- NextIntInRange(xs.length) } yield xs(idx)
  }
}
