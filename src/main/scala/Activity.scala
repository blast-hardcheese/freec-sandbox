package algebras

import scalaz.{ Coyoneda, Free, Monad }

sealed trait ActivityOp[T]

object Activity {
  type Program[T] = Free.FreeC[ActivityOp, T]
  type CoYoActivity[A] = Coyoneda[ActivityOp, A]

  implicit val MonadActivity: Monad[Program] =
    Free.freeMonad[CoYoActivity]

  case class WalkCat(catName: String) extends ActivityOp[Unit]
  case class WashDishes(dishCount: Int) extends ActivityOp[Int]
}
