import scalaz.{ Coproduct, Free, Inject, ~>, \/-, -\/ }

package object algebras {
  implicit def liftFCInj[A, P[_], F[_]](x: P[A])(implicit I: Inject[P, F]): Free.FreeC[F, A] = {
    Free.liftFC(I.inj(x))
  }

  implicit class RichFreeC[P[_], A](x: P[A]) {
    def liftI[F[_]](implicit I: Inject[P, F]): Free.FreeC[F, A] = liftFCInj(x)
  }

  // enhancing natural transformations with an or operator
  implicit class NaturalTransformationOrOps[F[_], H[_]](private val f2h: F ~> H) extends AnyVal {
    type Copro[F[_], G[_]] = { type f[x] = Coproduct[F, G, x] } // just for better readability

    // given F ~> H and G ~> H we derive Copro[F, G]#f ~> H
    def or[G[_]](g2h: G ~> H): Copro[F, G]#f ~> H =
      new (Copro[F, G]#f ~> H) {
        def apply[A](c: Coproduct[F, G, A]): H[A] = c.run match {
          case -\/(fa) => f2h(fa)
          case \/-(ga) => g2h(ga)
        }
      }
  }
}
