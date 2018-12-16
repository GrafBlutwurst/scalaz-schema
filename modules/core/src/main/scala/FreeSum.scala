package scalaz

package schema

sealed trait FreeSum[F[_], A]
final case class SumConcat[F[_], A, B](lhs: FreeSum[F, A], rhs: FreeSum[F, B])
    extends FreeSum[F, A \/ B]
final case class SumSnoc[F[_], A, B](heads: FreeSum[F, A], fb: F[B]) extends FreeSum[F, A \/ B]
final case class SumCons[F[_], A, B](fa: F[A], tail: FreeSum[F, B])  extends FreeSum[F, A \/ B]
final case class SumPure[F[_], A](fa: F[A])                          extends FreeSum[F, A]
//Not sure if we can make this a Monoid
//final case class SumIdentity[F[_]]() extends FreeSum[F, Nothing]

object FreeSum {

  def covariantFold[F[_], G[_]: Alt, A](choice: FreeSum[F, A])(nt: F ~> G): G[A] = choice match {
    case x: SumConcat[F, at, bt] =>
      Alt[G].either2(covariantFold(x.lhs)(nt), covariantFold(x.rhs)(nt))

    //in this case A has to be Either[at, bt]
    case x: SumCons[F, at, bt] => Alt[G].either2(nt(x.fa), covariantFold(x.tail)(nt))
    case x: SumSnoc[F, at, bt] => Alt[G].either2(covariantFold(x.heads)(nt), nt(x.fb))

    case x: SumPure[F, at] => nt(x.fa)
  }

  def contravariantFold[F[_], G[_]: Decidable, A](choice: FreeSum[F, A])(nt: F ~> G): G[A] =
    choice match {
      case x: SumConcat[F, at, bt] =>
        Decidable[G].choose2(contravariantFold(x.lhs)(nt), contravariantFold(x.rhs)(nt))(identity)

      //in this case A has to be Either[at, bt]
      case x: SumCons[F, at, bt] =>
        Decidable[G].choose2(nt(x.fa), contravariantFold(x.tail)(nt))(identity)
      case x: SumSnoc[F, at, bt] =>
        Decidable[G].choose2(contravariantFold(x.heads)(nt), nt(x.fb))(identity)

      case x: SumPure[F, at] => nt(x.fa)
    }

  implicit class FreeSumOps[F[_], B](choices: FreeSum[F, B]) {
    def <-: [A](fa: F[A]): FreeSum[F, A \/ B]             = SumCons(fa, choices)
    def :-> [A](fa: F[A]): FreeSum[F, B \/ A]             = SumSnoc(choices, fa)
    def ::: [A](aSums: FreeSum[F, A]): FreeSum[F, A \/ B] = SumConcat(aSums, choices)
  }

  implicit def functionDecidable[X: Monoid]: Decidable[? => X] = new Decidable[? => X] {
    override def choose2[Z, A1, A2](a1: => (A1 => X), a2: => (A2 => X))(
      f: Z => A1 \/ A2
    ): (Z => X) = z => f(z).fold(a1, a2)

    override def conquer[A]: A => X = _ => Monoid[X].zero

    override def divide[A, B, C](fa: A => X, fb: B => X)(f: C => (A, B)): C => X = c => {
      val tpl = f(c)
      Monoid[X].append(fa(tpl._1), fb(tpl._2))
    }
  }

}
