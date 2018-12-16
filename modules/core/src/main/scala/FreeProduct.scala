package scalaz

package schema

sealed trait FreeProduct[F[_], A]
final case class ProductConcat[F[_], A, B](lhs: FreeProduct[F, A], rhs: FreeProduct[F, B])
    extends FreeProduct[F, (A, B)]
final case class ProductSnoc[F[_], A, B](heads: FreeProduct[F, A], fb: F[B])
    extends FreeProduct[F, (A, B)]
final case class ProductCons[F[_], A, B](fa: F[A], tail: FreeProduct[F, B])
    extends FreeProduct[F, (A, B)]
final case class ProductPure[F[_], A](fa: F[A]) extends FreeProduct[F, A]

object FreeProduct {

  def covariantFold[F[_], G[_]: Applicative, A](fa: FreeProduct[F, A])(nt: F ~> G): G[A] =
    fa match {
      case x: ProductConcat[F, at, bt] =>
        Applicative[G].tuple2(covariantFold(x.lhs)(nt), covariantFold(x.rhs)(nt))
      case x: ProductSnoc[F, at, bt] => Applicative[G].tuple2(covariantFold(x.heads)(nt), nt(x.fb))
      case x: ProductCons[F, at, bt] => Applicative[G].tuple2(nt(x.fa), covariantFold(x.tail)(nt))
      case x: ProductPure[F, at]     => nt(x.fa)

    }

  def contravariantFold[F[_], G[_]: Divisible, A](fa: FreeProduct[F, A])(nt: F ~> G): G[A] =
    fa match {
      case x: ProductConcat[F, at, bt] =>
        Divisible[G].divide2(contravariantFold(x.lhs)(nt), contravariantFold(x.rhs)(nt))(identity)
      case x: ProductSnoc[F, at, bt] =>
        Divisible[G].divide2(contravariantFold(x.heads)(nt), nt(x.fb))(identity)
      case x: ProductCons[F, at, bt] =>
        Divisible[G].divide2(nt(x.fa), contravariantFold(x.tail)(nt))(identity)
      case x: ProductPure[F, at] => nt(x.fa)
    }

  implicit class FreeProductOps[F[_], B](products: FreeProduct[F, B]) {
    def <-: [A](fa: F[A]): FreeProduct[F, (A, B)] = ProductCons(fa, products)
    def :-> [A](fa: F[A]): FreeProduct[F, (B, A)] = ProductSnoc(products, fa)

    def ::: [A](aProducts: FreeProduct[F, A]): FreeProduct[F, (A, B)] =
      ProductConcat(aProducts, products)
  }

  implicit def functionDivisible[X: Monoid]: Divisible[? => X] = new Divisible[? => X] {
    def conquer[A]: A => X = _ => Monoid[X].zero

    def contramap[A, B](r: A => X)(f: B => A): B => X = f.andThen(r)

    def divide[A, B, C](fa: A => X, fb: B => X)(f: C => (A, B)): C => X = c => {
      val tpl = f(c)
      Monoid[X].append(fa(tpl._1), fb(tpl._2))
    }

  }

}
