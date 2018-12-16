package scalaz

package schema

object SumProductIso {
  sealed trait SumTypeForProduct[A] {
    type Out
  }

  object SumTypeForProduct {

    type Aux[A, Out0] = SumTypeForProduct[A] {
      type Out = Out0
    }
  }

  implicit def sumTypeForProductPure[F[_], A](
    products: ProductPure[F, A]
  ): SumTypeForProduct.Aux[A, A] = new SumTypeForProduct[A] {
    override type Out = A
  }

  implicit def sumTypeForProductCons[F[_], A, B](products: ProductCons[F, A, B])(
    implicit rhs: SumTypeForProduct[B]
  ): SumTypeForProduct.Aux[(A, B), A \/ rhs.Out] = new SumTypeForProduct[(A, B)] {
    override type Out = A \/ rhs.Out
  }

  implicit def sumTypeForProductSnoc[F[_], A, B](products: ProductSnoc[F, A, B])(
    implicit lhs: SumTypeForProduct[A]
  ): SumTypeForProduct.Aux[(B, A), lhs.Out \/ A] = new SumTypeForProduct[(B, A)] {
    override type Out = lhs.Out \/ A
  }

  implicit def sumTypeForProductConcat[F[_], A, B](
    products: ProductConcat[F, A, B]
  )(
    implicit lhs: SumTypeForProduct[A],
    rhs: SumTypeForProduct[B]
  ): SumTypeForProduct.Aux[(A, B), lhs.Out \/ rhs.Out] = new SumTypeForProduct[(A, B)] {
    override type Out = lhs.Out \/ rhs.Out
  }

  def productToSum[F[_], A, X](
    products: FreeProduct[F, A]
  )(implicit ev: SumTypeForProduct.Aux[A, X]): FreeSum[F, X] = products match {
    case x: ProductConcat[F, at, bt] => SumConcat(productToSum(x.lhs), productToSum(x.rhs))
    case x: ProductCons[F, at, bt]   => SumCons(x.fa, productToSum(x.tails))
    case x: ProductSnoc[F, at, bt]   => SumSnoc(productToSum(x.heads), x.fb)
    case x: ProductPure[F, at]       => SumPure(x.fa)
  }

}
