package scalaz

package schema

//import Scalaz._
import recursion._

import monocle.Iso
import shapeless._

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

object Representation {
  type RSum[RA, A, RB, B]
  type RProd[RA, A, RB, B]
  type RIso[RA, A, B]
  type RSelf[A]
  type RSeq[R, A]
  type -*>[K, V]
  type -+>[K, V]
  type RRecord[RA, An, A]
  type RUnion[RA, An, A]
}

object Pathing {
  final case class PLeft()
  final case class PRight()
  final case class SLeft()
  final case class SRight()
  final case class Base()
}

import Representation._

import Pathing._

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_, _], R, A] {
  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, R, A]
}

trait SelfRef {}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[F[_, _], Prim[_], SumTermId, ProductTermId]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit, Unit] {
  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class SumF[F[_, _], RA, RB, A, B, Prim[_], SumTermId, ProductTermId](
  left: F[RA, A],
  right: F[RB, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RSum[RA, A, RB, B], A \/ B] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RSum[RA, A, RB, B], A \/ B] =
    SumF(
      nt(left),
      nt(right)
    )
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_, _], RA, RB, A, B, Prim[_], SumTermId, ProductTermId](
  left: F[RA, A],
  right: F[RB, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RProd[RA, A, RB, B], (A, B)] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RProd[RA, A, RB, B], (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_, _], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Prim[A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Prim[A], A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_, _], RA, I <: SumTermId, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, -+>[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, -+>[I, RA], A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class UnionF[F[_, _], RA, A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[RA, AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RUnion[RA, AE, A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RUnion[RA, AE, A], A] =
    new UnionF[G, RA, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class FieldF[F[_, _], RA, I <: ProductTermId, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, -*>[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, -*>[I, RA], A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class RecordF[F[_, _], RA, A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[RA, AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RRecord[RA, AP, A], A] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RRecord[RA, AP, A], A] =
    new RecordF[G, RA, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqF[F[_, _], RA, A, Prim[_], SumTermId, ProductTermId](element: F[RA, A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, RSeq[RA, A], List[A]] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RSeq[RA, A], List[A]] =
    SeqF(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchemaF[F[_, _], RA, A0, A, Prim[_], SumTermId, ProductTermId](
  base: F[RA, A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RIso[RA, A0, A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RIso[RA, A0, A], A] =
    IsoSchemaF(nt(base), iso)
}

final case class SelfReference[F[_, _], H[_, _], A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[_, A],
  private val nattrans: F ~~> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, RSelf[A], A] {

  lazy val unroll: H[_, A] = nattrans(ref())

  def hmap[G[_, _]](nt: H ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RSelf[A], A] =
    SelfReference[F, G, A, Prim, SumTermId, ProductTermId](ref, nt.compose(nattrans))
}

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_, _], G[_, _]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~~> G

  def compose[H[_, _]](nt: H ~~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_, _], G[_, _], H[_, _]](
  underlying: Interpreter[F, G],
  nt: H ~~> F
) extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_, _], _, _], F[_, _]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_, _], _, _], F[_, _], G[_, _]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

object SchemaF {

  implicit def schemaHFunctor[Prim[_], SumTermId, ProductTermId] =
    new HFunctor[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?]] {

      def hmap[F[_, _], G[_, _]](nt: F ~~> G) =
        new (SchemaF[Prim, SumTermId, ProductTermId, F, ?, ?] ~~> SchemaF[
          Prim,
          SumTermId,
          ProductTermId,
          G,
          ?,
          ?
        ]) {
          def apply[R, A](fa: SchemaF[Prim, SumTermId, ProductTermId, F, R, A]) = fa.hmap(nt)
        }
    }

  type FSchema[Prim[_], SumTermId, ProductTermId, A] =
    Fix[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], _, A]

  type FSchemaR[Prim[_], SumTermId, ProductTermId, Repr, A] =
    Fix[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], Repr, A]

  sealed private[schema] trait LabelledSum_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]

    def :+: [R2, B](
      l: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledSum_[B \/ A, RSum[R2, B, Repr, A], Prim, SumTermId, ProductTermId] =
      LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[A, Repr, I <: SumTermId, Prim[_], SumTermId, ProductTermId](
    id: I,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]
  ) extends LabelledSum_[A, -+>[I, Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      Fix(BranchF(id, schema))

  }

  final private[schema] case class LabelledSum2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledSum_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledSum_[A \/ B, RSum[R1, A, R2, B], Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new SumF(l.toSchema, r.toSchema))

  }

  sealed private[schema] trait LabelledProduct_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]

    def :*: [R2, B](
      l: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledProduct_[(B, A), RProd[R2, B, Repr, A], Prim, SumTermId, ProductTermId] =
      LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[A, Repr, I <: ProductTermId, Prim[
    _
  ], SumTermId, ProductTermId](
    id: I,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]
  ) extends LabelledProduct_[A, -*>[I, Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      Fix(
        FieldF(
          id,
          schema
        )
      )

  }

  final private[schema] case class LabelledProduct2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledProduct_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledProduct_[(A, B), RProd[R1, A, R2, B], Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new ProdF(l.toSchema, r.toSchema))

  }

}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_, _]] = Interpreter[Schema, F]

  type RSchema[F[_, _], Repr, A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, Repr, A]

  type BareSchema[A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], _, A]

  type Schema[Repr, A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], Repr, A]

  type Schema_[A] = BareSchema[A]

  type LabelledSum[Repr, A] = LabelledSum_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

  type LabelledProduct[Repr, A] = LabelledProduct_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

  type ROne[F[_, _]]               = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type RPrim[F[_, _], A]           = PrimSchemaF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_, _], RA, RB, A, B]  = SumF[F, RA, RB, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_, _], RA, RB, A, B] = ProdF[F, RA, RB, A, B, R.Prim, R.SumTermId, R.ProductTermId]

  type Branch[F[_, _], RA, I <: R.SumTermId, A] =
    BranchF[F, RA, I, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_, _], RA, AE, A] = UnionF[F, RA, A, AE, R.Prim, R.SumTermId, R.ProductTermId]

  type Field[F[_, _], RA, I <: R.ProductTermId, A] =
    FieldF[F, RA, I, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_, _], RA, An, A]   = RecordF[F, RA, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_, _], RA, A]     = SeqF[F, RA, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_, _], RA, A, B] = IsoSchemaF[F, RA, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Self[F[_, _], A]             = SelfReference[Any, F, A, R.Prim, R.SumTermId, R.ProductTermId]

  implicit class FixProdSyntax[RA, RB, A, B](schema: Schema[RProd[RA, A, RB, B], (A, B)]) {

    def left: Schema[RA, A] =
      schema.unFix.asInstanceOf[Prod[Schema, RA, RB, A, B]].left

    def right: Schema[RB, B] =
      schema.unFix.asInstanceOf[Prod[Schema, RA, RB, A, B]].right

  }

  implicit class FixSumSyntax[RA, RB, A, B](schema: Schema[RSum[RA, A, RB, B], A \/ B]) {

    def left: Schema[RA, A] =
      schema.unFix.asInstanceOf[Sum[Schema, RA, RB, A, B]].left

    def right: Schema[RB, B] =
      schema.unFix.asInstanceOf[Sum[Schema, RA, RB, A, B]].right

  }

  implicit class FixFieldSyntax[I <: R.ProductTermId, RA, A](schema: Schema[I -*> RA, A]) {
    def id: I               = schema.unFix.asInstanceOf[Field[Schema, RA, I, A]].id
    def base: Schema[RA, A] = schema.unFix.asInstanceOf[Field[Schema, RA, I, A]].schema
  }

  implicit class FixBranchSyntax[I <: R.SumTermId, RA, A](schema: Schema[I -+> RA, A]) {
    def id: I               = schema.unFix.asInstanceOf[Branch[Schema, RA, I, A]].id
    def base: Schema[RA, A] = schema.unFix.asInstanceOf[Branch[Schema, RA, I, A]].schema
  }

  implicit class FixRecordSyntax[RA, An, A](schema: Schema[RRecord[RA, An, A], A]) {
    def fields: Schema[RA, An] = schema.unFix.asInstanceOf[Record[Schema, RA, An, A]].fields
    def iso: Iso[An, A]        = schema.unFix.asInstanceOf[Record[Schema, RA, An, A]].iso
  }

  implicit class FixUnionSyntax[RA, Ae, A](schema: Schema[RUnion[RA, Ae, A], A]) {
    def choices: Schema[RA, Ae] = schema.unFix.asInstanceOf[Union[Schema, RA, Ae, A]].choices
    def iso: Iso[Ae, A]         = schema.unFix.asInstanceOf[Union[Schema, RA, Ae, A]].iso
  }

  implicit class FixIsoSyntax[RA, A0, A](schema: Schema[RIso[RA, A0, A], A]) {
    def base: Schema[RA, A0] = schema.unFix.asInstanceOf[IsoSchema[Schema, RA, A0, A]].base
    def iso: Iso[A0, A]      = schema.unFix.asInstanceOf[IsoSchema[Schema, RA, A0, A]].iso
  }

  implicit class FixSeqSyntax[RA, A](schema: Schema[RSeq[RA, A], List[A]]) {
    def element: Schema[RA, A] = schema.unFix.asInstanceOf[Sequence[Schema, RA, A]].element
  }

  implicit class FixPrimSyntax[A](schema: Schema[R.Prim[A], A]) {
    def prim: R.Prim[A] = schema.unFix.asInstanceOf[RPrim[Schema, A]].prim
  }

  sealed abstract class Derivation[G[_, _], Repr, A, ReprOut, Out](
    val schema: Schema[Repr, A]
  ) {
    def to: G[ReprOut, Out]
  }

  sealed class DerivationTo[G[_, _]] {

    def const[XR, X, ReprOut, Out](
      schema: Schema[XR, X]
    )(
      value: G[ReprOut, Out]
    ): Derivation[G, XR, X, ReprOut, Out] =
      new Derivation[G, XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = value
      }

    //SumCase
    def sum[
      XR,
      YR,
      X,
      Y,
      XROut,
      YROut,
      XOut,
      YOut,
      ReprOut,
      Out
    ](
      schema: Schema[RSum[XR, X, YR, Y], X \/ Y]
    )(
      lDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut],
      rDerive: Schema[YR, Y] => Derivation[G, YR, Y, YROut, YOut]
    )(
      derive: (G[XROut, XOut], G[YROut, YOut]) => G[ReprOut, Out]
    ): Derivation[G, RSum[XR, X, YR, Y], X \/ Y, ReprOut, Out] =
      new Derivation[G, RSum[XR, X, YR, Y], X \/ Y, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val lDerived = lDerive(schema.left)
          val rDerived = rDerive(schema.right)
          derive(lDerived.to, rDerived.to)
        }
      }

    //ProductCase
    def prod[
      XR,
      YR,
      X,
      Y,
      XROut,
      YROut,
      XOut,
      YOut,
      ReprOut,
      Out
    ](
      schema: Schema[RProd[XR, X, YR, Y], (X, Y)]
    )(
      lDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut],
      rDerive: Schema[YR, Y] => Derivation[G, YR, Y, YROut, YOut]
    )(
      derive: (G[XROut, XOut], G[YROut, YOut]) => G[ReprOut, Out]
    ): Derivation[G, RProd[XR, X, YR, Y], (X, Y), ReprOut, Out] =
      new Derivation[G, RProd[XR, X, YR, Y], (X, Y), ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val lDerived = lDerive(schema.left)
          val rDerived = rDerive(schema.right)
          derive(lDerived.to, rDerived.to)
        }
      }

    //field case
    def field[
      I <: R.ProductTermId,
      XR,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[I -*> XR, X]
    )(
      baseDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut]
    )(
      derive: (R.ProductTermId, G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, I -*> XR, X, ReprOut, Out] =
      new Derivation[G, I -*> XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.id, baseDerived.to)
        }
      }

    //branch case
    def branch[
      I <: R.SumTermId,
      XR,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[I -+> XR, X]
    )(
      baseDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut]
    )(
      derive: (R.SumTermId, G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, I -+> XR, X, ReprOut, Out] =
      new Derivation[G, I -+> XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.id, baseDerived.to)
        }
      }

    //record case
    def rec[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RRecord[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RRecord[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RRecord[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.fields)
          derive(schema.iso, baseDerived.to)
        }
      }

    //union case
    def union[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RUnion[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RUnion[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RUnion[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.choices)
          derive(schema.iso, baseDerived.to)
        }
      }

    //iso case
    def iso[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RIso[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RIso[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RIso[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.iso, baseDerived.to)
        }
      }

    //prim case
    def prim[
      X,
      ReprOut,
      Out
    ](
      schema: Schema[R.Prim[X], X]
    )(
      derive: R.Prim[X] => G[ReprOut, Out]
    ): Derivation[G, R.Prim[X], X, ReprOut, Out] =
      new Derivation[G, R.Prim[X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] =
          derive(schema.prim)
      }

  }

  object DerivationTo {
    def apply[G[_, _]] = new DerivationTo[G] {}
  }

  object Interpreter {

    def cata[S[_[_, _], _, _], F[_, _]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
      new CataInterpreter[S, F](alg)

    def hylo[S[_[_, _], _, _], F[_, _], G[_, _]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
      implicit ev: HFunctor[S]
    ) = new HyloInterpreter(coalg, alg)

  }

  trait AtPath[Repr, A, P <: HList] {
    type ROuter[_]
    type RO
    type O

    def select(schema: Schema[Repr, A]): Schema[RO, O]

    def applyAt[RD](schema: Schema[Repr, A])(
      f: Schema[RO, O] => Schema[RD, O]
    ): Schema[ROuter[RD], A]
  }

  trait LowPrioAtPath0 {

    implicit def atSumRight[
      Repr,
      A,
      RB,
      B,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RSum[RB, B, Repr, A],
      B \/ A,
      SRight :: P,
      RT,
      AT,
      λ[X => RSum[RB, B, ROuter0[X], A]]
    ] =
      new AtPath[RSum[RB, B, Repr, A], B \/ A, SRight :: P] {
        override type ROuter[X] = RSum[RB, B, ROuter0[X], A]
        override type RO        = RT
        override type O         = AT

        override def select(schema: Schema[RSum[RB, B, Repr, A], B \/ A]): Schema[RT, AT] =
          rest.select(schema.right)

        override def applyAt[RD](
          schema: Schema[RSum[RB, B, Repr, A], B \/ A]
        )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], B \/ A] =
          schema.left :+: rest.applyAt(schema.right)(f)
      }

    implicit def atProdRight[
      Repr,
      A,
      RB,
      B,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RProd[RB, B, Repr, A],
      (B, A),
      PRight :: P,
      RT,
      AT,
      λ[X => RProd[RB, B, ROuter0[X], A]]
    ] =
      new AtPath[RProd[RB, B, Repr, A], (B, A), PRight :: P] {
        override type ROuter[X] = RProd[RB, B, ROuter0[X], A]
        override type RO        = RT
        override type O         = AT

        override def select(schema: Schema[RProd[RB, B, Repr, A], (B, A)]): Schema[RT, AT] =
          rest.select(schema.right)

        override def applyAt[RD](
          schema: Schema[RProd[RB, B, Repr, A], (B, A)]
        )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], (B, A)] =
          schema.left :*: rest.applyAt(schema.right)(f)
      }
  }

  trait LowPrioAtPath extends LowPrioAtPath0 {
    implicit def atField[
      N <: R.ProductTermId,
      Repr,
      A,
      T <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, T, RT, AT, ROuter0]
    ): AtPath.Aux[
      N -*> Repr,
      A,
      N :: T,
      RT,
      AT,
      λ[X => N -*> ROuter0[X]]
    ] = new AtPath[N -*> Repr, A, N :: T] {
      override type ROuter[X] = N -*> ROuter0[X]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[N -*> Repr, A]): Schema[RT, AT] =
        rest.select(schema.base)

      override def applyAt[RD](
        schema: Schema[N -*> Repr, A]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A] =
        LabelledProduct1(schema.id, rest.applyAt(schema.base)(f)).toSchema

    }

    implicit def atBranch[
      N <: R.SumTermId,
      Repr,
      A,
      T <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, T, RT, AT, ROuter0]
    ): AtPath.Aux[
      N -+> Repr,
      A,
      N :: T,
      RT,
      AT,
      λ[X => N -+> ROuter0[X]]
    ] = new AtPath[N -+> Repr, A, N :: T] {
      override type ROuter[X] = N -+> ROuter0[X]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[N -+> Repr, A]): Schema[RT, AT] =
        rest.select(schema.base)

      override def applyAt[RD](
        schema: Schema[N -+> Repr, A]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A] =
        LabelledSum1(schema.id, rest.applyAt(schema.base)(f)).toSchema
    }

    implicit def atRecord[
      Repr,
      An,
      A,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, An, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RRecord[Repr, An, A],
      A,
      Base :: P,
      RT,
      AT,
      λ[X => RRecord[ROuter0[X], An, A]]
    ] = new AtPath[RRecord[Repr, An, A], A, Base :: P] {
      override type ROuter[X] = RRecord[ROuter0[X], An, A]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[RRecord[Repr, An, A], A]): Schema[RT, AT] =
        rest.select(schema.fields)

      override def applyAt[RD](
        schema: Schema[RRecord[Repr, An, A], A]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A] =
        unsafeRecord(rest.applyAt(schema.fields)(f), schema.iso)
    }

    implicit def atUnion[
      Repr,
      Ae,
      A,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, Ae, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RUnion[Repr, Ae, A],
      A,
      Base :: P,
      RT,
      AT,
      λ[X => RUnion[ROuter0[X], Ae, A]]
    ] = new AtPath[RUnion[Repr, Ae, A], A, Base :: P] {
      override type ROuter[X] = RUnion[ROuter0[X], Ae, A]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[RUnion[Repr, Ae, A], A]): Schema[RT, AT] =
        rest.select(schema.choices)

      override def applyAt[RD](
        schema: Schema[RUnion[Repr, Ae, A], A]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A] =
        unsafeUnion(rest.applyAt(schema.choices)(f), schema.iso)
    }

    implicit def atIso[
      Repr,
      A0,
      A,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A0, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RIso[Repr, A0, A],
      A,
      Base :: P,
      RT,
      AT,
      λ[X => RIso[ROuter0[X], A0, A]]
    ] = new AtPath[RIso[Repr, A0, A], A, Base :: P] {
      override type ROuter[X] = RIso[ROuter0[X], A0, A]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[RIso[Repr, A0, A], A]): Schema[RT, AT] =
        rest.select(schema.base)

      override def applyAt[RD](
        schema: Schema[RIso[Repr, A0, A], A]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A] =
        iso(rest.applyAt(schema.base)(f), schema.iso)
    }

    implicit def atSeq[
      Repr,
      A,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RSeq[Repr, A],
      List[A],
      Base :: P,
      RT,
      AT,
      λ[X => RSeq[ROuter0[X], A]]
    ] = new AtPath[RSeq[Repr, A], List[A], Base :: P] {
      override type ROuter[X] = RSeq[ROuter0[X], A]
      override type RO        = RT
      override type O         = AT

      override def select(schema: Schema[RSeq[Repr, A], List[A]]): Schema[RT, AT] =
        rest.select(schema.element)

      override def applyAt[RD](
        schema: Schema[RSeq[Repr, A], List[A]]
      )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], List[A]] =
        seq(rest.applyAt(schema.element)(f))
    }

    implicit def atSumLeft[
      Repr,
      A,
      RB,
      B,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RSum[Repr, A, RB, B],
      A \/ B,
      SLeft :: P,
      RT,
      AT,
      λ[X => RSum[ROuter0[X], A, RB, B]]
    ] =
      new AtPath[RSum[Repr, A, RB, B], A \/ B, SLeft :: P] {
        override type ROuter[X] = RSum[ROuter0[X], A, RB, B]
        override type RO        = RT
        override type O         = AT

        override def select(schema: Schema[RSum[Repr, A, RB, B], A \/ B]): Schema[RT, AT] =
          rest.select(schema.left)

        override def applyAt[RD](
          schema: Schema[RSum[Repr, A, RB, B], A \/ B]
        )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], A \/ B] =
          rest.applyAt(schema.left)(f) :+: schema.right
      }

    implicit def atProdLeft[
      Repr,
      A,
      RB,
      B,
      P <: HList,
      RT,
      AT,
      ROuter0[_]
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT, ROuter0]
    ): AtPath.Aux[
      RProd[Repr, A, RB, B],
      (A, B),
      PLeft :: P,
      RT,
      AT,
      λ[X => RProd[ROuter0[X], A, RB, B]]
    ] =
      new AtPath[RProd[Repr, A, RB, B], (A, B), PLeft :: P] {
        override type ROuter[X] = RProd[ROuter0[X], A, RB, B]
        override type RO        = RT
        override type O         = AT

        override def select(schema: Schema[RProd[Repr, A, RB, B], (A, B)]): Schema[RT, AT] =
          rest.select(schema.left)

        override def applyAt[RD](
          schema: Schema[RProd[Repr, A, RB, B], (A, B)]
        )(f: Schema[RT, AT] => Schema[RD, AT]): Schema[ROuter[RD], (A, B)] =
          rest.applyAt(schema.left)(f) :*: schema.right

      }

    /* implicit def atPrim[
      A
    ]: AtPath.Aux[
      R.Prim[A],
      A,
      HNil,
      R.Prim[A],
      A,
      Id
    ] =
      new AtPath[R.Prim[A], A, HNil] {
        override type ROuter[X] = Id[X]
        override type RO        = R.Prim[A]
        override type O         = A

        override def select(schema: Schema[R.Prim[A], A]): Schema[R.Prim[A], A] = schema

        override def applyAt[RD](schema: Schema[R.Prim[A], A])(
          f: Schema[R.Prim[A], A] => Schema[RD, A]
        ): Schema[RD, A] = f(schema)
      }*/
  }

  object AtPath extends LowPrioAtPath {

    type Aux[Repr, A, P <: HList, RT, AT, ROuter0[_]] = AtPath[Repr, A, P] {
      type ROuter[X] = ROuter0[X]
      type RO        = RT
      type O         = AT
    }

    def apply[Repr, A, P <: HList, RT, AT, ROuter0[_]](schema: Schema[Repr, A], path: P)(
      implicit atPath: Aux[Repr, A, P, RT, AT, ROuter0]
    ): Aux[Repr, A, P, RT, AT, ROuter0] = {
      identity(schema)
      identity(path)
      atPath
    }

    implicit def atRoot[Repr, A, P <: HNil]: Aux[Repr, A, P, Repr, A, Id] =
      new AtPath[Repr, A, P] {
        override type ROuter[X] = X
        override type RO        = Repr
        override type O         = A

        override def select(schema: Schema[Repr, A]): Schema[Repr, A] = schema

        override def applyAt[RD](schema: Schema[Repr, A])(
          f: Schema[Repr, A] => Schema[RD, A]
        ): Schema[RD, A] = f(schema)
      }

  }

  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[Repr, A](schema: Schema[Repr, A]) {

    def :*: [R2, B](left: Schema[R2, B]): Schema[RProd[R2, B, Repr, A], (B, A)] =
      Fix(new ProdF(left, schema))

    def :+: [R2, B](left: Schema[R2, B]): Schema[RSum[R2, B, Repr, A], B \/ A] =
      Fix(new SumF(left, schema))

    def -*>: [I <: R.ProductTermId](
      id: I
    ): LabelledProduct[I -*> Repr, A] =
      LabelledProduct1(id, schema)

    def -+>: [I <: R.SumTermId](id: I): LabelledSum[I -+> Repr, A] =
      LabelledSum1(id, schema)

    def to[F[_, _]](implicit interpreter: RInterpreter[F]): F[_, A] = interpreter.interpret(schema)

    def imap[B](_iso: Iso[A, B]): Schema[RIso[Repr, A, B], B] = Fix(IsoSchemaF(schema, _iso))
    /*
    schema.unFix match {
      case i: IsoSchema[Schema, Repr, a0, A] =>
        Fix(
          IsoSchemaF[Schema, Repr, a0, B, R.Prim, R.SumTermId, R.ProductTermId](
            i.base,
            i.iso.composeIso(_iso)
          )
        )
      case _ => Fix(IsoSchemaF(schema, _iso))
    }
   */
  }

  final def unit: Schema[Unit, Unit] =
    Fix(
      One()
    )

  final def prim[A](prim: R.Prim[A]): Schema[R.Prim[A], A] =
    Fix(
      PrimSchemaF(prim)
    )

  final def unsafeUnion[Repr, A, AE](
    choices: Schema[Repr, AE],
    iso: Iso[AE, A]
  ): Schema[RUnion[Repr, AE, A], A] =
    Fix(
      new UnionF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices, iso) {}
    )

  final def union[Repr, A, AE](
    choices: LabelledSum[Repr, AE],
    iso: Iso[AE, A]
  ): Schema[RUnion[Repr, AE, A], A] =
    Fix(
      new UnionF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices.toSchema, iso) {}
    )

  final def optional[Repr, A](
    aSchema: Schema[Repr, A]
  ): Schema[RIso[RSum[Repr, A, Unit, Unit], A \/ Unit, Option[A]], Option[A]] =
    iso(
      Fix(SumF(aSchema, unit)),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def unsafeRecord[Repr, A, An](
    terms: Schema[Repr, An],
    isoA: Iso[An, A]
  ): Schema[RRecord[Repr, An, A], A] =
    Fix(
      new RecordF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms, isoA) {}
    )

  final def record[Repr, A, An](
    terms: LabelledProduct[Repr, An],
    isoA: Iso[An, A]
  ): Schema[RRecord[Repr, An, A], A] =
    Fix(
      new RecordF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms.toSchema, isoA) {}
    )

  final def seq[Repr, A](element: Schema[Repr, A]): Schema[RSeq[Repr, A], List[A]] =
    Fix(SeqF(element))

  final def iso[Repr, A0, A](
    base: Schema[Repr, A0],
    iso: Iso[A0, A]
  ): Schema[RIso[Repr, A0, A], A] =
    Fix(IsoSchemaF(base, iso))

  final def self[A](root: => Schema[_, A]): Schema[RSelf[A], A] =
    Fix(
      SelfReference(() => root, new (Schema ~~> Schema) { def apply[R0, X](a: Schema[R0, X]) = a })
    )

}
