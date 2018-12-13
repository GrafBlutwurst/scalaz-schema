package scalaz

package schema

import scalaz.Scalaz._
import scalaz.schema.JsonSchema._
import FreeChoice._

object Json {
  type JSON = String

  val module = new SchemaModule {
    type Prim[A]       = JsonSchema.Prim[A]
    type ProductTermId = String
    type SumTermId     = String
  }

  trait ToJson[S[_]] {
    def serializer: S ~> (? => JSON)
  }

  implicit val toJson: ToJson[module.Prim] = new ToJson[module.Prim] {
    override def serializer: module.Prim ~> (? => JSON) = new (module.Prim ~> (? => JSON)) {
      override def apply[A](fa: module.Prim[A]): A => JSON = fa match {
        case JsonString => a => s""""$a""""
        case JsonBool   => a => if (a) "true" else "false"
        case JsonNull   => _ => "null"
        case JsonNumber => a => a.shows
      }
    }
  }

  /**
    * So the General idea is to unify the SchemaADT even one step further
    * In this prototype branch it has 2 entries SeqSchema and OptionalSchema they extends Schema[List[A]] and Schema[Option[A]] respectively.
    * My current Idea (whether it's a good one or even a feasible one is up to debate) is that we maybe can unify this to an EffectSchema[F[_], A] extends Schema[F[A]]
    * Or to lift Schema in general to be Schema[F[_], A] and have the other Schema types extend Schema[Id, A]
    * Now in a Module there would need to be a way to restrict the selection of Effects. Maybe as some sort of Coproduct. E.G. in JSON List :+: Option :+: Id
    * For each of these Effects there needs to be an approriate Fold and a Functor so we can construct things like the jsonSerializer. Currently I am not sure if and how this would generalize.
    **/

  type FAlgebra[F[_], A] = F[A] => A

  val listAlgebra:FAlgebra[List, JSON] = lst => lst.mkString("[", ",", "]")

  val optionAlgebra:FAlgebra[Option, JSON] = opt => opt.fold("null")(identity[JSON])


  def foldMap[F[_]: Functor, A, B](fa:F[A])(f:A => B)(fold:FAlgebra[F, B]):B = fold(fa.map(f))
 
  def jsonSerializer[A](
    schemaModule: SchemaModule
  )(
    schema: schemaModule.Schema[A]
  )(
    sumID: schemaModule.SumTermId => module.SumTermId,
    productID: schemaModule.ProductTermId => module.ProductTermId
  )(
    implicit
    prims: ToJson[schemaModule.Prim]
  ): A => JSON = {

    def nt[ID, IDM, G[_]](
      idf: ID => IDM
    )(pure: JSON => G[JSON]): ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) =
      new ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) {
        override def apply[X](term: schemaModule.Schema.Term[ID, A, X]): X => G[JSON] =
          (x: X) =>
            pure(s""""${idf(term.id)}": ${jsonSerializer(schemaModule)(term.base)(sumID, productID)(
              prims
            )(x)}""")
      }

    schema match {
      case schemaModule.Schema.OptionalSchema(base) =>
        a => foldMap(a)(jsonSerializer(schemaModule)(base)(sumID, productID)(prims))(optionAlgebra)
      case schemaModule.Schema.PrimSchema(prim) => a => prims.serializer(prim)(a)
      case schemaModule.Schema.IsoSchema(base, iso) =>
        a => jsonSerializer(schemaModule)(base)(sumID, productID)(prims)(iso(a))
      case rec: schemaModule.Schema.RecordSchema[_, ap] =>
        a => {
          val fun = FreeAp2
            .contravariantFold[
              schemaModule.Schema.Term[schemaModule.ProductTermId, A, ?],
              ? => IList[JSON],
              ap
            ](
              rec.fields
            )(
              nt[schemaModule.ProductTermId, module.ProductTermId, IList](
                productID
              )(
                Monad[IList].pure[JSON](_)
              )
            )
          fun(rec.f(a)).toList.mkString("{", ",", "}")
        }

      case listSchema:schemaModule.Schema.SeqSchema[a0] =>
        a =>foldMap[List,a0, JSON](a)(jsonSerializer(schemaModule)(listSchema.element)(sumID, productID)(prims))(listAlgebra)
      case union: schemaModule.Schema.Union[_, ae] =>
        a => {
          val fun =
            FreeChoice.contravariantFold[
              schemaModule.Schema.Term[schemaModule.SumTermId, A, ?],
              ? => JSON,
              ae
            ](
              union.choices
            )(
              nt[schemaModule.SumTermId, module.SumTermId, Id](
                sumID
              )(
                identity[JSON](_)
              )
            )
          "{" + fun(union.f(a)) + "}"
        }
    }
  }

}
