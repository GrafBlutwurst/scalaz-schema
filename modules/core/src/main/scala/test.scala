import scalaz._, schema._, Json._
import monocle._

import shapeless._
import shapeless.syntax.singleton._

object module extends JsonModule[JsonSchema.type] {

  val R = JsonSchema
  import R._
  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean, boss: Person) extends Role
  final case class Admin(rights: List[String])         extends Role

  def user(pers: BareSchema[Person]) = record(
    "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "boss".narrow -*>: self(pers),
    Iso[(Boolean, Person), User]((User.apply _).tupled)(u => (u.active, u.boss))
  )

  val admin = record(
    "rights".narrow -*>: seq(prim(JsonSchema.JsonString)),
    Iso[List[String], Admin](Admin.apply)(_.rights)
  )

  def role(pers: Schema_[Person]) = union(
    "user".narrow -+>: user(pers) :+:
      "admin".narrow -+>: admin,
    Iso[User \/ Admin, Role] {
      case -\/(u) => u
      case \/-(a) => a
    } {
      case u @ User(_, _) => -\/(u)
      case a @ Admin(_)   => \/-(a)
    }
  )

  def person(pers: Schema_[Person]) = record(
    "name".narrow -*>: prim(JsonSchema.JsonString) :*:
      "role".narrow -*>: optional(
      role(pers)
    ),
    Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
  )

  implicit val primToEncoderNT = new (R.Prim ~> EncoderA) {

    def apply[A](fa: R.Prim[A]): EncoderA[A] = { a =>
      fa match {
        case JsonNumber => a.toString
        case JsonBool   => a.toString
        case JsonString => s""""$a""""
        case JsonNull   => "null"
      }
    }
  }

  val p = person(person(null))

  type Name    = Witness.`"name"`.T
  type Foo     = Witness.`"foo"`.T
  type UserW   = Witness.`"user"`.T
  type ActiveW = Witness.`"active"`.T
  type BossW   = Witness.`"boss"`.T
  type AdminW  = Witness.`"admin"`.T
  type RightsW = Witness.`"rights"`.T
  type RoleW   = Witness.`"role"`.T

  import schema.Pathing._

  //import AtPath._

  //val path = "role".narrow :: "user".narrow :: "active".narrow :: HNil

  val path = Base() :: PLeft() :: "name".narrow :: HNil

//  val lookup = AtPath(p, path)

  val schemaTest = prim(JsonSchema.JsonString) :*:
    prim(
      JsonSchema.JsonBool
    )



  import schema.Representation._
  val nav = AtPath[
    RProd[
      JsonPrim[String], String, JsonPrim[Boolean], Boolean
    ], 
    (String,Boolean), 
    PLeft :: HNil, 
    JsonPrim[String], 
    String, 
    λ[X => RProd[X, String, JsonPrim[Boolean], Boolean]]
  ](schemaTest, PLeft() ::  HNil)

  val burns = Person("Montgommery Burns", Some(Admin(List("billionaire", "evil mastermind"))))
  val homer = Person("Homer Simpson", Some(User(true, burns)))

}
