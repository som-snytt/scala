// scalac: -Vimplicits
//
// Scala 2 requires that implicits in lexical scope are accessible without a prefix.
// Therefore, ordinary name binding determines eligible symbols for phase 1 implicit search.
// Scala 3 will ignore name binding, but also prefer a symbol at greater context depth.
//
// This bug shows that "ours" is incorrectly taken as ambiguous because of the import clause.
// In Scala 2, phase 2 implicit search continues with implicit scope and finds the value in T.
// In Scala 3, implicit search will terminate when an ambiguity is found.

trait T[A]
object T {
  implicit val `default for string`: T[String] = new T[String] {}
}

object Penumbra {
  implicit val ours: T[String] = new T[String] {}
}

object Locally {
  import Penumbra._
  def verify(x: AnyRef) = assert(x eq ours)
  implicit val ours: T[String] = new T[String] {}
  def test(): Unit = {
    val tested = implicitly[T[String]]   // fails
    verify(tested)
    verify(implicitly[T[String]])        // fails
  }
}

object Memberly {
  import Penumbra._
  def verify(x: AnyRef) = assert(x eq ours)
  implicit val ours: T[String] = new T[String] {}
  val tested = implicitly[T[String]] // correct
  verify(tested)                     // OK
  verify(implicitly[T[String]])      // fails
  def test(): Unit = ()
}

object Test extends App {
  Locally.test()
  Memberly.test()
}
