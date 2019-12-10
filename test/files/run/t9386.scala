import scala.language.implicitConversions
class Foo
object Foo {
  implicit def int2Foo(a: => Int): Foo = new Foo //Never evaluate the argument
  def bar(foo: Foo) = ()
  def bar(foo: Boolean) = () //unrelated overload
}

object Test extends App {
  Foo.bar { println("barring"); 0 }
}
