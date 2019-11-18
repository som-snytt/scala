
package object example {
  def Self[A](a: A): Self[A] = ???
  def Self[A](unit: Unit): Self[A] = ???
}

package example {

  trait Self[A] {
    def self: A
  }

  object App extends App {
    println(scala.reflect.runtime.universe.typeOf[Self[Any]].toString)
  }

}
