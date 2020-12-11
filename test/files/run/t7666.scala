
class X(n: => Int) {
  def x = n
}
// error: super constructor cannot be passed a self reference unless parameter is declared by-name
object Y extends X(Y.z) {
  def z = 1
}

class Foo(f: Foo) {
  println(f)
}

object Redirector {
  def voop = Voop
}

// arg is null
object Voop extends Foo(Redirector.voop)

object Test extends App {
  println {
    Y.x
  }
  println {
    Voop
  }
}
