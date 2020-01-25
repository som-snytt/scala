package foreign

object Util {
  class C
  class D
}

object Main {
  import Util.C

  def f = new C

  import Util.D
  class D

  def g = new D
}
