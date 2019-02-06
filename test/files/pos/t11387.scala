// scalac: -Xlint -Xfatal-warnings
object X {
  private class C
  private type A = C
  def apply(): String = (new A).toString
}
