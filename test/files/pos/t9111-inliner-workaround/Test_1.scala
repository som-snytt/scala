// scalac: -opt:l:inline -opt-inline-from:** -opt-warnings:no-inline-missing-bytecode -Werror
//
object Test extends App {
  //println(new A_1.Inner())

  // Accessing foo or Deeper triggers the error of scala/bug#9111.
  // However, when not referring to those definitions, compilation should
  // succeed, also if the inliner is enabled.

  val i = new A_1.Inner()
  println(i.foo(null))
  new i.Deeper()
}
