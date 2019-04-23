// scalac: -Xlint:deprecation -Werror

trait T {
  def then = 42     // deprecation of reserved word
}

trait U {
  def `then` = 42   // OK
}

trait V {
  @deprecated("Think of a different name", "1.0")
  def then = 42     // normally, deprecations not issued within a deprecated element
}
