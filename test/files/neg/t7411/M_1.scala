
import language.experimental._

import reflect.macros.whitebox.Context

object M {
  implicit class `nice inf message`(private val sc: StringContext) extends AnyVal {
    def implicitNotFound(args: Any*): String = macro infNice
  }

  def infNice(c: Context)(args: c.Expr[Any]*) = {
    import c._, universe._
    Literal(Constant("A true constant"))
  }
}
