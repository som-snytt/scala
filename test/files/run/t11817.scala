
// working minimization
//
object ClassFormatErrorBug {
  type Id[A] = A
}

abstract class PF[A, B]{
  def x[A1 <: A](x1: A1): Any
}
class PF1 extends PF[ClassFormatErrorBug.Id[java.lang.String], Any] {
  def x[A1 <: ClassFormatErrorBug.Id[java.lang.String]](x1: A1): Any = null
}

// broken minimization
//
object SI_7288 {
  type Id[A] = A
  def f[A](pf: PartialFunction[A, Unit]): Unit = ()
}

object Test extends App {
  println(new PF1)

  import SI_7288._
  println(f[Id[Int]] { case _ => })
}
