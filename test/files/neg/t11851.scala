
import scala.language.implicitConversions

object Main extends App {
  def function2_0(i: Int, j: Int): String = j.getClass.getSimpleName
  def function2_1(i: Int, j: Double): String = j.getClass.getSimpleName

  def function3_0(i: Int, j: Int, k: Int): String = j.getClass.getSimpleName
  def function3_1(i: Int, j: Double, k: Int): String = j.getClass.getSimpleName

  implicit def fromMy2[T](f: (Int, T) => String): (Int, Any, Int) => String = (i, a, _) => f(i, a.asInstanceOf[T])
  implicit def fromMy3[T](f: (Int, T, Int) => String): (Int, Any, Int) => String = (i, a, k) => f(i, a.asInstanceOf[T], k)

  def chooseAFunction(a: Int): (Int, Any, Int) => String =
    a match {
      case 20 => function2_0 
      case 21 => function2_1
      case 30 => function3_0
      case 31 => function3_1
    }

  val f30 = chooseAFunction(30)
  val f31 = chooseAFunction(31)
  println(f30(0, 1, 2))
  println(f31(0, 1.0, 2))
}
