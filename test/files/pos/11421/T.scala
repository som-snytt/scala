
package post.fix

import scala.language.postfixOps

trait T {
  //import scala.language.postfixOps      // compiles here
  //def f[A](xs: List[A]): Int = xs length    // sample feature lookup has same behavior
  def g = postfixOps       // resolves correctly
  def f = implicitly[languageFeature.postfixOps]  // fails to compile
}

object Test extends App with T {
  println(f)
  println(g)
}

/* Was:

scalac -Vimplicits -d /tmp -classpath /tmp test/files/pos/11421/T.scala
test/files/pos/11421/T.scala:10: postfixOps is not a valid implicit value for languageFeature.postfixOps because:
candidate implicit method postfixOps in package fix is shadowed by lazy value postfixOps in object language
  def f = implicitly[languageFeature.postfixOps]  // fails to compile
                    ^
test/files/pos/11421/T.scala:10: error: could not find implicit value for parameter e: languageFeature.postfixOps
  def f = implicitly[languageFeature.postfixOps]  // fails to compile
                    ^
one error found
 */
