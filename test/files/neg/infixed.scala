// scalac: -Xfatal-warnings -Xlint

import annotation.{infix, postfix}
import language.postfixOps

object X {
  def f(i: Int) = i+1
  @infix def g(i: Int) = i+2

  def m = 17
  @postfix def n = 42
}

trait T {
  def t1 = X f 42
  def t2 = X g 42

  def t3 = X m
  def t4 = X n
}
