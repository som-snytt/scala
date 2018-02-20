
import annotation._

import M._

@implicitNotFound("Boring")
trait ActualConstantMessage

@implicitNotFound(implicitNotFound"Ain't no way for f-interpolator, has to be white-box")
trait SimpleConstantMessage

@implicitNotFound(implicitNotFound"The usual missing ${A} and ${B} but explicit interpolation")
trait T[A, B]

@implicitNotFound("The usual missing $A and $B with regular albeit implicit interpolation")
trait U[A, B]

@implicitNotFound("The usual missing $A, maybe ${A.typeParams} are wrong? and $B showing A,B are typeOf[A]")
trait V[A, B]

class C {
  def f = (implicitly[ActualConstantMessage], implicitly[SimpleConstantMessage],
           implicitly[T[List[Int], String]],  implicitly[U[collection.mutable.ListBuffer[collection.immutable.BitSet], Int]],
           implicitly[V[collection.mutable.ListBuffer[collection.immutable.BitSet], Int]],
          )
  def g = {
    import M._
    @implicitNotFound("Custom interpolator: $A, maybe ${A.typeParams} are wrong? and $B showing A,B are typeOf[A]")
    trait W[A, B]
    implicitly[U[collection.mutable.ListBuffer[collection.immutable.BitSet], Int]],
  }
}
