package dingus {
  class Foo1() { protected def this(name: String) = this() }
  class Foo2 protected (name: String) { }
  object Ding {
    protected class Foo3(name: String) { }
  }
}

package hungus {
  import dingus._

  object P {
    class Bar1 extends Foo1("abc")
    class Bar2 extends Foo2("abc")
    class Bar3 extends Ding.Foo3("abc")

    val foo1 = new Foo1("abc")
    val foo2 = new Foo2("abc")
    val foo3 = new Ding.Foo3("abc")
  }
}

// attempt to follow the advice in the diagnostics
//
package traitorus {
  class Foo1() { protected def this(name: String) = this() }
  class Foo2 protected (name: String) { }
  class Ding {
    protected trait Foo3 { def name: String }
  }
}
package fixups {
  import traitorus._

  object P {
    class Bar1 extends Foo1("abc")
    class Bar2 extends Foo2("abc")

    /*
z.scala:15: error: class Foo3 in object Ding cannot be accessed as a member of object dingus.Ding from object P in package hungus
 Access to protected class Foo3 not permitted because
 enclosing object P in package hungus is not a subclass of
 object Ding in package dingus where target is defined
z.scala:15: error: class Foo3 in object Ding cannot be accessed in object dingus.Ding
   |class Foo3 in object Ding cannot be accessed as a member of dingus.Ding.type from class Bar3.
   | Access to protected class Foo3 not permitted because enclosing class Bar3 in object P
   | is not a subclass of object Ding in package dingus where target is defined
   */
    //class Bar3 extends Ding.Foo3("abc")
    class Bar3 extends Ding {
      class Inner3 extends Foo3 {
        def name = "bar3"
      }
    }

    val foo1 = new Foo1("abc")
    val foo2 = new Foo2("abc")
    val foo3 = new Ding.Foo3("abc")
  }
}

/*
-- Error: z.scala:15:28 --------------------------------------------------------
15 |    class Bar3 extends Ding.Foo3("abc")
   |                       ^^^^^^^^^
   |class Foo3 in object Ding cannot be accessed as a member of dingus.Ding.type from class Bar3.
   | Access to protected class Foo3 not permitted because enclosing class Bar3 in object P
   | is not a subclass of object Ding in package dingus where target is defined
-- Error: z.scala:17:24 --------------------------------------------------------
17 |    val foo1 = new Foo1("abc")
   |                        ^^^^^
   |                too many arguments for constructor Foo1: (): dingus.Foo1
-- Error: z.scala:18:19 --------------------------------------------------------
18 |    val foo2 = new Foo2("abc")
   |                   ^^^^
   |constructor Foo2 cannot be accessed as a member of dingus.Foo2 from module class P$.
   | Access to protected constructor Foo2 not permitted because enclosing object P in package hungus
   | is not a subclass of class Foo2 in package dingus where target is defined
-- Error: z.scala:19:24 --------------------------------------------------------
19 |    val foo3 = new Ding.Foo3("abc")
   |                   ^^^^^^^^^
   |class Foo3 in object Ding cannot be accessed as a member of dingus.Ding.type from module class P$.
   | Access to protected class Foo3 not permitted because enclosing object P in package hungus
   | is not a subclass of object Ding in package dingus where target is defined
-- Error: z.scala:19:29 --------------------------------------------------------
19 |    val foo3 = new Ding.Foo3("abc")
   |                             ^^^^^
   |                   too many arguments for constructor Object: (): Object
5 errors found

z.scala:15: error: class Foo3 in object Ding cannot be accessed in object dingus.Ding
 Access to protected class Foo3 not permitted because
 enclosing object P in package hungus is not a subclass of
 object Ding in package dingus where target is defined
    class Bar3 extends Ding.Foo3("abc")
                            ^
z.scala:15: error: no arguments allowed for nullary constructor Object: ()Object
    class Bar3 extends Ding.Foo3("abc")
                                 ^
z.scala:17: error: no arguments allowed for nullary constructor Foo1: ()dingus.Foo1
    val foo1 = new Foo1("abc")
                        ^
z.scala:18: error: constructor Foo2 in class Foo2 cannot be accessed in object P
 Access to protected constructor Foo2 not permitted because
 enclosing object P in package hungus is not a subclass of
 class Foo2 in package dingus where target is defined
    val foo2 = new Foo2("abc")
               ^
z.scala:19: error: class Foo3 in object Ding cannot be accessed in object dingus.Ding
 Access to protected class Foo3 not permitted because
 enclosing object P in package hungus is not a subclass of
 object Ding in package dingus where target is defined
    val foo3 = new Ding.Foo3("abc")
                        ^
5 errors found

z.scala:15: error: class Foo3 in object Ding cannot be accessed as a member of object dingus.Ding from object P in package hungus
 Access to protected class Foo3 not permitted because
 enclosing object P in package hungus is not a subclass of
 object Ding in package dingus where target is defined
    class Bar3 extends Ding.Foo3("abc")
                            ^
z.scala:15: error: no arguments allowed for nullary constructor Object: ()Object
    class Bar3 extends Ding.Foo3("abc")
                                 ^
z.scala:17: error: no arguments allowed for nullary constructor Foo1: ()dingus.Foo1
    val foo1 = new Foo1("abc")
                        ^
z.scala:18: error: constructor Foo2 in class Foo2 cannot be accessed in object P from object P in package hungus
 Access to protected constructor Foo2 not permitted because
 enclosing object P in package hungus is not a subclass of
 class Foo2 in package dingus where target is defined
    val foo2 = new Foo2("abc")
               ^
z.scala:19: error: class Foo3 in object Ding cannot be accessed as a member of object dingus.Ding from object P in package hungus
 Access to protected class Foo3 not permitted because
 enclosing object P in package hungus is not a subclass of
 object Ding in package dingus where target is defined
    val foo3 = new Ding.Foo3("abc")
                        ^
5 errors found
*/
