/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.annotation.nowarn
import scala.reflect.internal.SymbolPairs

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields pairs of corresponding symbols visible in some base class,
 *  unless there's a parent class that already contains the same pairs.
 *  Most of the logic is in SymbolPairs, which contains generic
 *  pair-oriented traversal logic.
 */
abstract class OverridingPairs extends SymbolPairs {
  import global._

  @nowarn("""cat=deprecation&origin=scala\.tools\.nsc\.transform\.OverridingPairs\.Cursor""")
  final type PairsCursor = Cursor

  @nowarn("msg=shadowing a nested class of a parent is deprecated")
  @deprecated("use PairsCursor instead", since = "2.13.4")
  class Cursor(base: Symbol) extends super.Cursor(base) {
    /** Symbols to exclude: Here these are constructors and private/artifact symbols,
     *  including bridges. But it may be refined in subclasses.
     */
    override protected def exclude(sym: Symbol) = (
         sym.isPrivateLocal
      || sym.isArtifact
      || sym.isConstructor
      || (sym.isPrivate && sym.owner != base) // Privates aren't inherited. Needed for pos/t7475a.scala
    )

    /** Types always match. Term symbols match if their member types
     *  relative to `self` match.
     */
    override protected def matches(high: Symbol) = low.isType || (
         (low.owner != high.owner)     // don't try to form pairs from overloaded members
      && !bothJavaOwnedAndEitherIsField(low, high)
      && !high.isPrivate               // private or private[this] members never are overridden
      && !exclude(low)                 // this admits private, as one can't have a private member that matches a less-private member.
      && (lowMemberType matches (self memberType high))
    ) // TODO we don't call exclude(high), should we?

    override def skipOwnerPair(lowClass: Symbol, highClass: Symbol): Boolean =
      lowClass.isJavaDefined && highClass.isJavaDefined // javac is already checking this better than we could
  }

  private def bothJavaOwnedAndEitherIsField(low: Symbol, high: Symbol): Boolean = {
    low.owner.isJavaDefined && high.owner.isJavaDefined &&
      (low.isField || high.isField)
  }

  final class BridgesCursor(base: Symbol) extends PairsCursor(base) {
    // Varargs bridges may need generic bridges due to the non-repeated part of the signature of the involved methods.
    // The vararg bridge is generated during refchecks (probably to simplify override checking),
    // but then the resulting varargs "bridge" method may itself need an actual erasure bridge.
    // TODO: like javac, generate just one bridge method that wraps Seq <-> varargs and does erasure-induced casts
    override def exclude(sym: Symbol) = !sym.isMethod || super.exclude(sym)

    // Skip if the (non-trait) class in `parents` is a subclass of the owners of both low and high.
    // Correctness of bridge generation relies on visiting each such class only once.
    override def skipOwnerPair(lowClass: Symbol, highClass: Symbol): Boolean =
      nonTraitParent.isNonBottomSubClass(lowClass) && nonTraitParent.isNonBottomSubClass(highClass)
  }
}
