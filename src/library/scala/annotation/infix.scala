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
package scala.annotation

/** A method annotation that suggests that the annotated method should
 *  be used as an infix operator. Infix operations with alphanumeric
 *  operator names require the operator to be annotated with `@infix`.
 */
final class infix extends StaticAnnotation

/** A method annotation that suggests that the annotated method may
 *  be used as a postfix operator. Note that the `postfixOps`
 *  language feature must be enabled.
 */
final class postfix extends StaticAnnotation
