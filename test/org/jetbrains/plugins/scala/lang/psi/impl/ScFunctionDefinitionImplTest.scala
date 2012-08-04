package org.jetbrains.plugins.scala
package lang.psi.impl

import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.intellij.lang.annotations.Language
import org.junit.Assert._
import org.jetbrains.plugins.scala.lang.psi.api.statements.RecursionType.{OrdinaryRecursion, TailRecursion, NoRecursion}
import lang.psi.api.statements.{ScFunctionDefinition, RecursionType}
import collection.immutable.IndexedSeq

/**
 * Pavel Fatin
 */
class ScFunctionDefinitionImplTest extends SimpleTestCase {
  def testNoRecursion() {
    assertRecursionTypeIs("def f(n: Int) = n", NoRecursion)
  }

  def testLinearRecursion() {
    assertRecursionTypeIs("def f(n: Int): Int = 1 + f(n)", OrdinaryRecursion)
  }

  def testTailRecursion() {
    assertRecursionTypeIs("def f(n: Int): Int = f(n + 1)", TailRecursion)
  }

  def testTailRecursionWithCurring() {
    assertRecursionTypeIs("def f(n: Int)(x:Int)(y:Int): Int = f(n + 1)(x)(y)", TailRecursion)
  }

  def testReturn() {
    assertRecursionTypeIs("def f(n: Int): Int = return f(n + 1)", TailRecursion)
  }

  def testAndAnd() {
    assertRecursionTypeIs("def f(n: Int): Boolean = n > 0 && f(n)", TailRecursion)
  }

  def testOrOr() {
    assertRecursionTypeIs("def f(n: Int): Boolean = n > 0 || f(n)", TailRecursion)
  }

  def testOtherInfixOperator() {
    assertRecursionTypeIs("def f(n: Int): Boolean = n > 0 ** f(n)", OrdinaryRecursion)
  }

  private def assertRecursionTypeIs(@Language("Scala") code: String, expectation: RecursionType) {
    val function = code.parse[ScFunctionDefinition]
    assertEquals(expectation, function.recursionType)
  }

  def testMyRefactorings() {
    import lang.psi.impl.statements.ScFunctionDefinitionImpl
    val code = toCode("""
            def f(n: Int)(x:Int)(y:Int): Int = {
              if (n == 34) 34
              else if (n == 45) 45
              else f(n + 1)(x)(y)
            }
                      """)

    val definition = code.parse[ScFunctionDefinition].asInstanceOf[ScFunctionDefinitionImpl]

    val n = 10000

    def printMedianAndValue[A](name: String)(body: => A) {
      val results: (Long, A) = calculateMedianTime(n) {
        body
      }
      println(name + ": " + results._1)
      println(name + ": " + results._2)
      println()
    }

    printMedianAndValue("recursionTypeNew") {
      definition.recursionType
    }
    printMedianAndValue("bodyNew"){
      definition.body
    }
     printMedianAndValue("hasAssignNew"){
      definition.hasAssign
    }
  }

  def calculateMedianTime[A](n: Int)(body: => A) = {
    val seq: IndexedSeq[(Long, A)] = for {i <- 1 to n} yield withTimer(body)
    (seq.map(_._1).sum / n, seq.head._2)
  }

  def withTimer[A](body: => A): (Long, A) = {
    val start: Long = System.nanoTime()
    val res = body
    (System.nanoTime() - start, res)
  }
}