package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel

import com.intellij.util.ArrayFactory
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import psi.api.toplevel.typedef._
import psi.api.toplevel.packaging._
import org.jetbrains.plugins.scala.lang.parser._
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import com.intellij.psi._
import stubs.StubElement
import impl.source.PsiFileImpl
import collection.mutable.ArrayBuffer
import psi.impl.toplevel.packaging.ScPackagingImpl

/**
 * Trait that implements logic by some type definitions aggregation
 *
 * @author ilyas
 */

trait ScToplevelElement extends ScalaPsiElement {
  def typeDefinitions(): Seq[ScTypeDefinition] = {
    val buff = new ArrayBuffer[ScTypeDefinition]
    for (clazz <- immediateTypeDefinitions) buff += clazz
    for (pack <- packagings) buff ++= pack.typeDefinitions
    buff.toSeq
  }

  def immediateTypeDefinitions: Seq[ScTypeDefinition] = {
    val stub: StubElement[_ <: PsiElement] = this match {
      case file: PsiFileImpl => file.getStub
      case st: ScPackagingImpl => st.getStub
      case _ => null
    }
    if (stub != null) {
      stub.getChildrenByType[ScTypeDefinition](TokenSets.TMPL_DEF_BIT_SET, ScalaPsiUtil.arrayFactory[ScTypeDefinition])
    } else collection.immutable.Seq(findChildrenByClassScala(classOf[ScTypeDefinition]).toSeq : _*)
  }

  def packagings: Seq[ScPackaging] = {
    val stub: StubElement[_ <: PsiElement] = this match {
      case file: PsiFileImpl => file.getStub
      case st: ScPackagingImpl => st.getStub
      case _ => null
    }
    if (stub != null) {
      stub.getChildrenByType[ScPackaging](ScalaElementTypes.PACKAGING, ScalaPsiUtil.arrayFactory[ScPackaging])
    } else {
      val buffer = new ArrayBuffer[ScPackaging]
      var curr = getFirstChild
      while (curr != null) {
        if (curr.isInstanceOf[ScPackaging]) buffer += curr.asInstanceOf[ScPackaging]
        curr = curr.getNextSibling
      }
      buffer.toSeq
      //collection.immutable.Seq(findChildrenByClassScala(classOf[ScPackaging]).toSeq : _*)
    }
  }
}