package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base

import org.jetbrains.plugins.scala.lang._
import completion.lookups.LookupElementManager
import lexer.ScalaTokenTypes
import psi.ScalaPsiElementImpl
import psi.api.base._
import psi.impl.ScalaPsiElementFactory
import resolve._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports._
import com.intellij.lang.ASTNode
import com.intellij.psi._
import com.intellij.psi.PsiElement
import com.intellij.util.IncorrectOperationException
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScInfixPattern, ScConstructorPattern}
import api.base.types.{ScParameterizedTypeElement, ScInfixTypeElement, ScSimpleTypeElement}
import impl.source.tree.LeafPsiElement
import processor.CompletionProcessor
import api.ScalaElementVisitor
import extensions.{toPsiNamedElementExt, toPsiClassExt}
import api.statements.{ScMacroDefinition, ScTypeAlias}
import api.expr.{ScSuperReference, ScThisReference}
import annotator.intention.ScalaImportClassFix
import util.PsiTreeUtil
import settings.ScalaProjectSettings
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember

/**
 * @author AlexanderPodkhalyuzin
 *         Date: 22.02.2008
 */

class ScStableCodeReferenceElementImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ResolvableStableCodeReferenceElement {
  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => super.accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  def getVariants: Array[Object] = {
    val isInImport: Boolean = ScalaPsiUtil.getParentOfType(this, classOf[ScImportStmt]) != null
    doResolve(this, new CompletionProcessor(getKinds(incomplete = true), this)).flatMap {
      case res: ScalaResolveResult =>
        import org.jetbrains.plugins.scala.lang.psi.types.Nothing
        val qualifier = res.fromType.getOrElse(Nothing)
        LookupElementManager.getLookupElement(res, isInImport = isInImport, qualifierType = qualifier, isInStableCodeReference = true)
      case r => Seq(r.getElement)
    }
  }

  def getResolveResultVariants: Array[ScalaResolveResult] = {
    doResolve(this, new CompletionProcessor(getKinds(incomplete = true), this)).flatMap {
      case res: ScalaResolveResult => Seq(res)
      case r => Seq.empty
    }
  }

  def getConstructor = {
    getContext match {
      case s: ScSimpleTypeElement => {
        s.getContext match {
          case p: ScParameterizedTypeElement => {
            p.getContext match {
              case constr: ScConstructor => Some(constr)
              case _ => None
            }
          }
          case constr: ScConstructor => Some(constr)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def isConstructorReference = !getConstructor.isEmpty

  override def toString: String = "CodeReferenceElement"

  def getKinds(incomplete: Boolean, completion: Boolean): Set[ResolveTargets.Value] = {
    import StdKinds._

    // The qualified identifer immediately following the `mqcro` keyword
    // may only refer to a method.
    def isInMacroDef = getContext match {
      case _: ScMacroDefinition =>
        prevSiblings.exists {
          case l: LeafPsiElement if l.getNode.getElementType == ScalaTokenTypes.kMACRO => true
          case _ => false
        }
      case _ => false
    }

    val result = getContext match {
      case _: ScStableCodeReferenceElement => stableQualRef
      case e: ScImportExpr => if (e.selectorSet != None
              //import Class._ is not allowed
              || qualifier == None || e.singleWildcard) stableQualRef
      else stableImportSelector
      case ste: ScSimpleTypeElement =>
        if (incomplete) noPackagesClassCompletion // todo use the settings to include packages
        else if (ste.getLastChild.isInstanceOf[PsiErrorElement]) stableQualRef

        else if (ste.singleton) stableQualRef
        else stableClass
      case _: ScTypeAlias => stableClass
      case _: ScConstructorPattern => objectOrValue
      case _: ScInfixPattern => objectOrValue
      case _: ScThisReference | _: ScSuperReference => stableClassOrObject
      case _: ScImportSelector => stableImportSelector
      case _: ScInfixTypeElement => stableClass
      case _ if isInMacroDef => methodsOnly
      case _ => stableQualRef
    }
    if (completion) result + ResolveTargets.PACKAGE + ResolveTargets.OBJECT + ResolveTargets.VAL else result
  }

  def nameId: PsiElement = findChildByType(ScalaTokenTypes.tIDENTIFIER)

  //  @throws(IncorrectOperationException)
  def bindToElement(element: PsiElement): PsiElement = {
    if (isReferenceTo(element)) this
    else {
      val aliasedRef: Option[ScReferenceElement] = ScalaPsiUtil.importAliasFor(element, this)
      if (aliasedRef.isDefined) {
        this.replace(aliasedRef.get)
      }
      else element match {
        case c: PsiClass => {
          val suitableKinds = getKinds(incomplete = false)
          if (!ResolveUtils.kindMatches(element, suitableKinds))
            throw new IncorrectOperationException("class does not match expected kind, problem place: " + {
              if (getContext != null)
                if (getContext.getContext != null)
                  if (getContext.getContext.getContext != null)
                    getContext.getContext.getContext.getText
                  else getContext.getContext.getText
                else getContext.getText
              else getText
            })
          if (nameId.getText != c.name) {
            val ref = ScalaPsiElementFactory.createReferenceFromText(c.name, getManager)
            nameId.getNode.getTreeParent.replaceChild(nameId.getNode, ref.nameId.getNode)
            return ref
          }
          val qname = c.qualifiedName
          val isPredefined = ScalaProjectSettings.getInstance(getProject).hasImportWithPrefix(qname)
          if (qualifier.isDefined && !isPredefined) {
            val ref = ScalaPsiElementFactory.createReferenceFromText(c.name, getContext, this)
            if (ref.isReferenceTo(element)) {
              val ref = ScalaPsiElementFactory.createReferenceFromText(c.name, getManager)
              return this.replace(ref)
            }
          }
          if (qname != null) {
            val selector: ScImportSelector = PsiTreeUtil.getParentOfType(this, classOf[ScImportSelector])
            if (selector != null) {
              val importExpr = PsiTreeUtil.getParentOfType(this, classOf[ScImportExpr])
              selector.deleteSelector() //we can't do anything here, so just simply delete it
              return importExpr.reference.get //todo: what we should return exactly?
              //              }
            } else getParent match {
              case importExpr: ScImportExpr if !importExpr.singleWildcard && !importExpr.selectorSet.isDefined =>
                val holder = PsiTreeUtil.getParentOfType(this, classOf[ScImportsHolder])
                holder.addImportForClass(c)
                importExpr.deleteExpr()
              //todo: so what to return? probable PIEAE after such code invocation
              case _ =>
                return safeBindToElement(qname, {
                  case (qual, true) => ScalaPsiElementFactory.createReferenceFromText(qual, getContext, this)
                  case (qual, false) => ScalaPsiElementFactory.createReferenceFromText(qual, getManager)
                }) {
                  ScalaImportClassFix.getImportHolder(ref = this, project = getProject).
                          addImportForClass(c, ref = this)
                  if (qualifier != None) {
                    //let's make our reference unqualified
                    val ref: ScStableCodeReferenceElement = ScalaPsiElementFactory.createReferenceFromText(c.name, getManager)
                    this.replace(ref).asInstanceOf[ScReferenceElement]
                  }
                  this
                }
            }
          }
          this
        }
        case binding: ScBindingPattern =>
          binding.nameContext match {
            case member: ScMember =>
              val containingClass = member.containingClass
              val refToClass = bindToElement(containingClass)
              val refToMember = ScalaPsiElementFactory.createReferenceFromText(refToClass.getText + "." + binding.name, getManager)
              this.replace(refToMember).asInstanceOf[ScReferenceElement]
          }
        case t: ScTypeAlias => this //todo: do something
        case _ => throw new IncorrectOperationException("Cannot bind to anything but class")
      }
    }
  }

  def getSameNameVariants: Array[ResolveResult] = doResolve(this, new CompletionProcessor(getKinds(incomplete = true), this, false, Some(refName)))

  override def delete() {
    getContext match {
      case sel: ScImportSelector => sel.deleteSelector()
      case expr: ScImportExpr => expr.deleteExpr()
      case _ => super.delete()
    }
  }
}