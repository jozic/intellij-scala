package org.jetbrains.plugins.scala
package lang
package psi
package types

import com.intellij.psi._
import result.TypingContext
import org.apache.commons.lang.StringEscapeUtils
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocumentationProvider
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTypeDefinition, ScObject}
import api.statements._
import api.base.patterns.{ScReferencePattern, ScBindingPattern}
import extensions.{toPsiNamedElementExt, toPsiClassExt}
import params.{ScParameter, ScTypeParam}
import refactoring.util.{ScalaNamesUtil, ScTypeUtil}
import collection.mutable.ArrayBuffer

trait ScTypePresentation {
  def presentableText(t: ScType) = typeText(t, _.name, {
      case obj: ScObject if Set("scala.Predef", "scala").contains(obj.qualifiedName) => ""
      case pack: PsiPackage => ""
      case e => e.name + "."
    }
  )

  def urlText(t: ScType) = {
    def nameFun(e: PsiNamedElement, withPoint: Boolean): String = {
      e match {
        case obj: ScObject if withPoint && obj.qualifiedName == "scala.Predef" => ""
        case e: PsiClass => "<a href=\"psi_element://" + e.qualifiedName + "\"><code>" +
                StringEscapeUtils.escapeHtml(e.name) +
                "</code></a>" + (if (withPoint) "." else "")
        case pack: PsiPackage if withPoint => ""
        case _ => StringEscapeUtils.escapeHtml(e.name) + "."
      }
    }
    typeText(t, nameFun(_, withPoint = false), nameFun(_, withPoint = true))
  }

  //todo: resolve cases when java type have keywords as name (type -> `type`)
  def canonicalText(t: ScType) = {
    def removeKeywords(s: String): String = {
      s.split('.').map(s => if (ScalaNamesUtil.isKeyword(s)) "`" + s + "`" else s).mkString(".")
    }
    def nameFun(e: PsiNamedElement, withPoint: Boolean): String = {
      removeKeywords(e match {
        case c: PsiClass => {
          val qname = c.qualifiedName
          if (qname != null && qname != c.name /* exlude default package*/ ) "_root_." + qname else c.name
        }
        case p: PsiPackage => "_root_." + p.getQualifiedName
        case _ =>
          ScalaPsiUtil.nameContext(e) match {
            case m: ScMember =>
              m.containingClass match {
                case o: ScObject => nameFun(o, withPoint = true) + e.name
                case _ => e.name
              }
            case _ => e.name
          }
      }) + (if (withPoint) "." else "")
    }
    typeText(t, nameFun(_, withPoint = false), nameFun(_, withPoint = true))
  }

  private def typeText(t: ScType, nameFun: PsiNamedElement => String, nameWithPointFun: PsiNamedElement => String): String = {
    val buffer = new StringBuilder
    def appendSeq(ts: Seq[ScType], sep: String, stable: Boolean, checkWildcard: Boolean = false) {
      var first = true
      for (t <- ts) {
        if (!first) buffer.append(sep)
        first = false
        inner(t, stable, checkWildcard)
      }
    }

    def appendTypeTail(stable: Boolean) {
      if (!stable) buffer.append(".type")
    }

    def inner(t: ScType, stable: Boolean, checkWildcard: Boolean = false) {
      t match {
        case ScAbstractType(tpt, lower, upper) =>
          buffer.append(ScTypePresentation.ABSTRACT_TYPE_PREFIX).append(tpt.name.capitalize)
        case StdType(name, _) => buffer.append(name)
        case ScFunctionType(ret, params) => {
          buffer.append("(")
          appendSeq(params, ", ", stable = false)
          buffer.append(") => ")
          inner(ret, stable = false)
        }
        case ScThisType(clazz: ScTypeDefinition) =>
          buffer.append(clazz.name + ".").append("this")
          appendTypeTail(stable)
        case ScThisType(clazz) =>
          buffer.append("this")
          appendTypeTail(stable)
        case ScTupleType(comps) => buffer.append("("); appendSeq(comps, ", ", stable = false); buffer.append(")")
        case ScDesignatorType(e@(_: ScObject | _: ScReferencePattern | _: ScParameter)) =>
          buffer.append(nameFun(e))
          appendTypeTail(stable)
        case ScDesignatorType(e) => buffer.append(nameFun(e))
        case proj@ScProjectionType(p, el, _) => {
          //todo:
          val e = proj.actualElement
          val refName = e.name
          def appendPointType() {
            e match {
              case obj: ScObject => appendTypeTail(stable)
              case v: ScBindingPattern => appendTypeTail(stable)
              case _ =>
            }
          }
          p match {
            case ScDesignatorType(pack: PsiPackage) => buffer.append(nameWithPointFun(pack)).append(refName)
            case ScDesignatorType(obj: ScObject) => {
              buffer.append(nameWithPointFun(obj)).append(refName)
              appendPointType()
            }
            case ScDesignatorType(v: ScBindingPattern) => {
              buffer.append(nameWithPointFun(v)).append(refName)
              appendPointType()
            }
            case ScThisType(obj: ScObject) => {
              buffer.append(nameWithPointFun(obj)).append(refName)
              appendPointType()
            }
            case p: ScProjectionType if p.actualElement.isInstanceOf[ScObject] =>
              inner(p, stable = true); buffer.append(".").append(refName)
            case ScDesignatorType(clazz: PsiClass) if clazz.getLanguage != ScalaFileType.SCALA_LANGUAGE &&
              e.isInstanceOf[PsiModifierListOwner] &&
              e.asInstanceOf[PsiModifierListOwner].getModifierList.hasModifierProperty("static") => {
              buffer.append(nameWithPointFun(clazz)).append(refName)
            }
            case _: ScCompoundType | _: ScExistentialType =>
              buffer.append("(")
              inner(p, stable = false)
              buffer.append(")")
              buffer.append("#").append(refName)
            case _ =>
              inner(p, stable = false); buffer.append("#").append(refName)
          }
        }
        case p: ScParameterizedType if p.getTupleType != None => inner(p.getTupleType.get, stable)
        case p: ScParameterizedType if p.getFunctionType != None => inner(p.getFunctionType.get, stable)
        case ScParameterizedType(des, typeArgs) => {
          inner(des, stable = false)
          buffer.append("[")
          appendSeq(typeArgs, ", ", stable = false, checkWildcard = true)
          buffer.append("]")
        }
        case j@JavaArrayType(arg) => buffer.append("Array["); inner(arg, stable = false); buffer.append("]")
        case ScSkolemizedType(name, _, _, _) => buffer.append(name)
        case ScTypeParameterType(name, _, _, _, _) => buffer.append(name)
        case ScUndefinedType(tpt: ScTypeParameterType) => buffer.append("NotInfered").append(tpt.name)
        case ScTypeVariable(name) => buffer.append(name)
        case c@ScCompoundType(comps, decls, typeDecls, s) => {
          def typeText0(tp: ScType) = typeText(s.subst(tp), nameFun, nameWithPointFun)
          buffer.append(comps.map(typeText(_, nameFun, nameWithPointFun)).mkString(" with "))
          if (decls.length + typeDecls.length > 0) {
            if (!comps.isEmpty) buffer.append(" ")
            buffer.append("{")
            buffer.append(decls.map {
              decl =>
                decl match {
                  case fun: ScFunction => {
                    val buffer = new StringBuilder("")
                    buffer.append("def ").append(fun.name)
                    buffer.append(fun.paramClauses.clauses.map(_.parameters.map(param =>
                      ScalaDocumentationProvider.parseParameter(param, typeText0)).
                      mkString("(", ", ", ")")).mkString(""))
                    for (tp <- fun.returnType) {
                      val scType: ScType = s.subst(tp)
                      val text = if (!c.equiv(scType)) typeText(scType, nameFun, nameWithPointFun) else "this.type"
                      buffer.append(": ").append(text)
                    }
                    buffer.toString()
                  }
                  case v: ScValue => {
                    v.declaredElements.map(td => {
                      val scType: ScType = td.getType(TypingContext.empty).getOrAny
                      val text = if (!c.equiv(scType)) typeText0(scType) else "this.type"
                      "val " + td.name + ": " + text
                    }).mkString("; ")
                  }
                  case v: ScVariable => {
                    v.declaredElements.map(td => {
                      val scType: ScType = td.getType(TypingContext.empty).getOrAny
                      val text = if (!c.equiv(scType)) typeText0(scType) else "this.type"
                      "var " + td.name + ": " + text
                    }).mkString("; ")
                  }
                  case _ => ""
                }
            }.mkString("; "))

            if (!typeDecls.isEmpty) {
              if (!decls.isEmpty) buffer.append("; ")
              buffer.append(typeDecls.map {
                (ta: ScTypeAlias) =>
                  val paramsText = if (ta.typeParameters.length > 0)
                    ta.typeParameters.map(param => typeParamText(param, s, nameFun, nameWithPointFun)).mkString("[", ", ", "]")
                  else ""

                  val decl = "type " + ta.name + paramsText

                  val defnText = ta match {
                    case tad: ScTypeAliasDefinition =>
                      var x = ""
                      tad.aliasedType.foreach {
                        case psi.types.Nothing => ""
                        case tpe => x += (" = " + typeText0(tpe))
                      }
                      x
                    case _ =>
                      var x = ""
                      ta.lowerBound foreach {
                        case psi.types.Nothing =>
                        case tp: ScType => x += (" >: " + typeText0(tp))
                      }
                      ta.upperBound foreach {
                        case psi.types.Any =>
                        case tp: ScType => x += (" <: " + typeText0(tp))
                      }
                      x
                  }
                  decl + defnText
              }.mkString("; "))
            }

            buffer.append("}")
          }
        }
        case ScExistentialType(q, wilds) if checkWildcard && wilds.length == 1 =>
          q match {
            case ScTypeVariable(name) if name == wilds(0).name =>
              val wildcard = wilds(0)
              buffer.append("_")
              if (wildcard.lowerBound != Nothing) {
                buffer.append(" >: ")
                inner(wildcard.lowerBound, stable = false)
              }
              if (wildcard.upperBound != Any) {
                buffer.append(" <: ")
                inner(wildcard.upperBound, stable = false)
              }
            case ScDesignatorType(a: ScTypeAlias) if a.isExistentialTypeAlias && a.name == wilds(0).name =>
              val wildcard = wilds(0)
              buffer.append("_")
              if (wildcard.lowerBound != Nothing) {
                buffer.append(" >: ")
                inner(wildcard.lowerBound, stable = false)
              }
              if (wildcard.upperBound != Any) {
                buffer.append(" <: ")
                inner(wildcard.upperBound, stable = false)
              }
            case _ => inner(t, stable, checkWildcard = false)
          }
        case ex@ScExistentialType(ScParameterizedType(des, typeArgs), wilds) =>
          val wildcardsMap = ex.wildcardsMap()
          val replacingArgs = new ArrayBuffer[(ScType, ScExistentialArgument)]()
          val left = wilds.filter {
            case arg: ScExistentialArgument =>
              val seq = wildcardsMap.getOrElse(arg, Seq.empty)
              if (seq.length == 1 && typeArgs.exists(_ eq seq(0))) {
                replacingArgs += ((seq(0), arg))
                false
              } else true
          }
          if (left.nonEmpty) buffer.append("(")
          inner(des, stable = false)
          buffer.append("[")
          var first = true
          for (t <- typeArgs) {
            if (!first) buffer.append(", ")
            first = false
            replacingArgs.find(_._1 eq t) match {
              case Some((_, wildcard)) =>
                buffer.append("_")
                if (wildcard.lowerBound != Nothing) {
                  buffer.append(" >: ")
                  inner(wildcard.lowerBound, stable = false)
                }
                if (wildcard.upperBound != Any) {
                  buffer.append(" <: ")
                  inner(wildcard.upperBound, stable = false)
                }
              case _ => inner(t, stable = false, checkWildcard)
            }
          }
          buffer.append("]")
          if (left.nonEmpty) {
            buffer.append(") forSome {")
            val iter = left.iterator
            while (iter.hasNext) {
              val next = iter.next()
              buffer.append("type ").append(next.name)
              if (next.lowerBound != Nothing) {
                buffer.append(" >: ")
                inner(next.lowerBound, stable = false)
              }
              if (next.upperBound != Any) {
                buffer.append(" <: ")
                inner(next.upperBound, stable = false)
              }
              if (iter.hasNext) buffer.append("; ")
            }
            buffer.append("}")
          }
        case ScExistentialType(q, wilds) => {
          buffer.append("(")
          inner(q, stable = false)
          buffer.append(")")
          buffer.append(" forSome {")
          val iter = wilds.iterator
          while (iter.hasNext) {
            val next = iter.next()
            buffer.append("type ").append(next.name)
            if (next.lowerBound != Nothing) {
              buffer.append(" >: ")
              inner(next.lowerBound, stable = false)
            }
            if (next.upperBound != Any) {
              buffer.append(" <: ")
              inner(next.upperBound, stable = false)
            }
            if (iter.hasNext) buffer.append("; ")
          }
          buffer.append("}")
        }
        case _ => //todo
      }
    }
    inner(t, stable = false)
    buffer.toString()
  }

  private def typeParamText(param: ScTypeParam, subst: ScSubstitutor, nameFun: PsiNamedElement => String, nameWithPointFun: PsiNamedElement => String): String = {
    def typeText0(tp: ScType) = typeText(subst.subst(tp), nameFun, nameWithPointFun)
    var paramText = param.name
    if (param.isContravariant) paramText = "-" + paramText
    else if (param.isCovariant) paramText = "+" + paramText
    param.lowerBound foreach {
      case psi.types.Nothing =>
      case tp: ScType => paramText += (" >: " + typeText0(tp))
    }
    param.upperBound foreach {
      case psi.types.Any =>
      case tp: ScType => paramText += (" <: " + typeText0(tp))
    }
    param.viewBound foreach {
      (tp: ScType) => paramText += (" <% " + typeText0(tp))
    }
    param.contextBound foreach {
      (tp: ScType) =>
        paramText += (" : " + typeText0(ScTypeUtil.stripTypeArgs(subst.subst(tp))))
    }
    paramText
  }
}

object ScTypePresentation {
  val ABSTRACT_TYPE_PREFIX = "AbstractType"
}