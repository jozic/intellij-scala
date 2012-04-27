package org.jetbrains.plugins.scala
package codeInsight.intention.literal

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.project.Project
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import lang.lexer.ScalaTokenTypes
import editor.enterHandler.MultilineStringEnterHandler

/**
 * User: Dmitry Naydanov
 * Date: 4/2/12
 */

class AddReplaceSlashRToMLStringIntention extends PsiElementBaseIntentionAction {
  def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean = {
    if (element == null || element.getNode.getElementType != ScalaTokenTypes.tMULTILINE_STRING || !element.getText.contains("\n")) {
      return false
    }

    val calls = MultilineStringEnterHandler.findAllMethodCallsOnMLString(element, "replace")
    !MultilineStringEnterHandler.containsArgs(calls, """"\r"""", "\"\"")
  }

  def getFamilyName: String = """Add .replace("\r","")"""

  override def getText: String = "Add 'replace(\"\\r\", \"\")'"

  override def invoke(project: Project, editor: Editor, element: PsiElement) {
    extensions.inWriteAction {
      editor.getDocument.insertString(element.getTextRange.getEndOffset, ".replace(\"\\r\", \"\")")
    }
  }
}

