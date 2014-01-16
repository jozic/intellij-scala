package org.jetbrains.sbt
package project.module

import com.intellij.openapi.roots.ui.configuration._
import com.intellij.openapi.module.{ModuleType, ModuleConfigurationEditor}

/**
 * User: Dmitry Naydanov
 * Date: 11/22/13
 */
class SbtModuleEditorProvider extends ModuleConfigurationEditorProvider {
  def createEditors(state: ModuleConfigurationState) = {
    val module = state.getRootModel.getModule

    ModuleType.get(module) match {
      case _: SbtModuleType => Array(
        new ContentEntriesEditor(module.getName, state),
        new DefaultModuleConfigurationEditorFactoryImpl().createOutputEditor(state),
        new ClasspathEditor(state))
      case _ =>
        ModuleConfigurationEditor.EMPTY
    }
  }
}