package org.jetbrains.plugins.scala.debugger.filters;

import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.options.Configurable;
import com.intellij.xdebugger.settings.XDebuggerSettings;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

/**
 * @author ilyas
 */
@State(
    name = "ScalaDebuggerSettings",
    storages = {
    @Storage(
        id = "sacla_debugger",
        file = "$APP_CONFIG$/scala_debug.xml"
    )}
)
public class ScalaDebuggerSettings extends XDebuggerSettings<ScalaDebuggerSettings> {

  public Boolean DEBUG_DISABLE_SPECIFIC_SCALA_METHODS = true;
  public Boolean FRIENDLY_COLLECTION_DISPLAY_ENABLED = true;
  public Boolean FRIENDLY_OBJECT_REF_DISPLAY_ENABLED = true;
  public Boolean DO_NOT_DISPLAY_STREAMS = true;
  public Integer COLLECTION_START_INDEX = 0;
  public Integer COLLECTION_END_INDEX = 49;

  public ScalaDebuggerSettings() {
    super("scala_debugger");
  }

  @NotNull
  public Configurable createConfigurable() {
    return new ScalaDebuggerSettingsConfigurable(this);
  }

  public ScalaDebuggerSettings getState() {
    return this;
  }

  public void loadState(final ScalaDebuggerSettings state) {
    XmlSerializerUtil.copyBean(state, this);
  }

  public static ScalaDebuggerSettings getInstance() {
    return getInstance(ScalaDebuggerSettings.class);
  }

}