package org.jetbrains.plugins.scala.compiler;

import com.intellij.compiler.CompilerConfiguration;
import com.intellij.compiler.CompilerConfigurationImpl;
import com.intellij.compiler.OutputParser;
import com.intellij.compiler.impl.CompilerUtil;
import com.intellij.compiler.impl.javaCompiler.ExternalCompiler;
import com.intellij.compiler.impl.javaCompiler.ModuleChunk;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.JavaSdkType;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.MockJdkWrapper;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderEnumerator;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.encoding.EncodingManager;
import com.intellij.util.ArrayUtil;
import com.intellij.util.PathUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.plugins.scala.ScalaBundle;
import org.jetbrains.plugins.scala.ScalaFileType;
import org.jetbrains.plugins.scala.compiler.rt.ClassRunner;
import org.jetbrains.plugins.scala.config.CompilerLibraryData;
import org.jetbrains.plugins.scala.config.Libraries;
import org.jetbrains.plugins.scala.config.ScalaFacet;
import org.jetbrains.plugins.scala.util.ScalaUtils;
import org.jetbrains.plugins.scala.util.macroDebug.ScalaMacroDebuggingUtil;
import scala.Option;
import scala.io.Source;

import java.io.*;
import java.net.InetAddress;
import java.nio.charset.Charset;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author ilyas, Pavel Fatin
 */
public class ScalacBackendCompiler extends ExternalCompiler {

  private static final Logger LOG = Logger.getInstance("#org.jetbrains.plugins.scala.compiler.ScalacBackendCompiler");

  private final Project myProject;
  private boolean myFsc;
  private final List<File> myTempFiles = new ArrayList<File>();

  // Scalac parameters
  @NonNls
  private static final String VERBOSE_PROPERTY = "-verbose";
  @NonNls
  private static final String DESTINATION_COMPILER_PROPERTY = "-d";

  private final static HashSet<FileType> COMPILABLE_FILE_TYPES = new HashSet<FileType>(Arrays.asList(ScalaFileType.SCALA_FILE_TYPE, StdFileTypes.JAVA));

  public ScalacBackendCompiler(Project project, boolean fsc) {
    myProject = project;
    myFsc = fsc;
  }

  public boolean checkCompiler(CompileScope scope) {
    // Do not run compiler for pure Java projects
    final Module[] allModules = scope.getAffectedModules();

    // Just skip pure Java projects
    if (!isScalaProject(allModules)) return true;


    VirtualFile[] scalaFiles = scope.getFiles(ScalaFileType.SCALA_FILE_TYPE, true);
    VirtualFile[] javaFiles = scope.getFiles(StdFileTypes.JAVA, true);
    if (scalaFiles.length == 0 && javaFiles.length == 0) return true;

    final ProjectFileIndex index = ProjectRootManager.getInstance(myProject).getFileIndex();
    Set<Module> modules = new HashSet<Module>();
    for (VirtualFile file : ArrayUtil.mergeArrays(javaFiles, scalaFiles, VirtualFile.class)) {
      Module module = index.getModuleForFile(file);
      if (module != null) {
        modules.add(module);
      }
    }

    boolean hasJava = false;
    for (Module module : modules) {
      if (ScalaUtils.isSuitableModule(module)) {
        hasJava = true;
      }
    }
    if (!hasJava) return false; //this compiler work with only Java modules, so we don't need to continue.


    ScalaFacet facet = null;

    // Check for compiler existence
    for (Module module : allModules) {
      Option<ScalaFacet> facetOption = ScalaFacet.findIn(module);
      if (facetOption.isDefined()) {
        facet = facetOption.get();
        break;
      }
    }

    if (facet == null) {
      Messages.showErrorDialog(myProject, 
          ScalaBundle.message("cannot.compile.scala.files.no.facet"),
          ScalaBundle.message("cannot.compile"));
      return false;
    }

    Option<CompilerLibraryData> compilerOption = facet.compiler();
    
    if (facet.fsc()) {
      ScalacSettings settings = ScalacSettings.getInstance(myProject);
      Option<CompilerLibraryData> data =
          Libraries.findBy(settings.COMPILER_LIBRARY_NAME, settings.COMPILER_LIBRARY_LEVEL, myProject);
      if(data.isDefined()) {
        Option<String> problemOption = data.get().problem();
        if (problemOption.isDefined()) {
          Messages.showErrorDialog(myProject,
              String.format("Please, adjust compiler library for project FSC: %s", problemOption.get()),
              ScalaBundle.message("cannot.compile"));
          return false;
        }
      } else {
        Messages.showErrorDialog(myProject,
            "Please, set up compiler library for project FSC ",
            ScalaBundle.message("cannot.compile"));
        return false;
      }
    } else {
      if (compilerOption.isEmpty()) {
        Messages.showErrorDialog(myProject,
            ScalaBundle.message("cannot.compile.scala.files.no.compiler"),
            ScalaBundle.message("cannot.compile"));
        return false;
      }

      Option<String> problemOption = compilerOption.get().problem();

      if (problemOption.isDefined()) {
        Messages.showErrorDialog(myProject,
            ScalaBundle.message("cannot.compile.scala.files.compiler.problem", problemOption.get()),
            ScalaBundle.message("cannot.compile"));
        return false;
      }
    }

    Set<Module> nojdkModules = new HashSet<Module>();
    for (Module module : scope.getAffectedModules()) {
      if (!(ScalaUtils.isSuitableModule(module))) continue;
      Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
      if (sdk == null || !(sdk.getSdkType() instanceof JavaSdkType)) {
        nojdkModules.add(module);
      }
    }

    if (!nojdkModules.isEmpty()) {
      final Module[] noJdkArray = nojdkModules.toArray(new Module[nojdkModules.size()]);
      if (noJdkArray.length == 1) {
        Messages.showErrorDialog(myProject, ScalaBundle.message("cannot.compile.scala.files.no.sdk", noJdkArray[0].getName()), ScalaBundle.message("cannot.compile"));
      } else {
        StringBuilder modulesList = new StringBuilder();
        for (int i = 0; i < noJdkArray.length; i++) {
          if (i > 0) modulesList.append(", ");
          modulesList.append(noJdkArray[i].getName());
        }
        Messages.showErrorDialog(myProject, ScalaBundle.message("cannot.compile.scala.files.no.sdk.mult", modulesList.toString()), ScalaBundle.message("cannot.compile"));
      }
      return false;
    }

    //todo: Project SDK can be undefined, however Module SDK is defined
    if (myFsc && !ApplicationManager.getApplication().isUnitTestMode() &&
        ProjectRootManager.getInstance(myProject).getProjectSdk() ==  null) {
      Messages.showErrorDialog(myProject, "Please, set up project SDK (required for project FSC instantiation)", ScalaBundle.message("cannot.compile"));
      return false;
    }

    // There's a bug in IDEA resulting in the incorrect compiler
    // invocation order when annotation processing is enabled
    // (Javac is invoked before Scalac no matter what).
    ScalacSettings settings = ScalacSettings.getInstance(myProject);
    if (settings.SCALAC_BEFORE) {
      final CompilerConfigurationImpl config = (CompilerConfigurationImpl) CompilerConfiguration.getInstance(myProject);
      LinkedList<String> names = new LinkedList<String>();
      for (Module module : allModules) {
        if (config.getAnnotationProcessingConfiguration(module).isEnabled()) {
          names.add(module.getName());
        }
      }
      if (!names.isEmpty()) {
        Messages.showErrorDialog(myProject,
            "Scala compiler cannot be invoked before the Java annotation processing, " +
                String.format("yet the annotation processing is enabled in the following modules: %s. ", StringUtil.join(names, ", ")) +
                "Please either disable the annotation processing (Project Settings / Compiler / Annotation Processors) " +
                "or change the compiler invocation order (Project Settings / Compiler / Scala Compiler).",
            ScalaBundle.message("cannot.compile"));
        return false;
      }
    }

    return true;
  }

  public static boolean isScalaProject(Module[] allModules) {
    boolean isScalaProject = false;
    for (Module module : allModules) {
      if (ScalaFacet.isPresentIn(module)) {
        isScalaProject = true;
        break;
      }
    }
    return isScalaProject;
  }

  @NotNull
  public String getId() {
    return "Scalac";
  }

  @NotNull
  public String getPresentableName() {
    return ScalaBundle.message("scalac.compiler.name");
  }

  @NotNull
  public Configurable createConfigurable() {
    return null;
  }

  public OutputParser createOutputParser(@NotNull String outputDir) {
    return new ScalacOutputParser();
  }

  @NotNull
  public String[] createStartupCommand(final ModuleChunk chunk, CompileContext context, final String outputPath) throws IOException, IllegalArgumentException {
    final ArrayList<String> commandLine = new ArrayList<String>();
    final Exception[] ex = new Exception[]{null};
    ApplicationManager.getApplication().runReadAction(new Runnable() {
      public void run() {
        try {
          createStartupCommandImpl(chunk, commandLine, outputPath);
        }
        catch (IllegalArgumentException e) {
          ex[0] = e;
        }
        catch (IOException e) {
          ex[0] = e;
        }
      }
    });
    if (ex[0] != null) {
      if (ex[0] instanceof IOException) {
        throw (IOException) ex[0];
      } else if (ex[0] instanceof IllegalArgumentException) {
        throw (IllegalArgumentException) ex[0];
      } else {
        LOG.error(ex[0]);
      }
    }
    return commandLine.toArray(new String[commandLine.size()]);
  }

  @NotNull
  @Override
  public Set<FileType> getCompilableFileTypes() {
    return COMPILABLE_FILE_TYPES;
  }

  private void createStartupCommandImpl(ModuleChunk chunk, ArrayList<String> commandLine, String outputPath) throws IOException {
    final Sdk jdk = getJdkForStartupCommand(chunk);
    final String versionString = jdk.getVersionString();
    if (versionString == null || "".equals(versionString)) {
      throw new IllegalArgumentException(ScalaBundle.message("javac.error.unknown.jdk.version", jdk.getName()));
    }
    final JavaSdkType sdkType = (JavaSdkType) jdk.getSdkType();

    final String toolsJarPath = sdkType.getToolsPath(jdk);
    if (toolsJarPath == null) {
      throw new IllegalArgumentException(ScalaBundle.message("javac.error.tools.jar.missing", jdk.getName()));
    }
    if (!allModulesHaveSameScalaSdk(chunk)) {
      throw new IllegalArgumentException(ScalaBundle.message("different.scala.sdk.in.modules"));
    }

    String javaExecutablePath = sdkType.getVMExecutablePath(jdk);
    commandLine.add(javaExecutablePath);

    // We should not launch Scalac on Java files only,
    // yet the initial filtering in IDEA (which uses ScalaCompiler.isCompilableFile)
    // is done on production and test sources combined (while Scalac can be launched twice).
    // Because there's no way to interrupt a started compilation,
    // we return a command line to launch a bare Java command as a "compilation" process.
    if (!ScalaCompiler.containsScalaFiles(chunk.getFilesToCompile())) {
      return;
    }

    ScalacSettings settings = ScalacSettings.getInstance(myProject);

    //For debug
    //commandLine.add("-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5009");

    ScalaFacet[] facets = ScalaFacet.findIn(chunk.getModules());

    if (myFsc) {
      if (!settings.INTERNAL_SERVER && !settings.SHARED_DIRECTORY.isEmpty()) {
        commandLine.add(String.format("-Djava.io.tmpdir=%s", settings.SHARED_DIRECTORY));
      }
    } else {
      for(ScalaFacet facet : facets) {
        commandLine.addAll(Arrays.asList(facet.javaParameters()));
        break;
      }
    }

    CompilerUtil.addLocaleOptions(commandLine, false);

    commandLine.add("-cp");
    final StringBuilder classPathBuilder = new StringBuilder();

    if (myFsc) {
      Option<CompilerLibraryData> lib = Libraries.findBy(settings.COMPILER_LIBRARY_NAME, settings.COMPILER_LIBRARY_LEVEL, myProject);
      classPathBuilder.append(lib.get().classpath());
      classPathBuilder.append(File.pathSeparator);
    } else {
      for(ScalaFacet facet : facets) {
        classPathBuilder.append(facet.classpath());
        classPathBuilder.append(File.pathSeparator);
        break;
      }
    }

    String rtJarPath = PathUtil.getJarPathForClass(ClassRunner.class);
    classPathBuilder.append(rtJarPath).append(File.pathSeparator);

    commandLine.add(classPathBuilder.toString());

    commandLine.add(ClassRunner.class.getName());

    if (myFsc) {
      commandLine.add("scala.tools.nsc.CompileClient");
    } else {
      commandLine.add("scala.tools.nsc.Main");
    }

    String[] parameters = facets.length > 0 ? facets[0].compilerParameters() : new String[] {};

    try {
      File fileWithParams = File.createTempFile("scalac", ".tmp");
      fillFileWithScalacParams(chunk, fileWithParams, outputPath, myProject, parameters);

      String path = fileWithParams.getPath();
      commandLine.add(path);

      if (LOG.isDebugEnabled()) {
        for (String s : commandLine) LOG.debug(s);
        String s = Source.fromFile(fileWithParams, "UTF8").getLines().mkString("\n");
        LOG.debug(s);
      }
    } catch (IOException e) {
      LOG.error(e);
    }
  }

  private static String getEncodingOptions() {
    StringBuilder options = new StringBuilder();
    final Charset ideCharset = EncodingManager.getInstance().getDefaultCharset();
    if (!Comparing.equal(CharsetToolkit.getDefaultSystemCharset(), ideCharset)) {
      options.append("-encoding ");
      options.append(ideCharset.name());
    }
    return options.toString();
  }

  private void fillFileWithScalacParams(ModuleChunk chunk, File fileWithParameters, String outputPath,
                                               Project myProject, String[] parameters)
      throws IOException {

    PrintStream printer = new PrintStream(new FileOutputStream(fileWithParameters));
//    PrintStream printer = System.out;

    ScalacSettings settings = ScalacSettings.getInstance(myProject);

    if (myFsc) {
      if (settings.INTERNAL_SERVER) {
        int port = myProject.getComponent(FscServerLauncher.class).port();
        if (port != -1) {
          printer.println("-server");
          printer.println(String.format("%s:%s", InetAddress.getLocalHost().getHostAddress(), port));
        }
      } else {
        printer.println("-server");
        printer.println(String.format("%s:%s", settings.REMOTE_HOST, settings.REMOTE_PORT));
      }

      String compilerVersion = Libraries.findBy(settings.COMPILER_LIBRARY_NAME,
          settings.COMPILER_LIBRARY_LEVEL, myProject).get().version().get();

      if (!compilerVersion.startsWith("2.8")) {
        printer.println("-max-idle");
        printer.println(settings.IDLE_TIMEOUT);
      }
    }

    StringTokenizer tokenizer = new StringTokenizer(getEncodingOptions(), " ");
    while (tokenizer.hasMoreTokens()) {
      printer.println(tokenizer.nextToken());
    }
    printer.println(VERBOSE_PROPERTY);
    if (ScalaMacroDebuggingUtil.isEnabled()) printer.println("-Yrangepos");
//    printer.println(DEBUG_PROPERTY);
    //printer.println(WARNINGS_PROPERTY);
//    printer.println(DEBUG_INFO_LEVEL_PROPEERTY);

    for(String parameter : parameters)
      printer.println(parameter);
    
    printer.println(DESTINATION_COMPILER_PROPERTY);

    printer.println(outputPath.replace('/', File.separatorChar));

    //write classpath
    printer.println("-cp");

    printer.print(chunk.getCompilationBootClasspath());
    printer.print(File.pathSeparator);
    printer.println(chunk.getCompilationClasspath());

    List<VirtualFile> filesToCompile = new LinkedList<VirtualFile>();
    if (settings.SCALAC_BEFORE) {
      // Add both Scala and Java files
      filesToCompile.addAll(chunk.getFilesToCompile());

      // TODO Remove this step when IDEA's src/test compilation order will be fixed
      // Add Java source files as dependencies for the tests.
      // The action should not be required, because at this stage Java sources shoud already be compiled
      // (so Scalac can use them as .class files).
      // However the IDEA's internal compilation system invokes Scala compiler for test compilation
      // before compilation of Java module sources.
      if (isTestChunk(chunk)) {
        ProjectRootManager rootManager = ProjectRootManager.getInstance(myProject);
        for (Module module : chunk.getModules()) {
          for (VirtualFile root : OrderEnumerator.orderEntries(module).getSourceRoots()) {
            if (rootManager.getFileIndex().isInSourceContent(root) &&
                !rootManager.getFileIndex().isInTestSourceContent(root)) {
              printJavaSourceFiles(printer, root);
            }
          }
        }
      }
    } else {
      // Add Scala files only
      for (VirtualFile file : chunk.getFilesToCompile()) {
        if (file.getFileType() == ScalaFileType.SCALA_FILE_TYPE) {
          filesToCompile.add(file);
        }
      }
    }

    for (VirtualFile file : filesToCompile) {
      printer.println(file.getPath());
    }

    printer.close();

    if (LOG.isDebugEnabled()) {
      try {
        LOG.debug(FileUtil.loadTextAndClose(new FileReader(fileWithParameters)));
      } catch (IOException e) {
        // ignore
      }
    }
  }

  private static boolean isTestChunk(ModuleChunk chunk) {
    final Module[] modules = chunk.getModules();
    if (modules.length > 0) {
      ProjectRootManager rm = ProjectRootManager.getInstance(modules[0].getProject());
      for (VirtualFile file : chunk.getSourceRoots()) {
        if (file != null && rm.getFileIndex().isInTestSourceContent(file)) {
          return true;
        }
      }
    }
    return false;
  }

  private static void printJavaSourceFiles(PrintStream stream, VirtualFile src) {
    if (src.getPath().contains("!/")) return;
    if (src.isDirectory()) {
      for (VirtualFile file : src.getChildren()) {
        printJavaSourceFiles(stream, file);
      }
    } else if (src.getFileType() == StdFileTypes.JAVA) {
      stream.println(src.getPath());
    }
  }

  private boolean allModulesHaveSameScalaSdk(ModuleChunk chunk) {
    if (chunk.getModuleCount() == 0) return false;
    final Module[] modules = chunk.getModules();
    String firstVersion = null;
    for (Module module : modules) {
      Option<ScalaFacet> facetOption = ScalaFacet.findIn(module);
      if (facetOption.isDefined()) {
        final ScalaFacet f = (ScalaFacet) facetOption.get();
        String version = f.version();
        if (firstVersion == null) {
          firstVersion = version;
        } else {
          if (!version.equals(firstVersion)) return false;
        }
      }
    }
    return true;
  }

  public void compileFinished() {
    FileUtil.asyncDelete(myTempFiles);
  }

  private Sdk getJdkForStartupCommand(final ModuleChunk chunk) {
    final Sdk jdk = chunk.getJdk();
    if (ApplicationManager.getApplication().isUnitTestMode()) {
      final String jdkHomePath = CompilerConfigurationImpl.getTestsExternalCompilerHome();
      if (jdkHomePath == null) {
        throw new IllegalArgumentException("[TEST-MODE] Cannot determine home directory for JDK to use javac from");
      }
      // when running under Mock JDK use VM executable from the JDK on which the tests run
      return new MockJdkWrapper(jdkHomePath, jdk);
    }
    return jdk;
  }

  public OutputParser createErrorParser(@NotNull String outputDir, final Process process) {
    return new ScalacOutputParser() {
      AtomicBoolean myDumperStarted = new AtomicBoolean(false);

      @Override
      public boolean processMessageLine(final Callback callback) {
        if (!myDumperStarted.getAndSet(true)) {
          ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
            public void run() {
              try {
                process.waitFor();
              }
              catch (InterruptedException ignored) {
              }
              flushWrittenList(callback);
            }
          });
        }
        return super.processMessageLine(callback);
      }
    };
  }
}
