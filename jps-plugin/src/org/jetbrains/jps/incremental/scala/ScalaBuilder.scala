package org.jetbrains.jps.incremental.scala

import _root_.java.util.Collections
import java.io.File
import com.intellij.openapi.util.io.FileUtil
import com.intellij.util.PathUtil
import com.intellij.util.Processor
import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.builders.{BuildRootDescriptor, BuildTarget, DirtyFilesHolder}
import org.jetbrains.jps.builders.java.JavaSourceRootDescriptor
import org.jetbrains.jps.incremental._
import messages.BuildMessage.Kind
import org.jetbrains.jps.incremental.messages.CompilerMessage
import org.jetbrains.jps.incremental.messages.ProgressMessage
import org.jetbrains.jps.incremental.scala.data.CompilationData
import org.jetbrains.jps.incremental.scala.data.CompilerData
import org.jetbrains.jps.incremental.scala.data.SbtData
import collection.JavaConverters._
import org.jetbrains.jps.incremental.ModuleLevelBuilder.{OutputConsumer, ExitCode}
import sbt.inc.{AnalysisFormats, FileBasedStore, AnalysisStore}
import org.jetbrains.jps.model.java.JavaSourceRootType

/**
 * @author Pavel Fatin
 */
class ScalaBuilder extends ModuleLevelBuilder(BuilderCategory.TRANSLATOR) {
  def getPresentableName = "Scala builder"

  def build(context: CompileContext, chunk: ModuleChunk,
            dirtyFilesHolder: DirtyFilesHolder[JavaSourceRootDescriptor, ModuleBuildTarget],
            outputConsumer: OutputConsumer): ModuleLevelBuilder.ExitCode = {

    context.processMessage(new ProgressMessage("Searching for compilable files..."))
    val filesToCompile = ScalaBuilder.collectCompilableFiles(chunk)

    if (filesToCompile.isEmpty) {
      return ExitCode.NOTHING_DONE
    }

    val sources = filesToCompile.keySet.toSeq

    val client = new IdeClient("scala", context, outputConsumer, filesToCompile.get)

    client.progress("Reading compilation settings...")

    ScalaBuilder.compilerFactory.flatMap { compilerFactory =>
      CompilerData.from(context, chunk).flatMap { compilerData =>
        CompilationData.from(sources, context, chunk).map { compilationData =>
          client.progress("Instantiating compiler...")
          val compiler = compilerFactory.createCompiler(compilerData, client, ScalaBuilder.createAnalysisStore)

          client.progress("Compiling...")
          compiler.compile(compilationData, client)
        }
      }
    } match {
      case Left(error) =>
        client.error(error)
        ExitCode.ABORT
      case Right(_) =>
        client.progress("Compilation completed", Some(1.0F))
        ExitCode.OK
    }
  }
}

object ScalaBuilder {
  // Globally cached instance of CompilerFactory
  private var cachedCompilerFactory: Option[CompilerFactory] = None

  def compilerFactory: Either[String, CompilerFactory] = cachedCompilerFactory.map(Right(_)).getOrElse {
    val sbtData = {
      val classLoader = getClass.getClassLoader
      val pluginRoot = (new File(PathUtil.getJarPathForClass(getClass))).getParentFile
      val systemRoot = Utils.getSystemRoot
      val javaClassVersion = System.getProperty("java.class.version")

      SbtData.from(classLoader, pluginRoot, systemRoot, javaClassVersion)
    }

    sbtData.map { data =>
      val factory = new CachingFactory(new CompilerFactoryImpl(data), 5, 5)
      cachedCompilerFactory = Some(factory)
      factory
    }
  }

  private def collectCompilableFiles(chunk: ModuleChunk): Map[File, BuildTarget[_ <: BuildRootDescriptor]] = {
    var result = Map[File, BuildTarget[_ <: BuildRootDescriptor]]()

    for (target <- chunk.getTargets.asScala;
         rootType = if (target.isTests) JavaSourceRootType.TEST_SOURCE else JavaSourceRootType.SOURCE;
         root <- target.getModule.getSourceRoots(rootType).asScala) {

      FileUtil.processFilesRecursively(root.getFile, new Processor[File] {
        def process(file: File) = {
          val path = file.getPath
          if (path.endsWith(".scala") || path.endsWith(".java")) {
            result += file -> target
          }
          true
        }
      })
    }

    result
  }

  private def createAnalysisStore(cacheFile: File): AnalysisStore = {
    import sbinary.DefaultProtocol.{immutableMapFormat, immutableSetFormat, StringFormat, tuple2Format}
    import sbt.inc.AnalysisFormats._
    val store = FileBasedStore(cacheFile)(AnalysisFormats.analysisFormat, AnalysisFormats.setupFormat)
    AnalysisStore.sync(AnalysisStore.cached(store))
  }
}

private class IdeClient(compilerName: String,
                        context: CompileContext,
                        consumer: OutputConsumer,
                        sourceToTarget: File => Option[BuildTarget[_ <: BuildRootDescriptor]]) extends Client {

  def message(kind: Kind, text: String, source: Option[File], line: Option[Long], column: Option[Long]) {
    val sourcePath = source.map(file => FileUtil.toCanonicalPath(file.getPath))

    context.processMessage(new CompilerMessage(compilerName, kind, text, sourcePath.orNull,
      -1L, -1L, -1L, line.getOrElse(-1L), column.getOrElse(-1L)))
  }

  def trace(exception: Throwable) {
    context.processMessage(new CompilerMessage(compilerName, exception))
  }

  def progress(text: String, done: Option[Float]) {
    context.processMessage(new ProgressMessage(text, done.getOrElse(-1.0F)))
  }

  def generated(source: File, module: File) {
    val target = sourceToTarget(source).getOrElse {
      throw new RuntimeException("Unknown source file: " + source)
    }
    consumer.registerOutputFile(target, module, Collections.emptyList())
  }

  def isCanceled = context.getCancelStatus.isCanceled
}