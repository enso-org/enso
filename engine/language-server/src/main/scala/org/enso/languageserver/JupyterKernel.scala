package org.enso.languageserver

import java.nio.file.Files
import java.nio.file.Paths
import java.util.logging.Level

import io.github.spencerpark.jupyter.channels.JupyterConnection
import io.github.spencerpark.jupyter.channels.JupyterSocket
import io.github.spencerpark.jupyter.kernel.BaseKernel
import io.github.spencerpark.jupyter.kernel.KernelConnectionProperties
import io.github.spencerpark.jupyter.kernel
import io.github.spencerpark.jupyter.kernel.display.DisplayData
import org.enso.polyglot
import org.enso.polyglot.{ExecutionContext, LanguageInfo, Module, TopScope}
import org.graalvm.polyglot.{Context, Value}

/**
  * A wrapper for Enso interpreter for use by Jupyter
  */
class JupyterKernel extends BaseKernel {
  private val context: ExecutionContext =
    new ContextFactory().create(
      "",
      getIO.in,
      getIO.out,
      Repl(SimpleReplIO(getIO.in, getIO.out))
    )
  private val jupyterModule: Module =
    context.getTopScope.createModule("Jupyter")
  jupyterModule.patch("main = Unit")
  private val moduleCons: Value = jupyterModule.getAssociatedConstructor
  private var lastMain: polyglot.Function =
    jupyterModule.getMethod(moduleCons, "main")

  /**
    * Evaluates Enso code in the context of Jupyter request
    *
    * @param expr the expression to execute
    * @return the Jupyter-friendly representation of the result of executing expr
    */
  override def eval(expr: String): DisplayData = {
    jupyterModule.patch(expr)
    val newMain = jupyterModule.getMethod(moduleCons, "main")
    if (!newMain.equals(lastMain)) {
      lastMain = newMain
      new DisplayData(newMain.execute(moduleCons).toString)
    } else {
      DisplayData.EMPTY
    }
  }

  /**
    * Basic language information to display in Jupyter
    * @return the basic language information object
    */
  override def getLanguageInfo: kernel.LanguageInfo =
    new kernel.LanguageInfo.Builder(LanguageInfo.ID)
      .version(LanguageInfo.VERSION)
      .build

  /**
    * Starts the Jupyter kernel server
    * @param connectionFileStr filepath of the Jupyter connection file
    */
  def run(connectionFileStr: String): Unit = {
    val connectionFile = Paths.get(connectionFileStr)

    if (!Files.isRegularFile(connectionFile))
      throw new IllegalArgumentException(
        "Connection file '" + connectionFile + "' isn't a file."
      )

    val contents = new String(Files.readAllBytes(connectionFile))
    JupyterSocket.JUPYTER_LOGGER.setLevel(Level.WARNING)
    val connProps  = KernelConnectionProperties.parse(contents)
    val connection = new JupyterConnection(connProps)

    becomeHandlerForConnection(connection)
    connection.connect()
    connection.waitUntilClose()
  }
}
