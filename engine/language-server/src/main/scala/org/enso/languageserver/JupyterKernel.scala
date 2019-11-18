package org.enso.languageserver

import java.nio.file.Files
import java.nio.file.Paths
import java.util.logging.Level

import io.github.spencerpark.jupyter.channels.JupyterConnection
import io.github.spencerpark.jupyter.channels.JupyterSocket
import io.github.spencerpark.jupyter.kernel.BaseKernel
import io.github.spencerpark.jupyter.kernel.KernelConnectionProperties
import io.github.spencerpark.jupyter.kernel.LanguageInfo
import io.github.spencerpark.jupyter.kernel.display.DisplayData
import org.enso.interpreter.Constants
import org.graalvm.polyglot.Context

/**
  * A wrapper for Enso interpreter for use by Jupyter
  */
class JupyterKernel extends BaseKernel {
  private val context: Context =
    new ContextFactory().create("", getIO.in, getIO.out)

  /**
    * Evaluates Enso code in the context of Jupyter request
    *
    * @param expr the expression to execute
    * @return the Jupyter-friendly representation of the result of executing `expr`
    */
  override def eval(expr: String) =
    new DisplayData(context.eval(Constants.LANGUAGE_ID, expr).toString)

  /**
    * Basic language information to display in Jupyter
    * @return the basic language information object
    */
  override def getLanguageInfo: LanguageInfo =
    new LanguageInfo.Builder(Constants.LANGUAGE_ID)
      .version(Constants.LANGUAGE_VERSION)
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
