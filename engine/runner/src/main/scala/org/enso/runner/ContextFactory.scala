package org.enso.runner

import java.io.InputStream
import java.io.OutputStream

import org.enso.interpreter.instrument.ReplDebuggerInstrument
import org.enso.polyglot.{LanguageInfo, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.Context

/**
  * Utility class for creating Graal polyglot contexts.
  */
class ContextFactory {

  /**
    * Creates a new Graal polyglot context.
    *
    * @param packagesPath Enso packages path
    * @param in the input stream for standard in
    * @param out the output stream for standard out
    * @param repl the Repl manager to use for this context
    * @return configured Context instance
    */
  def create(
    packagesPath: String = "",
    in: InputStream,
    out: OutputStream,
    repl: Repl,
    strictErrors: Boolean = false
  ): PolyglotContext = {
    val context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.PACKAGES_PATH, packagesPath)
      .option(RuntimeOptions.STRICT_ERRORS, strictErrors.toString)
      .out(out)
      .in(in)
      .build
    val instrument = context.getEngine.getInstruments
      .get(ReplDebuggerInstrument.INSTRUMENT_ID)
      .lookup(classOf[ReplDebuggerInstrument])
    instrument.setSessionManager(repl)
    new PolyglotContext(context)
  }
}
