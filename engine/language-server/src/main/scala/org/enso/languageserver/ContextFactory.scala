package org.enso.languageserver

import java.io.InputStream
import java.io.OutputStream

import org.enso.interpreter.Constants
import org.enso.interpreter.runtime.RuntimeOptions
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
    * @return configured Context instance
    */
  def create(
    packagesPath: String = "",
    in: InputStream      = System.in,
    out: OutputStream    = System.out
  ): Context =
    Context
      .newBuilder(Constants.LANGUAGE_ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.getPackagesPathOption, packagesPath)
      .out(out)
      .in(in)
      .build
}
