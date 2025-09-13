package org.enso.compiler.data;

import java.io.PrintStream;
import scala.Option;

/**
 * Configuration for the compiler.
 *
 * @param autoParallelismEnabled whether or not automatic parallelism detection is enabled.
 * @param warningsEnabled whether or not warnings are enabled
 * @param privateCheckEnabled whether or not private keyword is enabled
 * @param staticAnalysisEnabled whether or not type inference, and other static analysis, is enabled
 * @param treatWarningsAsErrors If warnings should be treated as errors.
 * @param dumpModuleIR identification (name) of a module to dump
 * @param isStrictErrors if true, presence of any Error in IR will result in an exception
 * @param isLintingDisabled if true, compilation should not run any linting passes
 * @param outputRedirect redirection of the output of warnings and errors of compiler
 * @param removeUnusedImports If true, unused imports will be removed from the module sources.
 *     Beware this changes the files in place, use with caution.
 */
public record CompilerConfig(
    boolean autoParallelismEnabled,
    boolean warningsEnabled,
    boolean privateCheckEnabled,
    boolean staticAnalysisEnabled,
    boolean treatWarningsAsErrors,
    Option<IRDumperConfig> dumpModuleIR,
    boolean isStrictErrors,
    boolean isLintingDisabled,
    Option<PrintStream> outputRedirect,
    boolean parallelParsing,
    boolean removeUnusedImports) {
  public static Builder builder() {
    return new Builder();
  }

  public static CompilerConfig createDefault() {
    return new Builder().build();
  }

  public static final class Builder {
    private boolean autoParallelismEnabled = false;
    private boolean warningsEnabled = true;
    private boolean privateCheckEnabled = true;
    private boolean staticAnalysisEnabled = false;
    private boolean treatWarningsAsErrors = false;
    private Option<IRDumperConfig> dumpModuleIR = Option.empty();
    private boolean isStrictErrors = false;
    private boolean isLintingDisabled = false;
    private Option<PrintStream> outputRedirect = Option.empty();
    private boolean parallelParsing = false;
    private boolean removeUnusedImports = false;

    public Builder autoParallelismEnabled(boolean autoParallelismEnabled) {
      this.autoParallelismEnabled = autoParallelismEnabled;
      return this;
    }

    public Builder warningsEnabled(boolean warningsEnabled) {
      this.warningsEnabled = warningsEnabled;
      return this;
    }

    public Builder privateCheckEnabled(boolean privateCheckEnabled) {
      this.privateCheckEnabled = privateCheckEnabled;
      return this;
    }

    public Builder staticAnalysisEnabled(boolean staticAnalysisEnabled) {
      this.staticAnalysisEnabled = staticAnalysisEnabled;
      return this;
    }

    public Builder treatWarningsAsErrors(boolean treatWarningsAsErrors) {
      this.treatWarningsAsErrors = treatWarningsAsErrors;
      return this;
    }

    public Builder dumpModuleIR(Option<IRDumperConfig> dumpModuleIR) {
      this.dumpModuleIR = dumpModuleIR;
      return this;
    }

    public Builder isStrictErrors(boolean isStrictErrors) {
      this.isStrictErrors = isStrictErrors;
      return this;
    }

    public Builder isLintingDisabled(boolean isLintingDisabled) {
      this.isLintingDisabled = isLintingDisabled;
      return this;
    }

    public Builder outputRedirect(Option<PrintStream> outputRedirect) {
      this.outputRedirect = outputRedirect;
      return this;
    }

    public Builder parallelParsing(boolean parallelParsing) {
      this.parallelParsing = parallelParsing;
      return this;
    }

    public Builder removeUnusedImports(boolean removeUnusedImports) {
      this.removeUnusedImports = removeUnusedImports;
      return this;
    }

    public CompilerConfig build() {
      return new CompilerConfig(
          autoParallelismEnabled,
          warningsEnabled,
          privateCheckEnabled,
          staticAnalysisEnabled,
          treatWarningsAsErrors,
          dumpModuleIR,
          isStrictErrors,
          isLintingDisabled,
          outputRedirect,
          parallelParsing,
          removeUnusedImports);
    }
  }
}
