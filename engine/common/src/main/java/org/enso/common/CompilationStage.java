package org.enso.common;

/** Defines a stage of compilation of the module. */
public enum CompilationStage {
  INITIAL(0),
  AFTER_PARSING(10),
  AFTER_IMPORT_RESOLUTION(20),
  AFTER_GLOBAL_TYPES(30),
  AFTER_STATIC_PASSES(40),
  AFTER_TYPE_INFERENCE_PASSES(45),
  AFTER_RUNTIME_STUBS(50),
  AFTER_CODEGEN(60);

  private final int ordinal;

  CompilationStage(int ordinal) {
    this.ordinal = ordinal;
  }

  /**
   * Checks whether the current compilation stage is at least as advanced as the provided one.
   *
   * @param stage the stage to compare to.
   * @return whether or not {@code this} is at least as advanced as {@code stage}.
   */
  public boolean isAtLeast(CompilationStage stage) {
    return ordinal >= stage.ordinal;
  }

  /**
   * Checks that the current compilation stage is before the provided one.
   *
   * @param stage the stage to compare to.
   * @return whether or not {@code this} is before then {@code stage}.
   */
  public boolean isBefore(CompilationStage stage) {
    return ordinal < stage.ordinal;
  }
}
