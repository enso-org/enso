package org.enso.polyglot;

/** Defines a stage of compilation of the module. */
public enum CompilationStage {
  INITIAL(0),
  AFTER_PARSING(1),
  AFTER_IMPORT_RESOLUTION(2),
  AFTER_GLOBAL_TYPES(3),
  AFTER_STATIC_PASSES(4),
  AFTER_RUNTIME_STUBS(5),
  AFTER_CODEGEN(6);

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
