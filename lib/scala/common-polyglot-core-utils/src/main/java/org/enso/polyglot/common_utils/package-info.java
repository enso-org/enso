/**
 * This package contains common utilities which can be used both by the engine runtime and the libraries.
 * <p>
 * This allows us to avoid duplicating code between the runtime and library projects for operations that need to be
 * accessible on both sides.
 * <p>
 * The utilities that belong here are mostly operations that are builtins of the Enso language but also need to be used
 * from our Java libraries where the cost of calling back to Enso methods is relatively high, so accessing the Java
 * implementations directly is desirable. The primary example of that is the algorithm for computing the length of a
 * string by counting the grapheme clusters.
 * <p>
 * Due to classpath separation, the class files of this package will be duplicated with one copy embedded in the engine
 * and another attached as `common-polyglot-core-utils.jar` placed in the `polyglot` directory of the Standard.Base
 * library. But it allows us to avoid duplicating the code, so we can have a single source of truth for each
 * implementation.
 * <p>
 * Due to the copying, the project should not be expanded too much, but all utilities which would end up being
 * duplicated are best moved here.
 */
package org.enso.polyglot.common_utils;
