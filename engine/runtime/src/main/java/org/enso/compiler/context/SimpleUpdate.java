package org.enso.compiler.context;

import org.enso.compiler.core.IR;
import org.enso.text.editing.model.TextEdit;

/**
 * Simple editing change description.
 *
 * @param ir the current literal
 * @param edit the editor change
 * @param newIr the new literal
 */
public record SimpleUpdate(
  IR.Literal ir,
  TextEdit edit,
  IR.Literal newIr
) {
}
