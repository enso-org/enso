package org.enso.compiler.context;

import org.enso.compiler.core.ir.Literal;
import org.enso.text.editing.model.TextEdit;

/**
 * Simple editing change description.
 *
 * @param ir the current literal
 * @param edit the editor change
 * @param newIr the new literal
 */
public record SimpleUpdate(Literal ir, TextEdit edit, Literal newIr) {}
