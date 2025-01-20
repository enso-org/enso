package org.enso.compiler.core.ir;

import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;

@GenerateIR(interfaces = {Expression.class})
public final class Empty extends EmptyGen {
  @GenerateFields
  public Empty(IdentifiedLocation identifiedLocation, MetadataStorage passData) {
    super(identifiedLocation, passData);
  }

  public Empty(IdentifiedLocation identifiedLocation) {
    this(identifiedLocation, new MetadataStorage());
  }
}
