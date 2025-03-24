package org.enso.compiler.core.ir.module.scope;

import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.Scope;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.collection.immutable.List;

public interface JImport extends Scope {
  @GenerateIR(interfaces = {JImport.class, IRKind.Primitive.class})
  final class Module extends ImportModuleGen {
    @GenerateFields
    public Module(
        @IRChild Name.Qualified name,
        @IRChild(required = false) Name.Literal rename,
        @IRField boolean isAll,
        @IRChild(required = false) List<Name.Literal> onlyNames,
        @IRChild(required = false) List<Name.Literal> hiddenNames,
        @IRField boolean isSynthetic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, rename, isAll, onlyNames, hiddenNames, isSynthetic, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      throw new UnsupportedOperationException("unimplemented");
    }
  }
}
