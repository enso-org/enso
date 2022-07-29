package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

import java.util.List;

@BuiltinType
public class ProjectDescription extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Make_Project_Description", List.of("prim_root_file", "prim_config")));
  }
}
