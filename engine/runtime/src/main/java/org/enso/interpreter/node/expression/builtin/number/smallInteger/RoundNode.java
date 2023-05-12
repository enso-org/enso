package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Small_Integer", name = "round", description = "Small integer round.")
public class RoundNode extends Node {
    long execute(long self) {
        System.out.println("NOPE small");
        return self;
    }
}
