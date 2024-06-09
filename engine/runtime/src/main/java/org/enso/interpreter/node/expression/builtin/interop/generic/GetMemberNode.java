package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Polyglot",
    name = "get_member",
    description = "Gets a member by name from a polyglot object.",
    autoRegister = false)
public class GetMemberNode extends Node {
  private @Child HostValueToEnsoNode fromHost = HostValueToEnsoNode.build();
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object object, Object member_name) {
    try {
      var value = library.readMember(object, expectStringNode.execute(member_name));
      return fromHost.execute(value);
    } catch (UnsupportedMessageException | UnknownIdentifierException e) {
      err.enter();
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, null, e);
    }
  }
}
