package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.arrow.ArrowLanguage;
import org.enso.interpreter.arrow.ArrowParser;
import org.enso.interpreter.arrow.runtime.ArrowCastToFixedSizeArrayFactory;
import org.enso.interpreter.arrow.runtime.ArrowFixedSizeArrayFactory;
import org.enso.interpreter.arrow.runtime.ArrowOperationPlus;

public class ArrowEvalNode extends RootNode {
  private final ArrowParser.Result code;

  public static ArrowEvalNode create(ArrowLanguage language, ArrowParser.Result code) {
    return new ArrowEvalNode(language, code);
  }

  private ArrowEvalNode(ArrowLanguage language, ArrowParser.Result code) {
    super(language, new FrameDescriptor());
    this.code = code;
  }

  public Object execute(VirtualFrame frame) {
    return switch (code.physicalLayout()) {
      case Primitive -> switch (code.mode()) {
        case Allocate -> new ArrowFixedSizeArrayFactory(code.logicalLayout());
        case Cast -> new ArrowCastToFixedSizeArrayFactory(code.logicalLayout());
        case Plus -> {
          var factory = new ArrowFixedSizeArrayFactory(code.logicalLayout());
          yield new ArrowOperationPlus(factory);
        }
        default -> throw CompilerDirectives.shouldNotReachHere("unsupported mode");
      };
      default -> throw CompilerDirectives.shouldNotReachHere("unsupported physical layout");
    };
  }
}
