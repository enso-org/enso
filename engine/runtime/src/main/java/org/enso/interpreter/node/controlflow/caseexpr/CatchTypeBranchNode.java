package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNodeGen;
import org.enso.interpreter.runtime.data.Type;

/** An implementation of the case expression specialised to working on Date. */
@NodeInfo(shortName = "TypeMatch")
public abstract class CatchTypeBranchNode extends BranchNode {

    private final Type expectedType;
    private @Child TypeOfNode typeOfNode = TypeOfNode.build();
    private final ConditionProfile profile = ConditionProfile.createCountingProfile();

    CatchTypeBranchNode(Type tpe, RootCallTarget functionNode) {
        super(functionNode);
        this.expectedType = tpe;
    }

    /**
     * Creates a node to handle the case by-type.
     *
     * @param functionNode the function to execute in this case
     * @return a catch-all node
     */
    public static CatchTypeBranchNode build(Type tpe, RootCallTarget functionNode) {
        return CatchTypeBranchNodeGen.create(tpe, functionNode);
    }

    @Specialization
    void doType(VirtualFrame frame, Object state, Type target) {
        if (profile.profile(expectedType == target)) {
            accept(frame, state, new Object[0]);
        }
    }

    @Specialization
    public void doValue(VirtualFrame frame, Object state, Object target) {
        Object typeOfTarget = typeOfNode.execute(target);
        if (profile.profile(expectedType == typeOfTarget)) {
            accept(frame, state, new Object[] { target });
        }
    }
}
