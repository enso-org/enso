package org.enso.interpreter.dsl.builtins;

import org.apache.commons.lang3.StringUtils;

import javax.lang.model.element.VariableElement;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public abstract class MethodGenerator {
    protected final boolean isStatic;
    protected final boolean isConstructor;
    private final boolean convertToGuestValue;
    protected final TypeWithKind returnTpe;

    public MethodGenerator(boolean isStatic, boolean isConstructor, boolean convertToGuestValue, TypeWithKind returnTpe) {
        this.isStatic = isStatic;
        this.isConstructor = isConstructor;
        this.convertToGuestValue = convertToGuestValue;
        this.returnTpe = returnTpe;
    }

    public abstract List<String> generate(String name, String owner, Map<String, Integer> builtinTypesParameterCounts);

    /**
     * Generate node's `execute` method definition (return type and necessary parameters).
     * '
     * @param owner owner of the method
     * @return string representation of the `execute` method's definition
     */
    protected String methodSigDef(String owner, List<MethodParameter> params, boolean isAbstract) {
        int paramsLen = params.size();
        String paramsDef;
        if (params.isEmpty()) {
            paramsDef = "";
        } else {
            paramsDef =
                    ", "
                            + StringUtils.join(params.stream().flatMap(x -> x.declaredParameters(expandVararg(paramsLen, x.index()))).toArray(), ", ");
        }
        String abstractModifier = isAbstract ? "abstract " : "";
        String thisParamTpe = isStatic || isConstructor || isAbstract ? "Object" : owner;
        return abstractModifier + targetReturnType(returnTpe) + " execute(" + thisParamTpe + " _this" + paramsDef + ")" + (isAbstract ? ";" : "");
    }

    /**
     * Infers the correct return type from the method signature
     * @return String representation of the type to use
     */
    protected String targetReturnType(TypeWithKind tpe) {
        if (isConstructor) return "Object";
        else {
            switch (tpe.kind()) {
                case OBJECT:
                    if (tpe.isValidGuestType()) {
                        return tpe.baseType();
                    } else {
                        if (!convertToGuestValue) {
                            throw new RuntimeException(
                                    "If intended, automatic conversion of value of type " + tpe.baseType()
                                            + " to guest value requires explicit '@Builtin.ReturningGuestObject' annotation");
                        }
                        return "Object";
                    }
                default:
                    return "Object";
            }
        }
    }

    /**
     * Convert annotated method's variable to MethodParameter
     * @param i position of the variable representing the parameter
     * @param v variable element representing the parameter
     * @return MethodParameter encapsulating the method's parameter info
     */
    protected MethodParameter fromVariableElementToMethodParameter(int i, VariableElement v) {
        return new MethodParameter(i, v.getSimpleName().toString(), v.asType().toString(),
                v.getAnnotationMirrors().stream().map(am -> am.toString()).collect(Collectors.toList()));
    }

    protected abstract Optional<Integer> expandVararg(int paramsLen, int paramIndex);
}
