package org.enso.runtime.parser.processor.field;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import org.enso.runtime.parser.processor.IRProcessingException;
import org.enso.runtime.parser.processor.ProcessedClass;
import org.enso.runtime.parser.processor.utils.Utils;

/**
 * Collects abstract parameterless methods from the given interface and all its superinterfaces -
 * these will be represented as fields in the generated classes, hence the name.
 */
public final class FieldCollector {
  private final ProcessingEnvironment processingEnv;
  private final ProcessedClass processedClass;
  private final TypeElement metadataStorageType;
  private final TypeElement diagnosticStorageType;
  private final TypeElement identifiedLocationType;
  private final TypeElement uuidType;

  // Mapped by field name
  private Map<String, Field> fields;

  public FieldCollector(ProcessingEnvironment processingEnv, ProcessedClass processedClass) {
    this.processingEnv = processingEnv;
    this.processedClass = processedClass;
    this.metadataStorageType = Utils.metadataStorageTypeElement(processingEnv);
    this.diagnosticStorageType = Utils.diagnosticStorageTypeElement(processingEnv);
    this.identifiedLocationType = Utils.identifiedLocationTypeElement(processingEnv);
    this.uuidType = Utils.uuidTypeElement(processingEnv);
  }

  public List<Field> collectFields() {
    if (fields == null) {
      fields = new LinkedHashMap<>();
      collectFromCtor();
    }
    return fields.values().stream().toList();
  }

  private void collectFromCtor() {
    var ctor = processedClass.getCtor();
    for (var param : ctor.getParameters()) {
      var paramName = param.getSimpleName().toString();
      var irFieldAnnot = param.getAnnotation(IRField.class);
      var irChildAnnot = param.getAnnotation(IRChild.class);
      Field field;
      if (irFieldAnnot != null) {
        field = processIrField(param, irFieldAnnot);
      } else if (irChildAnnot != null) {
        field = processIrChild(param, irChildAnnot);
      } else if (Utils.hasNoAnnotations(param) && isMeta(param)) {
        field = null;
      } else {
        var errMsg =
            "Constructor parameter "
                + param
                + " must be annotated with either @IRField or @IRChild";
        throw new IRProcessingException(errMsg, param);
      }

      if (field != null) {
        fields.put(paramName, field);
      }
    }
  }

  private boolean isMeta(VariableElement param) {
    var typeUtils = processingEnv.getTypeUtils();
    return typeUtils.isSameType(param.asType(), metadataStorageType.asType())
        || typeUtils.isSameType(param.asType(), diagnosticStorageType.asType())
        || typeUtils.isSameType(param.asType(), identifiedLocationType.asType())
        || typeUtils.isSameType(param.asType(), uuidType.asType());
  }

  private Field processIrField(VariableElement param, IRField irFieldAnnot) {
    var isNullable = !irFieldAnnot.required();
    var name = param.getSimpleName().toString();
    if (isPrimitiveType(param)) {
      return new PrimitiveField(param.asType(), name, processingEnv);
    } else {
      // TODO: Assert that type is simple reference type - does not extend IR, is not generic
      return new ReferenceField(processingEnv, param.asType(), name, isNullable, false);
    }
  }

  private Field processIrChild(VariableElement param, IRChild irChildAnnot) {
    var name = param.getSimpleName().toString();
    var type = getParamType(param);
    var isNullable = !irChildAnnot.required();
    if (Utils.isScalaList(param.asType(), processingEnv)) {
      ensureTypeArgIsSubtypeOfIR(param.asType());
      return new ListField(name, param.asType(), processingEnv);
    } else if (Utils.isScalaOption(param.asType(), processingEnv)) {
      ensureTypeArgIsSubtypeOfIR(param.asType());
      return new OptionField(name, param.asType(), processingEnv);
    } else {
      if (!Utils.isSubtypeOfIR(type, processingEnv)) {
        throw new IRProcessingException(
            "Constructor parameter annotated with @IRChild must be a subtype of IR interface. "
                + "Actual type is: "
                + type,
            param);
      }
      return new ReferenceField(processingEnv, param.asType(), name, isNullable, true);
    }
  }

  private void ensureTypeArgIsSubtypeOfIR(TypeMirror typeMirror) {
    var declaredType = (DeclaredType) typeMirror;
    Utils.hardAssert(declaredType.getTypeArguments().size() == 1);
    var typeArg = declaredType.getTypeArguments().get(0);
    var typeArgElem = (TypeElement) processingEnv.getTypeUtils().asElement(typeArg);
    ensureIsSubtypeOfIR(typeArgElem);
  }

  private static boolean isPrimitiveType(VariableElement ctorParam) {
    return ctorParam.asType().getKind().isPrimitive();
  }

  private TypeElement getParamType(VariableElement param) {
    return (TypeElement) processingEnv.getTypeUtils().asElement(param.asType());
  }

  private void ensureIsSubtypeOfIR(TypeElement typeElem) {
    if (!Utils.isSubtypeOfIR(typeElem, processingEnv)) {
      throw new IRProcessingException(
          "Method annotated with @IRChild must return a subtype of IR interface", typeElem);
    }
  }
}
