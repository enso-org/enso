package org.enso.runtime.parser.processor.methodgen;

import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.field.ListField;
import org.enso.runtime.parser.processor.field.OptionField;
import org.enso.runtime.parser.processor.field.OptionListField;
import org.enso.runtime.parser.processor.field.PersistanceReferenceField;
import org.enso.runtime.parser.processor.utils.Utils;

public final class ChildrenMethodGenerator {
  private final GeneratedClassContext ctx;

  public ChildrenMethodGenerator(GeneratedClassContext ctx) {
    this.ctx = ctx;
  }

  public String generateCode() {
    var sb = new StringBuilder();
    sb.append("@Override").append(System.lineSeparator());
    sb.append("public scala.collection.immutable.List<IR> children() {")
        .append(System.lineSeparator());
    sb.append("  var list = new java.util.ArrayList<IR>();").append(System.lineSeparator());

    ctx.getUserFields().stream()
        .filter(Field::isChild)
        .forEach(
            userField -> {
              String addToListCode =
                  switch (userField) {
                    case ListField listField -> addListCode(listField);
                    case OptionField optionField -> addOptionCode(optionField);
                    case OptionListField optionListField -> addOptionListCode(optionListField);
                    case PersistanceReferenceField persistanceReferenceField ->
                        addPersistanceRefCode(persistanceReferenceField);
                    default -> addFieldCode(userField);
                  };
              sb.append(Utils.indent(addToListCode));
              sb.append(System.lineSeparator());
            });

    sb.append("  return scala.jdk.javaapi.CollectionConverters.asScala(list).toList();")
        .append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  private String addListCode(ListField listField) {
    if (listField.isNullable()) {
      var code =
          """
          if (${fieldName} != null) {
            ${fieldName}.foreach(list::add);
          }
          """
              .replace("${fieldName}", listField.getName());
      return code;
    } else {
      return listField.getName() + ".foreach(list::add);";
    }
  }

  private String addOptionCode(OptionField field) {
    var code =
        """
        if (${fieldName}.isDefined()) {
          list.add(${fieldName}.get());
        }
        """
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String addOptionListCode(OptionListField field) {
    var code =
        """
        if (${fieldName}.isDefined()) {
          ${fieldName}.get().foreach(list::add);
        }
        """
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String addPersistanceRefCode(PersistanceReferenceField field) {
    var code =
        """
        list.add(${fieldName}.get(${type}.class));
        """
            .replace("${fieldName}", field.getName())
            .replace("${type}", field.getTypeParameter().getQualifiedName().toString());
    return code;
  }

  private String addFieldCode(Field field) {
    Utils.hardAssert(!(field instanceof ListField));
    Utils.hardAssert(!(field instanceof OptionField));
    Utils.hardAssert(!(field instanceof OptionListField));
    Utils.hardAssert(!(field instanceof PersistanceReferenceField));
    if (field.isNullable()) {
      return """
      if (${fieldName} != null) {
        list.add(${fieldName});
      }
      """
          .replace("${fieldName}", field.getName());
    } else {
      return "list.add(" + field.getName() + ");";
    }
  }
}
