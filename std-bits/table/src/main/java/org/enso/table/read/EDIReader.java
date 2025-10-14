package org.enso.table.read;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class EDIReader {
  public record EDISegment(String name, List<String> values) {
    public String toJson() {
      var vals =
          values.stream()
              .map(v -> "\"" + v.replace("\"", "\\\"") + "\"")
              .collect(Collectors.joining(", "));
      return "[\"" + name + "\", [" + vals + "]]";
    }
  }

  public static HashMap<String, Object> parse(List<EDISegment> data) {
    EDIField output = new EDIField.Dictionary("", new HashMap<>());

    var path = "";
    var current = output;

    for (EDISegment segment : data) {
      var name = segment.name();

      var newPath = enterLoop(path, name);
      if (newPath != null) {
        if (newPath.isEmpty()) {
          current = output;
          path = "";
        } else {
          var parts = newPath.split("/");
          current = output;
          for (var part : parts) {
            // Walk into the structure, creating as we go
            current = current.getKey(part);
          }
          path = newPath;
        }
      }

      var vals = segment.values();
      var dict =
          IntStream.range(0, vals.size())
              .mapToObj(i -> new EDIField.Value(name + " " + (i + 1), vals.get(i)))
              .filter(v -> v.value != null && !v.value.isEmpty())
              .collect(Collectors.toMap(EDIField::name, v -> (EDIField)v));
      var segmentField = new EDIField.Dictionary(name, dict);

      if (name.equals(current.name())) {
        current.append(segmentField);
      } else {
        // Append in as a key
        current.appendKey(name, segmentField);
      }
    }

    @SuppressWarnings("unchecked")
    var result = (HashMap<String, Object>) output.value();
    return result;
  }

  private static String enterLoop(String path, String key) {
    return switch (key) {
      case "ISA" -> "ISA"; // Start of interchange
      case "IEA" -> "ISA/IEA"; // End of interchange, reset to root
      case "GS" -> "ISA/GS"; // Start of group
      case "GE" -> "ISA/GE"; // End of group, reset to root
      case "ST" -> "ISA/ST"; // Start of message
      case "SE" -> "ISA/SE"; // End of message, reset to root
      case "ENT" -> "ISA/ENT";
      case "N1" -> path.endsWith("N1") ? path : (path.equals("ISA/ENT") ? "ISA/ENT/N1" : "ISA/N1");
      case "ACT" -> "ISA/ENT/ACT";
      case "RTE" -> path.endsWith("RTE") ? path : (path.equals("ISA/ENT/ACT") ? "ISA/ENT/ACT/RTE" : "ISA/RTE");
      case "LX" -> "ISA/ENT/ACT/LX";
      case "SER" -> "ISA/ENT/ACT/SER";
      default -> null;
    };
  }

  private sealed interface EDIField permits EDIField.Value, EDIField.Array, EDIField.Dictionary {
    String name();

    default EDIField append(EDIField field) {
      throw new UnsupportedOperationException();
    }

    default EDIField getKey(String key) {
      throw new UnsupportedOperationException();
    }

    default void appendKey(String key, EDIField value) {
      throw new UnsupportedOperationException();
    }

    Object value();

    record Value(String name, String value) implements EDIField {}

    record Dictionary(String name, Map<String, EDIField> fields) implements EDIField {
      @Override
      public EDIField append(EDIField field) {
        var result = new Array(name, new ArrayList<>());
        result.append(this);
        result.append(field);
        return result;
      }

      @Override
      public EDIField getKey(String key) {
        return fields.computeIfAbsent(key, k -> new Array(key, new ArrayList<>()));
      }

      public void appendKey(String key, EDIField field) {
        if (fields.containsKey(key)) {
          fields.compute(key, (k, v) -> v.append(field));
        } else {
          fields.put(key, field);
        }
      }

      @Override
      public Object value() {
        return fields.entrySet().stream()
            .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().value()));
      }
    }

    record Array(String name, List<EDIField> fields) implements EDIField {
      @Override
      public String name() {
        return name;
      }

      @Override
      public EDIField append(EDIField field) {
        fields.add(field);
        return this;
      }

      @Override
      public void appendKey(String key, EDIField value) {
        if (fields.isEmpty()) {
          throw new IllegalArgumentException("Cannot append an empty array");
        }
        var last = fields.get(fields.size() - 1);
        last.appendKey(key, value);
      }

      @Override
      public EDIField getKey(String key) {
        if (fields.isEmpty()) {
          throw new IllegalArgumentException("Cannot append an empty array");
        }
        var last = fields.get(fields.size() - 1);
        return last.getKey(key);
      }

      @Override
      public Object value() {
        return fields.stream().map(EDIField::value).collect(Collectors.toList());
      }
    }
  }

  /**
   * A representation of the structure of an EDI message, parsed from a string definition.
   *
   * <p>For example, the definition "[ISA,GS,[ST,SE],GE,IEA]" represents a message with an ISA
   * segment containing a GS segment, which contains multiple ST segments (each ending with an SE),
   * followed by GE and IEA segments.
   */
  static class EDIStructure {
    static EDIStructure parse(String definition) {
      // Parse the structure definition into a tree of EDIStructure
      return innerParse(definition, 0, null);
    }

    private static EDIStructure innerParse(String definition, int start, EDIStructure parent) {
      // Parse from the start index, returning the structure and the end index
      if (definition.charAt(start) == '[') {
        // Array
        var name = findName(definition, start + 1);
        var array = new EDIStructure(name, true, false, parent);
        parseChildren(definition, start, name, array);
        return array;
      } else if (definition.charAt(start) == '{') {
        // Object
        var name = findName(definition, start + 1);
        var structure = new EDIStructure(name, false, true, parent);
        parseChildren(definition, start, name, structure);
        return structure;
      } else {
        // Field
        var name = findName(definition, start);
        return new EDIStructure(name, false, false, parent);
      }
    }

    private static void parseChildren(String definition, int start, String name, EDIStructure parent) {
      var current = start + 1 + name.length();
      while (definition.charAt(current) == ',') {
        var child = innerParse(definition, current + 1, parent);
        parent.addField(child);
        current += 1 + child.charLength();
      }
    }

    private static String findName(String definition, int start) {
      var current = definition.charAt(start);
      int end = start + 1;
      while (current != ',' && current != ']' && current != '}' && end < definition.length()) {
        current = definition.charAt(end);
        end++;
      }

      if (current == ',' || current == ']' || current == '}') {
        end--;
      }

      return definition.substring(start, end);
    }

    private final String name;
    private final boolean isArray;
    private final boolean isObject;
    private final EDIStructure parent;
    private final List<String> fieldOrder;
    private final Map<String, EDIStructure> fields;

    EDIStructure(String name, boolean isArray, boolean isObject, EDIStructure parent) {
      this.name = name;
      this.isArray = isArray;
      this.isObject = isObject;
      this.parent = parent;
      this.fieldOrder = new ArrayList<>();
      this.fieldOrder.add(name);
      this.fields = new HashMap<>();
    }

    public String name() {
      return name;
    }

    public boolean isArray() {
      return isArray;
    }

    public boolean isObject() {
      return isObject;
    }

    public EDIStructure parent() {
      return parent;
    }

    public List<String> fieldOrder() {
      return fieldOrder;
    }

    public EDIStructure child(String name) {
      if (name.equals(this.name())) {
        // Mock child of self
        return new EDIStructure(name, false, false, parent);
      }

      return fields.get(name);
    }

    private void addField(EDIStructure field) {
      if (fields.containsKey(field.name())) {
        throw new IllegalArgumentException("Duplicate field name: " + field.name());
      }

      fields.put(field.name(), field);
      fieldOrder.add(field.name());
    }

    int charLength() {
      // Field Length
      var fieldLength = fieldOrder.stream().reduce(1, (a, b) -> a + (b.equals(name) ? name.length() : fields.get(b).charLength()) + 1, Integer::sum);
      return fieldLength - (isArray || isObject ? 0 : 2);
    }

    @Override
    public String toString() {
      var body = fieldOrder.stream().map(f -> f.equals(name) ? name : fields.get(f).toString()).collect(Collectors.joining(","));
      if (isArray) {
        return "[" + body + "]";
      } else if (isObject) {
        return "{" + body + "}";
      } else {
        return body;
      }
    }
  }
}
