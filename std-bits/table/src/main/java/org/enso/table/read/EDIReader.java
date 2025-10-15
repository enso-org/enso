package org.enso.table.read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class EDIReader {
  /**
   * Parses EDI segments from a text input.
   *
   * @param text the EDI text input
   * @param separator the segment separator (e.g., "~", "\n", "\r\n"). If empty, the method will
   *     attempt to auto-detect it.
   * @return a list of segments, where each segment is represented as an array of strings
   */
  public static List<String[]> parseSegments(String text, String separator) {
    // Strip new lines and carriage returns
    if (!separator.equals("\r\n")) {
      text = separator.equals("\n") ? text : text.replace("\n", "");
      text = separator.equals("\r") ? text : text.replace("\r", "");
    }

    // Find the segment terminator
    if (separator.isEmpty()) {
      var index = text.indexOf("GS", 1);
      separator = index == -1 ? "~" : text.substring(index - 1, index);
    }

    var escaped = Pattern.quote(separator);
    var segments = text.split(escaped);
    return Arrays.stream(segments)
        .filter(s -> s != null && !s.trim().isEmpty())
        .map(s -> s.trim().split("\\s*\\*"))
        .collect(Collectors.toList());
  }

  /**
   * Parses EDI text into a structured object based on the provided structure definition and field
   * mappings.
   *
   * @param text the EDI text input
   * @param separator the segment separator (e.g., "~", "\n", "\r\n"). If empty, the method will
   *     attempt to auto-detect it.
   * @param structureDef the structure definition string (e.g., "[ISA,GS,[ST,SE],GE,IEA]")
   * @param fieldMappings a map of segment names to their corresponding field names
   * @return a structured object representing the parsed EDI data
   */
  public static Object parse(
      String text, String separator, String structureDef, Map<String, List<String>> fieldMappings) {
    var data = parseSegments(text, separator);
    var structure = EDIStructure.parse(structureDef);

    EDIField output =
        structure.isArray
            ? new EDIField.Array(structure.name, new ArrayList<>(), null)
            : new EDIField.Dictionary("", new HashMap<>(), null);

    var level = structure;
    var current = output;

    int segmentIndex = -1;
    for (String[] segment : data) {
      segmentIndex++;
      var name = segment[0];

      // Make a new segment
      var mapping = fieldMappings.getOrDefault(name, List.of());
      var dict =
          IntStream.range(1, segment.length)
              .filter(i -> segment[i] != null && !segment[i].isEmpty())
              .mapToObj(
                  i -> {
                    var fieldName = i - 1 < mapping.size() ? mapping.get(i - 1) : name + "-" + i;
                    return (EDIField) new EDIField.Value(fieldName, segment[i]);
                  })
              .collect(Collectors.toMap(EDIField::name, v -> v));

      // We are in an array of this segment type, so just append
      if (current.name().equals(name)) {
        if (current instanceof EDIField.Array) {
          var segmentField = new EDIField.Dictionary(name, dict, current);
          current.append(segmentField);
        } else {
          var path = name;
          var parent = current;
          while (parent != null) {
            path = parent.name() + "/" + path;
            parent = parent.parent();
          }
          throw new IllegalArgumentException(
              name + " is not an array (segment=" + segmentIndex + ") at " + path);
        }
      } else {
        // See if we can find the segment in the current level
        var child = level.child(name);

        // If not found, lets search in parent's.
        var oldLevel = level;
        var oldCurrent = current;
        while (child == null && level.parent() != null) {
          level = level.parent();
          current = current.parent();
          if (level.name.equals(name)) {
            child = level;
            current = current.parent();
          } else {
            child = level.child(name);
          }
        }

        // If we still didn't find it, report an error
        if (child == null) {
          var path = name;
          var parent = oldCurrent;
          while (parent != null) {
            path = parent.name() + "/" + path;
            parent = parent.parent();
          }
          throw new IllegalArgumentException(
              name + " could not be placed in structure (segment=" + segmentIndex + ") at " + path);
        }

        // Append Child to current
        EDIField segmentField = new EDIField.Dictionary(name, dict, current);
        if (child.isArray()) {
          if (current.getKey(name) == null) {
            segmentField =
                new EDIField.Array(
                    name,
                    new ArrayList<>(List.of(new EDIField.Dictionary(name, dict, current))),
                    current);
            current.appendKey(name, segmentField);
          } else {
            current.getKey(name).append(segmentField);
            segmentField = current.getKey(name);
          }
        } else {
          current.appendKey(name, segmentField);
        }

        if (child.isArray() || child.isObject()) {
          level = child;
          current = segmentField;
        }
      }
    }

    return output.value();
  }

  public sealed interface EDIField permits EDIField.Value, EDIField.Array, EDIField.Dictionary {
    String name();

    default EDIField parent() {
      return null;
    }

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

    record Value(String name, String value) implements EDIField {
      @Override
      public String toString() {
        return "Value{" + name + ':' + value + '}';
      }
    }

    record Dictionary(String name, Map<String, EDIField> fields, EDIField parent)
        implements EDIField {
      @Override
      public EDIField append(EDIField field) {
        var result = new Array(name, new ArrayList<>(), parent);
        result.append(this);
        result.append(field);
        return result;
      }

      @Override
      public EDIField getKey(String key) {
        return fields.getOrDefault(key, null);
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

      @Override
      public String toString() {
        return "Dictionary{" + name + ':' + fields.values() + '}';
      }
    }

    record Array(String name, List<EDIField> fields, EDIField parent) implements EDIField {
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
          return null;
        }
        var last = fields.get(fields.size() - 1);
        return last.getKey(key);
      }

      @Override
      public Object value() {
        return fields.stream().map(EDIField::value).collect(Collectors.toList());
      }

      @Override
      public String toString() {
        return "Array{" + name + ':' + fields + '}';
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

    private static void parseChildren(
        String definition, int start, String name, EDIStructure parent) {
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
      var fieldLength =
          fieldOrder.stream()
              .reduce(
                  1,
                  (a, b) -> a + (b.equals(name) ? name.length() : fields.get(b).charLength()) + 1,
                  Integer::sum);
      return fieldLength - (isArray || isObject ? 0 : 2);
    }

    @Override
    public String toString() {
      var body =
          fieldOrder.stream()
              .map(f -> f.equals(name) ? name : fields.get(f).toString())
              .collect(Collectors.joining(","));
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
