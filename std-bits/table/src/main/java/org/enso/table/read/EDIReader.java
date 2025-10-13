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
      case "IEA" -> ""; // End of interchange, reset to root
      case "GE" -> ""; // End of group, reset to root
      case "SE" -> ""; // End of message, reset to root
      case "ENT" -> "ENT";
      case "N1" -> path.endsWith("N1") ? path : path + "/N1";
      case "ACT" -> "ENT/ACT";
      case "RTE" -> path.endsWith("RTE") ? path : (path.endsWith("ACT") ? path + "/RTE" : "RTE");
      case "LX" -> "ENT/ACT/LX";
      case "SER" -> "ENT/ACT/SER";
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
}
