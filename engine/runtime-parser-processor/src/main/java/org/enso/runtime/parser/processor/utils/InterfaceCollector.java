package org.enso.runtime.parser.processor.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;

final class InterfaceCollector {
  private InterfaceCollector() {}

  /**
   * Collects all the interfaces from the whole type hierarchy.
   *
   * @param type Starting type.
   * @return Does not return duplicates.
   */
  static Set<? extends TypeMirror> collect(TypeElement type, Types typeUtils) {
    TypeElement currentType = type;
    var bldr = new Builder();
    while (currentType != null) {
      var directIfaces = currentType.getInterfaces();
      for (var iface : directIfaces) {
        bldr.addInterface(iface);
      }
      var superType = currentType.getSuperclass();
      if (superType.getKind() == TypeKind.NONE) {
        break;
      }
      var superTypeElem = typeUtils.asElement(superType);
      if (superTypeElem instanceof TypeElement typeEl) {
        currentType = typeEl;
      } else {
        currentType = null;
      }
    }
    return bldr.getInterfaces();
  }

  private static final class Builder {
    private final Map<String, TypeMirror> ifaces = new HashMap<>();

    private Set<? extends TypeMirror> getInterfaces() {
      return ifaces.values().stream().collect(Collectors.toUnmodifiableSet());
    }

    private void addInterface(TypeMirror iface) {
      var name = iface.toString();
      ifaces.put(name, iface);
    }
  }
}
