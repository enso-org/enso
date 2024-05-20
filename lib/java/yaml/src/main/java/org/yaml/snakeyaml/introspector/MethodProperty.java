/**
 * Copyright (c) 2008, SnakeYAML
 *
 * <p>Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * <p>http://www.apache.org/licenses/LICENSE-2.0
 *
 * <p>Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.yaml.snakeyaml.introspector;

// remove once https://github.com/snakeyaml/snakeyaml/pull/12 gets integrated
import java.beans.FeatureDescriptor;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import org.yaml.snakeyaml.error.YAMLException;
import org.yaml.snakeyaml.util.ArrayUtils;

/**
 * A <code>MethodProperty</code> is a <code>Property</code> which is accessed through accessor
 * methods (setX, getX). It is possible to have a <code>MethodProperty</code> which has only setter,
 * only getter, or both. It is not possible to have a <code>MethodProperty</code> which has neither
 * setter nor getter.
 */
public class MethodProperty extends GenericProperty {

  private final PropertyDescriptor property;
  private final boolean readable;
  private final boolean writable;

  private static Type discoverGenericType(PropertyDescriptor property) {
    Method readMethod = property.getReadMethod();
    if (readMethod != null) {
      return readMethod.getGenericReturnType();
    }

    Method writeMethod = property.getWriteMethod();
    if (writeMethod != null) {
      Type[] paramTypes = writeMethod.getGenericParameterTypes();
      if (paramTypes.length > 0) {
        return paramTypes[0];
      }
    }
    /*
     * This actually may happen if PropertyDescriptor is of type IndexedPropertyDescriptor and it
     * has only IndexedGetter/Setter. ATM we simply skip type discovery.
     */
    return null;
  }

  public MethodProperty(PropertyDescriptor property) {
    super(
        property.getName(),
        property.getPropertyType(),
        MethodProperty.discoverGenericType(property));
    this.property = property;
    this.readable = property.getReadMethod() != null;
    this.writable = property.getWriteMethod() != null;
  }

  @Override
  public void set(Object object, Object value) throws Exception {
    if (!writable) {
      throw new YAMLException(
          "No writable property '" + getName() + "' on class: " + object.getClass().getName());
    }
    property.getWriteMethod().invoke(object, value);
  }

  @Override
  public Object get(Object object) {
    try {
      property.getReadMethod().setAccessible(true); // issue 50
      return property.getReadMethod().invoke(object);
    } catch (Exception e) {
      throw new YAMLException(
          "Unable to find getter for property '"
              + property.getName()
              + "' on object "
              + object
              + ":"
              + e);
    }
  }

  /**
   * Returns the annotations that are present on read and write methods of this property or empty
   * {@code List} if there're no annotations.
   *
   * @return the annotations that are present on this property or empty {@code List} if there're no
   *     annotations
   */
  @Override
  public List<Annotation> getAnnotations() {
    List<Annotation> annotations;
    if (isReadable() && isWritable()) {
      annotations =
          ArrayUtils.toUnmodifiableCompositeList(
              property.getReadMethod().getAnnotations(),
              property.getWriteMethod().getAnnotations());
    } else if (isReadable()) {
      annotations = ArrayUtils.toUnmodifiableList(property.getReadMethod().getAnnotations());
    } else {
      annotations = ArrayUtils.toUnmodifiableList(property.getWriteMethod().getAnnotations());
    }
    return annotations;
  }

  /**
   * Returns property's annotation for the given type or {@code null} if it's not present. If the
   * annotation is present on both read and write methods, the annotation on read method takes
   * precedence.
   *
   * @param annotationType the type of the annotation to be returned
   * @return property's annotation for the given type or {@code null} if it's not present
   */
  @Override
  public <A extends Annotation> A getAnnotation(Class<A> annotationType) {
    A annotation = null;
    if (isReadable()) {
      annotation = property.getReadMethod().getAnnotation(annotationType);
    }
    if (annotation == null && isWritable()) {
      annotation = property.getWriteMethod().getAnnotation(annotationType);
    }
    return annotation;
  }

  @Override
  public boolean isWritable() {
    return writable;
  }

  @Override
  public boolean isReadable() {
    return readable;
  }

  private static final String TRANSIENT = "transient";

  private static boolean isTransient(FeatureDescriptor fd) {
    return Boolean.TRUE.equals(fd.getValue(TRANSIENT));
  }

  static void addProperties(
      Class<?> type, Map<String, Property> properties, boolean[] inaccessableFieldsExist) {
    try {
      for (PropertyDescriptor property : Introspector.getBeanInfo(type).getPropertyDescriptors()) {
        Method readMethod = property.getReadMethod();
        if ((readMethod == null || !readMethod.getName().equals("getClass"))
            && !isTransient(property)) {
          properties.put(property.getName(), new MethodProperty(property));
        }
      }
    } catch (IntrospectionException e) {
      throw new YAMLException(e);
    }

    // add public fields
    for (Class<?> c = type; c != null; c = c.getSuperclass()) {
      for (Field field : c.getDeclaredFields()) {
        int modifiers = field.getModifiers();
        if (!Modifier.isStatic(modifiers) && !Modifier.isTransient(modifiers)) {
          if (Modifier.isPublic(modifiers)) {
            properties.put(field.getName(), new FieldProperty(field));
          } else {
            inaccessableFieldsExist[0] = true;
          }
        }
      }
    }
  }
}
