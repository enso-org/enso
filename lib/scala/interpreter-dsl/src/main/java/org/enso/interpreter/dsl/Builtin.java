package org.enso.interpreter.dsl;

import java.lang.annotation.*;

/**
 * A class annotated with Builtin will auto-generate a corresponding {@link BuiltinType} known to
 * the compiler and can be marked as @Builtin_Type in Enso standard library.
 *
 * <p>Consider annotating class {@link Foo}</p>
 *
 * <pre>
 * {@link Builtin @Builtin}(pkg = "example", stdLibName = "Standard.Base.Data.Foo")
 * public class Foo extends {@link TruffleObject} {
 *  // ...
 * }
 * </pre>
 *
 * The processor will generate the following class
 *
 * <pre>
 * package org.enso.interpreter.node.expression.builtin.example;
 *
 * {@link BuiltinType @BuiltinType}(name = "Standard.Base.Data.Foo")
 * public class Foo extends Builtin {}
 * </pre>
 *
 * that refers to Foo builtin type present in Standard Library under the {@link Standard.Base.Data.Foo}
 * <pre>
 * type Foo
 *     {@link @Builtin_Type}
 *     type Foo
 * </pr>
 *
 * By default, all generated classes are being located in {@link org.enso.interpreter.node.expression.builtin} package.
 * <p>A {@link Builtin#pkg()} element allows to further customize the subpackage.
 * Class for @BuiltinType {@link Foo} and of its methods will be located in {@link
 * org.enso.interpreter.node.expression.builtin.example}.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface Builtin {
  /** @return the name of the subpackage for the generated method node. */
  String pkg() default "";

  /** @return A fully qualified name of the corresponding Enso type in standard library. */
  String stdlibName() default "";

  /** @return A custom name of the builtin type * */
  String name() default "";

  /**
   * A method (or constructor) annotated with {@link Builtin.Method} will generate a base
   * implementation for {@link BuiltinMethod}. {@link com.oracle.truffle.api.nodes.Node} class.
   *
   * <p>In its simplest form, annotating a simple non-overloaded with the annotation will generate a
   * concrete class with a single `execute` method, inside which we call the annotated method.
   *
   * <p>Consider the following class and its to-be builtin methods
   *
   * <pre>
   * public class Foo extends {@link TruffleObject} {
   *    // ...
   *    {@link Builtin.Method @Builtin.Method}
   *    public long length() {
   *      // ...
   *    }
   *
   *    {@link Builtin.Method @Builtin.Method}
   *    public static Foo empty(Object element) {
   *        // ...
   *    }
   * }
   * </pre>
   *
   * The annotation processing will generate the corresponding classes
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "length")
   * public class LengthFooNode extends Node {
   *
   *   long execute(Array _this) {
   *     return _this.length();
   *   }
   *
   * }
   * </pre>
   *
   * and
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "length")
   * public class EmptyFooNode extends Node {
   *
   *   Foo execute(Foo _this) {
   *     return Foo.empty();
   *   }
   *
   * }
   * </pre>
   *
   * The generated `execute` method ensures that a correct number of method parameters is captured,
   * while taking into account static modifier and return type of the method, including {@link void}
   * type.
   *
   * <p>Overloaded methods, representing different specializations of a single Builtin method have
   * to be additionally annotated with {@link Builtin.Specialize}, or a compiler error will be
   * reported about generated duplicate classes. Similarly, methods that use parameter of type
   * {@link org.enso.interpreter.runtime.Context} or parameters with one of Truffle's {@link
   * com.oracle.truffle.api.dsl DSL} or Enso's {@link org.enso.interpreter.dsl DSL} must also be
   * marked with {@link Builtin.Specialize}.
   */
  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @interface Method {
    /**
     * @return a custom name for the generated builting method. By default, it uses the name of the
     *     annotated method.
     */
    String name() default "";

    /** @return a short description for the generated builtin method. */
    String description() default "";

    /**
     * @return When applied to a method/constructor with varargs, indicates how many times vararg
     *     parameter should be expanded and implicitly how many builtin nodes should be generated.
     *     Must be zero when there are no varaargs in the parameters list.
     */
    int expandVarargs() default 0;
  }

  /**
   * Annotation indicating that compile- or run-time exception the method can throw should be
   * wrapped in Enso's error type that will be reported to the user. The annotation can be repeated
   * leading to multiple catch clauses.
   *
   * <p>Consider simple method with possible runtime exception:
   *
   * <pre>
   * class Foo {
   *   private Object[] items;
   *   {@link Builtin.Method @Builtin.Method}
   *   {@link Builtin.WrapException @Builtin.WrapException}(from=IndexOutOfBounds.class, to=InvalidArrayIndexError.class)
   *   public Object get(long index) {
   *     return items[index];
   *   }
   *
   * {@link Builtin.Method @Builtin.Method}
   * {@link Builtin.WrapException @Builtin.WrapException}(from=IOException.class, to=PolyglotError.class, propagate=true)
   *   public Object create(String path) throws java.io.IOException {
   *       // ...
   *   }
   * }
   * </pre>
   *
   * Given the information from {@link WrapException#from()} and {@link WrapException#to()}
   * elements, the processor knows that the call to {@link get()} has to be wrapped in try/catch,
   * catching and re-throwing values of a particular type, respectively. The processor will
   * automatically infer what parameters need to be passed using the information from {@link
   * WrapException#to()} class.
   *
   * <p>The generated class will therefore ensure that calls to methods are safe and wrapped in an
   * appropriate catch-clause:
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "get")
   * public class GetFooNode extends Node {
   *   java.lang.Object execute(Foo _this, long index) {
   *     try {
   *       return _this.get(index);
   *     } catch (java.lang.IndexOutOfBoundsException e) {
   *       Builtins builtins = Context.get(this).getBuiltins();
   *       throw new PanicException(builtins.error().makeInvalidArrayIndexError(_this, index), this);
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>One can also simply propagate the exception by setting the {@link WrapException#propagate()}
   * to true:
   *
   * <pre>
   * class Foo {
   * {@link Builtin.Method @Builtin.Method}
   * {@link Builtin.WrapException @Builtin.WrapException}(from=IOException.class, to=PolyglotError.class, propagate=true)
   *   public Object create(Object path) throws java.io.IOException {
   *       // ...
   *   }
   * }
   * </pre>
   *
   * Propagation will simply re-throw the wrapped exception resulting in the following class:
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "create")
   * public class CreateFooNode extends Node {
   *
   * java.lang.Object execute(Foo _this, Object path) {
   *   try {
   *     return _this.create(path)
   *   } catch (java.io.Exception e) {
   *     Builtins builtins = Context.get(this).getBuiltins();
   *     throw new PanicException(builtins.error().makePolyglotError(e), this);
   *   }
   * }
   * </pre>
   */
  @Repeatable(WrapExceptions.class)
  @interface WrapException {
    /** @return Class of the potential exception to be caught during the execution of the method */
    Class<? extends Exception> from();
    /** @return Class of Enso's builtin (error) type */
    Class<?> to();

    /**
     * @return true, if only the original exception should be wrapped. Otherwise we pass provided
     *     method params.
     */
    boolean propagate() default false;
  }

  /**
   * Container for {@link WrapException} annotations. The annotation should not be used directly.
   * Instead use multiple {@link WrapException}:
   *
   * <pre>
   *     {@link Builtin.WrapException @Builtin.WrapException}(from=FooException.class, to=PolyglotError.class, propagate=true)
   *     {@link Builtin.WrapException @Builtin.WrapException}(from=BarException.class, to=PolyglotError.class, propagate=true)
   *     Object foo(Object item) {
   *         // ...
   *     }
   * </pre>
   */
  @interface WrapExceptions {
    WrapException[] value() default {};
  }

  /**
   * Annotation accepting `env.asGuestValue` translation done on the return object. The conversion
   * is generated automatically, depending on the type of the value.
   *
   * <p>Note that while explicit translations to interop value are discouraged, we still want to
   * occasionally support it to easy the builtins-writing process. The presence of the {@link
   * ReturningGuestObject} only ensures that it is intentional.
   *
   * <p>Consider a method returning an {@link java.io.OutputStream} which is not an interop value,
   * for the sake of the example:
   *
   * <pre>
   * class Foo {
   *     {@link Builtin.Method @Builtin.Method}
   *     {@link Builtin.ReturningGuestObject @Builtin.ReturningGuestObject}
   *     java.lang.OutputStream foo(Object item) {
   *         // ...
   *     }
   * }
   * </pre>
   *
   * The processor will detect the return type of method {@link foo} and perform an automatic
   * conversion:
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "create")
   * public class CreateFooNode extends Node {
   *   java.lang.Object execute(Foo _this, Object item) {
   *     return context
   *           .getEnvironment()
   *           .asGuestValue(_this.foo(item));
   *   }
   * }
   * </pre>
   *
   * Without the presence of the annotation, the processor would detect the potential value
   * requiring {@link com.oracle.truffle.api.TruffleLanguage#asGuestValue(Object)} translation but
   * crash since it didn't seem to be intended by the user.
   */
  @interface ReturningGuestObject {}

  /**
   * A Method marked with {@link Builtin.Specialize} annotation will generate specializations for
   * overloaded and non-overloaded methods. The annotation requires presence of {@link
   * Builtin.Method} as well.
   *
   * <p><b>Non-overloaded method scenario</b> Any method annotated with {@link
   * Builtin.Method @Builtin.Method} can also be specialized, resulting in an abstract class.
   * Additionally, specialized methods may use parameters that involve Truffle's {@link
   * com.oracle.truffle.api.dsl DSL}, Enso's {@link org.enso.interpreter.dsl DSL} or {@link
   * org.enso.interpreter.runtime.Context}.
   *
   * <p>Consider method Foo that makes use of the language's context and a String parameter. Notice
   * that Enso uses {@link org.enso.interpreter.runtime.data.text.Text} instead of String and one
   * should typically pass it through {@link
   * enso.interpreter.node.expression.builtin.text.util.ExpectStringNode} to retrieve its value.
   *
   * <pre>
   * class Foo extends TruffleObject {
   *     {@link Builtin.Method @Builtin.Method}
   *     {@link Builtin.Specialize @Builtin.Specialize}
   *     public static Foo create(Context context, String path) {
   *         // ...
   *     }
   * }
   * </pre>
   *
   * The processor detects types of parameters and generates a specialized node class accordingly:
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "create")
   * public abstract class CreateFooNode extends Node {
   *   static CreateFooNode build() {
   *     return CreateFooNodeGen.create();
   *   }
   *
   *   abstract Foo execute((Object _this, Object path);
   *
   *   {@link Specialization @Specialization}
   *   Foo doString(Object _this, Object path, @Cached("build()") ExpectStringNode expectStringNode) {
   *     Context context = Context.get(this);
   *     java.lang.String pathCached = expectStringNode.execute(path);
   *     return Foo.create(context, pathCached);
   *   }
   * }
   * </pre>
   *
   * Notice how the processor - infers the correct signature of {@link execute} method, omitting
   * unnecessary parameters from the original signature, and inferring the signature of the
   * specialized method that will be accepted by Truffle's DSL - injects {@link
   * org.enso.interpreter.runtime.Context} value
   *
   * <p>Parameters having type {@link org.enso.interpreter.runtime.error.WithWarnings} will
   * automatically add {@link org.enso.interpreter.dsl.AcceptsWarning} annotation to the
   * corresponding
   *
   * <p><b>Overloaded method scenario</b> The processor infers the parameter on which we specialize.
   * Overloaded methods can only differ by a single parameter's type at the moment and the inference
   * is based solely on the position of the parameter.
   *
   * <p>As an example consider overloaded method bar in class Foo:
   *
   * <pre>
   * class Foo extends TruffleObject {
   *     {@link Builtin.Method @Builtin.Method}
   *     {@link Builtin.Specialize @Builtin.Specialize}
   *     public Foo resolve(String subPath) {
   *         // ...
   *     }
   *
   *     {@link Builtin.Method @Builtin.Method}
   *     {@link Builtin.Specialize @Builtin.Specialize}
   *     public Foo resolve(Foo subPath) {
   *         // ...
   *     }
   * }
   * </pre>
   *
   * The processor infers the right order of specializations, takes into account the types of
   * parameters and infers a single specialized method Node class:
   *
   * <pre>
   * {@link BuiltinMethod @BuiltinMethod}(type = "Foo", name = "resolve", description = "")
   * public abstract class FooNode extends Node {
   *   static ResolveFooNode build() {
   *     return ResolveFooNodeGen.create();
   *   }
   *
   *   abstract Foo execute(Object _this, Object sub_path);
   *
   *   {@link Specilization @Specialization}
   *   Foo doFoo(Foo _this, Foo sub_path) {
   *     return _this.resolve(sub_path);
   *   }
   *   {@link Specilization @Specialization}
   *   Foo doString(Foo _this, Object sub_path, @Cached("build()") ExpectStringNode expectStringNode) {
   *     java.lang.String sub_pathCached = expectStringNode.execute(sub_path);
   *     return _this.resolve(sub_pathCached);
   *   }
   * }
   * </pre>
   *
   * Parameters having type {@link org.enso.interpreter.runtime.error.WithWarnings} will
   * automatically add {@link org.enso.interpreter.dsl.AcceptsWarning} annotation to the
   * corresponding parameter in {@link execute} method declaration.
   *
   * <p><b>Fallback specialization</b> If the {@link Specialize#fallback()} element is true, the
   * corresponding overloaded method will lead to generating a specialization with {@link
   * Fallback @Fallback} annotation instead of {@link Specilization @Specialization}.
   */
  @interface Specialize {
    /**
     * return true, when the generated specialization should be marked as the final fallback method
     */
    boolean fallback() default false;
  }
}
