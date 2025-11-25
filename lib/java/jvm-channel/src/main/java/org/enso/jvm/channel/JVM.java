package org.enso.jvm.channel;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.struct.SizeOf;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.WordFactory;

/** Represents a JVM inside of current process. */
public final class JVM {
  private final JNIBoot.JNICreateJavaVMPointer createJvmFn;
  private final String[] options;
  private final ThreadLocal<Long> threadEnvs = new ThreadLocal<>();

  /**
   * @GuardedBy("this")
   */
  private JNI.JavaVM javaVM = WordFactory.nullPointer();

  JVM(JNIBoot.JNICreateJavaVMPointer factory, String[] options) {
    this.createJvmFn = factory;
    this.options = options;
  }

  /**
   * Create new JVM. Either HotSpot JVM or natively compiled JVM. Use {@link #executeMain} to launch
   * a main method inside of the JVM. Use {@link Channel#create} to establish a channel to send more
   * intricate messages to the JVM.
   *
   * @param path path where the JDK is installed - either path to the HotSpot JVM directory or
   *     direct path to the dynamic library that contains the {@code JNI_CreateJavaVM} entry point
   * @param options parameters to pass to the JVM
   * @return new instance of the JVM
   */
  public static JVM create(File path, String... options) {
    JNIBoot.JNICreateJavaVMPointer createJvmFn;
    File libPath;
    if (path.isDirectory()) {
      // assume it is a root of `System.getProperty("java.home")`
      libPath =
          Platform.includedIn(Platform.WINDOWS.class)
              ? WindowsJVM.findDynamicLibrary(path)
              : PosixJVM.findDynamicLibrary(path);
    } else {
      libPath = path;
    }
    assert libPath.isFile();
    createJvmFn =
        Platform.includedIn(Platform.WINDOWS.class)
            ? WindowsJVM.loadImpl(libPath.getAbsolutePath())
            : PosixJVM.loadImpl(libPath.getAbsolutePath());

    var jvmArgs = new ArrayList<String>();

    // java.home
    jvmArgs.add("-Djava.home=" + path);

    jvmArgs.addAll(Arrays.asList(options));
    return new JVM(createJvmFn, jvmArgs.toArray(new String[0]));
  }

  /**
   * Executes main method of provided class
   *
   * @param classNameWithSlashes class (with `/` as separators) to search main method in
   * @param args arguments to pass to the main method
   */
  public final void executeMain(String classNameWithSlashes, String... args) {
    var e = env();
    try (var className = CTypeConversion.toCString(classNameWithSlashes);
        var mainName = CTypeConversion.toCString("main");
        var stringName = CTypeConversion.toCString("java/lang/String");
        var mainSig = CTypeConversion.toCString("([Ljava/lang/String;)V"); ) {
      var fn = e.getFunctions();
      var mainClazz = fn.getFindClass().call(e, className.get());
      assert mainClazz.isNonNull() : "Class not found " + classNameWithSlashes;
      var mainMethod = fn.getGetStaticMethodID().call(e, mainClazz, mainName.get(), mainSig.get());
      assert mainMethod.isNonNull() : "main method found in " + classNameWithSlashes;
      var stringClazz = fn.getFindClass().call(e, stringName.get());
      var argsCopy =
          fn.getNewObjectArray().call(e, args.length, stringClazz, WordFactory.nullPointer());

      for (var i = 0; i < args.length; i++) {
        try (var ithArg = CTypeConversion.toCString(args[i]); ) {
          var str = fn.getNewStringUTF().call(e, ithArg.get());
          fn.getSetObjectArrayElement().call(e, argsCopy, i, str);
        }
      }
      var arg = StackValue.get(JNI.JValue.class);
      arg.setJObject(argsCopy);
      fn.getCallStaticVoidMethodA().call(e, mainClazz, mainMethod, arg);
    }
  }

  /**
   * Initialize or just obtain environment associated with this JVM.
   *
   * @return JNI environment to make calls into the JVM
   */
  final JNI.JNIEnv env() {
    if (javaVM.isNull()) {
      var env = initializeEnv();
      if (env.isNonNull()) {
        threadEnvs.set(env.rawValue());
        return env;
      }
    }
    var rawEnv = threadEnvs.get();
    JNI.JNIEnv env;
    if (rawEnv == null) {
      var envOut = StackValue.get(JNI.JNIEnvPointer.class);
      var attachThreadFn = javaVM.getFunctions().getAttachCurrentThread();
      var res = attachThreadFn.call(javaVM, envOut, WordFactory.nullPointer());
      if (res != JNI.JNI_OK()) {
        throw new AssertionError("Error attaching thread: " + res);
      }
      env = envOut.readJNIEnv();
      threadEnvs.set(env.rawValue());
    } else {
      env = WordFactory.pointer(rawEnv);
    }
    return env;
  }

  private synchronized JNI.JNIEnv initializeEnv() {
    if (javaVM.isNonNull()) {
      return WordFactory.nullPointer();
    }
    var jvmArgs = StackValue.get(JNIBoot.Args.class);
    var optionsCount = options.length;
    jvmArgs.nOptions(optionsCount);
    var sizeOfOption = SizeOf.get(JNIBoot.Option.class);
    JNIBoot.Option jvmOpts = UnmanagedMemory.calloc(optionsCount * sizeOfOption);
    var holder = new CTypeConversion.CCharPointerHolder[optionsCount];
    for (var i = 0; i < optionsCount; i++) {
      holder[i] = CTypeConversion.toCString(options[i]);
      var nth = jvmOpts.addressOf(i);
      nth.setOptionString(holder[i].get());
      nth.setExtraInfo(WordFactory.nullPointer());
    }
    jvmArgs.options(jvmOpts);
    jvmArgs.version(JNI.JNI_VERSION_10());
    jvmArgs.ignoreUnrecognized(false);

    var jvmPtr = StackValue.get(JNI.JavaVMPointer.class);
    var envPtr = StackValue.get(JNI.JNIEnvPointer.class);

    int res = createJvmFn.call(jvmPtr, envPtr, jvmArgs);
    if (res != JNI.JNI_OK()) {
      throw new AssertionError("Error creating JVM: " + res);
    }

    for (var i = 0; i < optionsCount; i++) {
      holder[i].close();
    }
    UnmanagedMemory.free(jvmOpts);

    javaVM = jvmPtr.readJavaVM();
    return envPtr.readJNIEnv();
  }
}
