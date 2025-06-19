package org.enso.jvm.channel;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.function.InvokeCFunctionPointer;
import org.graalvm.nativeimage.c.struct.CField;
import org.graalvm.nativeimage.c.struct.CStruct;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.WordPointer;
import org.graalvm.word.PointerBase;

/** Java virtual machine initialization API. */
@CContext(JNIDirectives.class)
final class JNIBoot {
  interface JNICreateJavaVMPointer extends CFunctionPointer {
    @InvokeCFunctionPointer
    int call(JNI.JavaVMPointer jvmptr, JNI.JNIEnvPointer env, Args args);
  }

  @CStruct("JavaVMInitArgs")
  interface Args extends PointerBase {
    @CField
    int version();

    @CField
    void version(int v);

    @CField
    int nOptions();

    @CField
    void nOptions(int n);

    @CField
    Option options();

    @CField
    void options(Option ptr);

    @CField
    boolean ignoreUnrecognized();

    @CField
    void ignoreUnrecognized(boolean v);
  }

  @CStruct(value = "JavaVMOption")
  interface Option extends PointerBase {

    @CField("optionString")
    CCharPointer getOptionString();

    @CField("optionString")
    void setOptionString(CCharPointer value);

    @CField("extraInfo")
    WordPointer getExtraInfo();

    @CField("extraInfo")
    void setExtraInfo(WordPointer value);

    Option addressOf(int index);
  }
}
