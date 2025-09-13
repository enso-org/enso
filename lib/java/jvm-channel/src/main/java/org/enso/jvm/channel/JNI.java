package org.enso.jvm.channel;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.constant.CConstant;
import org.graalvm.nativeimage.c.function.CFunction.Transition;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.function.InvokeCFunctionPointer;
import org.graalvm.nativeimage.c.struct.CField;
import org.graalvm.nativeimage.c.struct.CPointerTo;
import org.graalvm.nativeimage.c.struct.CStruct;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CDoublePointer;
import org.graalvm.nativeimage.c.type.CFloatPointer;
import org.graalvm.nativeimage.c.type.CIntPointer;
import org.graalvm.nativeimage.c.type.CLongPointer;
import org.graalvm.nativeimage.c.type.CShortPointer;
import org.graalvm.nativeimage.c.type.VoidPointer;
import org.graalvm.word.PointerBase;

@CContext(JNIDirectives.class)
final class JNI {
  @CConstant
  static native int JNI_OK();

  @CConstant
  static native int JNI_ERR();

  @CConstant
  static native int JNI_EDETACHED();

  @CConstant
  static native int JNI_EVERSION();

  @CConstant
  static native int JNI_ENOMEM();

  @CConstant
  static native int JNI_EEXIST();

  @CConstant
  static native int JNI_EINVAL();

  @CConstant
  static native int JNI_VERSION_1_1();

  @CConstant
  static native int JNI_VERSION_10();

  @CConstant
  static native int JNI_VERSION_21();

  interface JMethodID extends PointerBase {}

  interface JFieldID extends PointerBase {}

  interface JObject extends PointerBase {}

  interface JArray extends JObject {

    int MODE_WRITE_RELEASE = 0;
    int MODE_WRITE = 1;
    int MODE_RELEASE = 2;
  }

  interface JBooleanArray extends JArray {}

  interface JByteArray extends JArray {}

  interface JCharArray extends JArray {}

  interface JShortArray extends JArray {}

  interface JIntArray extends JArray {}

  interface JLongArray extends JArray {}

  interface JFloatArray extends JArray {}

  interface JDoubleArray extends JArray {}

  interface JObjectArray extends JArray {}

  interface JClass extends JObject {}

  interface JString extends JObject {}

  interface JThrowable extends JObject {}

  interface JWeak extends JObject {}

  @CStruct("jvalue")
  interface JValue extends PointerBase {

    // @formatter:off
    @CField("z")
    boolean getBoolean();

    @CField("b")
    byte getByte();

    @CField("c")
    char getChar();

    @CField("s")
    short getShort();

    @CField("i")
    int getInt();

    @CField("j")
    long getLong();

    @CField("f")
    float getFloat();

    @CField("d")
    double getDouble();

    @CField("l")
    JObject getJObject();

    @CField("z")
    void setBoolean(boolean b);

    @CField("b")
    void setByte(byte b);

    @CField("c")
    void setChar(char ch);

    @CField("s")
    void setShort(short s);

    @CField("i")
    void setInt(int i);

    @CField("j")
    void setLong(long l);

    @CField("f")
    void setFloat(float f);

    @CField("d")
    void setDouble(double d);

    @CField("l")
    void setJObject(JObject obj);

    // @formatter:on

    /** Gets JValue in an array of JValues pointed to by this object. */
    JValue addressOf(int index);
  }

  @CStruct(value = "JNIEnv_", addStructKeyword = true)
  interface JNIEnv extends PointerBase {

    @CField("functions")
    JNINativeInterface getFunctions();
  }

  @CPointerTo(JNIEnv.class)
  interface JNIEnvPointer extends PointerBase {

    JNIEnv readJNIEnv();

    void writeJNIEnv(JNIEnv env);
  }

  @CStruct(value = "JavaVM_", addStructKeyword = true)
  interface JavaVM extends PointerBase {

    @CField("functions")
    JNIInvokeInterface getFunctions();
  }

  @CPointerTo(JavaVM.class)
  interface JavaVMPointer extends PointerBase {

    JavaVM readJavaVM();

    void writeJavaVM(JavaVM javaVM);
  }

  @CStruct(value = "JavaVMAttachArgs", addStructKeyword = true)
  interface JavaVMAttachArgs extends PointerBase {

    @CField("version")
    int getVersion();

    @CField("version")
    void setVersion(int version);

    @CField("name")
    CCharPointer getName();

    @CField("name")
    void setName(CCharPointer name);

    @CField("group")
    JObject getGroup();

    @CField("group")
    void setGroup(JObject group);
  }

  @CStruct(value = "JNIInvokeInterface_", addStructKeyword = true)
  interface JNIInvokeInterface extends PointerBase {

    @CField("AttachCurrentThread")
    AttachCurrentThread getAttachCurrentThread();

    @CField("AttachCurrentThreadAsDaemon")
    AttachCurrentThreadAsDaemon getAttachCurrentThreadAsDaemon();

    @CField("DetachCurrentThread")
    DetachCurrentThread getDetachCurrentThread();

    @CField("GetEnv")
    GetEnv getGetEnv();
  }

  interface CallStaticIntMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface CallStaticBooleanMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface CallStaticVoidMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface CallStaticObjectMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JObject callNoTransition(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface CallStaticLongMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    long call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface CallObjectMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JObject object, JMethodID methodID, JValue args);
  }

  interface CallVoidMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallBooleanMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallShortMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    short call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallIntMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallLongMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    long call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallDoubleMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    double call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallFloatMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    float call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallByteMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    byte call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface CallCharMethodA extends CFunctionPointer {

    @InvokeCFunctionPointer
    char call(JNIEnv env, JObject o, JMethodID methodID, JValue args);
  }

  interface DeleteGlobalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JObject gref);
  }

  interface DeleteWeakGlobalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JWeak wref);
  }

  interface DeleteLocalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JObject lref);
  }

  interface PushLocalFrame extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, int capacity);
  }

  interface PopLocalFrame extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JObject result);
  }

  interface ExceptionCheck extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    boolean callNoTransition(JNIEnv env);
  }

  interface ExceptionClear extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env);
  }

  interface ExceptionDescribe extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    void callNoTransition(JNIEnv env);
  }

  interface ExceptionOccurred extends CFunctionPointer {

    @InvokeCFunctionPointer
    JThrowable call(JNIEnv env);
  }

  interface FindClass extends CFunctionPointer {

    @InvokeCFunctionPointer
    JClass call(JNIEnv env, CCharPointer name);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JClass callNoTransition(JNIEnv env, CCharPointer name);
  }

  interface DefineClass extends CFunctionPointer {

    @InvokeCFunctionPointer
    JClass call(JNIEnv env, CCharPointer name, JObject loader, CCharPointer buf, long bufLen);
  }

  interface GetArrayLength extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JArray array);
  }

  interface GetBooleanArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CCharPointer call(JNIEnv env, JBooleanArray array, JValue isCopy);
  }

  interface GetByteArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CCharPointer call(JNIEnv env, JByteArray array, JValue isCopy);
  }

  interface GetCharArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CShortPointer call(JNIEnv env, JCharArray array, JValue isCopy);
  }

  interface GetShortArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CShortPointer call(JNIEnv env, JShortArray array, JValue isCopy);
  }

  interface GetIntArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CIntPointer call(JNIEnv env, JIntArray array, JValue isCopy);
  }

  interface GetLongArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CLongPointer call(JNIEnv env, JLongArray array, JValue isCopy);
  }

  interface GetFloatArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CFloatPointer call(JNIEnv env, JFloatArray array, JValue isCopy);
  }

  interface GetDoubleArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    CDoublePointer call(JNIEnv env, JDoubleArray array, JValue isCopy);
  }

  interface GetMethodID extends CFunctionPointer {

    @InvokeCFunctionPointer
    JMethodID call(JNIEnv env, JClass clazz, CCharPointer name, CCharPointer sig);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JMethodID callNoTransition(JNIEnv env, JClass clazz, CCharPointer name, CCharPointer sig);
  }

  interface GetObjectArrayElement extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JObjectArray array, int index);
  }

  interface GetObjectClass extends CFunctionPointer {

    @InvokeCFunctionPointer
    JClass call(JNIEnv env, JObject object);
  }

  interface GetObjectRefType extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JObject obj);
  }

  interface GetStaticMethodID extends CFunctionPointer {

    @InvokeCFunctionPointer
    JMethodID call(JNIEnv env, JClass clazz, CCharPointer name, CCharPointer sig);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JMethodID callNoTransition(JNIEnv env, JClass clazz, CCharPointer name, CCharPointer sig);
  }

  interface GetStringChars extends CFunctionPointer {

    @InvokeCFunctionPointer
    CShortPointer call(JNIEnv env, JString string, JValue isCopy);
  }

  interface GetStringLength extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JString string);
  }

  interface GetStringUTFChars extends CFunctionPointer {

    @InvokeCFunctionPointer
    CCharPointer call(JNIEnv env, JString string, JValue isCopy);
  }

  interface GetStringUTFLength extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JString str);
  }

  interface IsSameObject extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env, JObject ref1, JObject ref2);
  }

  interface NewBooleanArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JBooleanArray call(JNIEnv env, int len);
  }

  interface NewByteArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JByteArray call(JNIEnv env, int len);
  }

  interface NewCharArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JCharArray call(JNIEnv env, int len);
  }

  interface NewShortArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JShortArray call(JNIEnv env, int len);
  }

  interface NewIntArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JIntArray call(JNIEnv env, int len);
  }

  interface NewLongArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JLongArray call(JNIEnv env, int len);
  }

  interface NewFloatArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JFloatArray call(JNIEnv env, int len);
  }

  interface NewDoubleArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JDoubleArray call(JNIEnv env, int len);
  }

  interface NewGlobalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JObject lobj);
  }

  interface NewWeakGlobalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    JWeak call(JNIEnv env, JObject lobj);
  }

  interface NewObjectA extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JObject callNoTransition(JNIEnv env, JClass clazz, JMethodID methodID, JValue args);
  }

  interface NewLocalRef extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JObject obj);
  }

  interface NewObjectArray extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObjectArray call(JNIEnv env, int len, JClass clazz, JObject init);
  }

  interface NewString extends CFunctionPointer {

    @InvokeCFunctionPointer
    JString call(JNIEnv env, CShortPointer unicode, int len);
  }

  interface NewStringUTF8 extends CFunctionPointer {

    @InvokeCFunctionPointer
    JString call(JNIEnv env, CCharPointer bytes);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    JString callNoTransition(JNIEnv env, CCharPointer bytes);
  }

  interface ReleaseBooleanArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JBooleanArray array, CCharPointer elems, int mode);
  }

  interface ReleaseByteArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JByteArray array, CCharPointer elems, int mode);
  }

  interface ReleaseCharArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JCharArray array, CShortPointer elems, int mode);
  }

  interface ReleaseShortArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JShortArray array, CShortPointer elems, int mode);
  }

  interface ReleaseIntArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JIntArray array, CIntPointer elems, int mode);
  }

  interface ReleaseLongArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JLongArray array, CLongPointer elems, int mode);
  }

  interface ReleaseFloatArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JFloatArray array, CFloatPointer elems, int mode);
  }

  interface ReleaseDoubleArrayElements extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JDoubleArray array, CDoublePointer elems, int mode);
  }

  interface GetBooleanArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JBooleanArray array, int start, int len, CCharPointer buf);
  }

  interface GetByteArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JByteArray array, int start, int len, CCharPointer buf);
  }

  interface GetCharArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JCharArray array, int start, int len, CShortPointer buf);
  }

  interface GetShortArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JShortArray array, int start, int len, CShortPointer buf);
  }

  interface GetIntArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JIntArray array, int start, int len, CIntPointer buf);
  }

  interface GetLongArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JLongArray array, int start, int len, CLongPointer buf);
  }

  interface GetFloatArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JFloatArray array, int start, int len, CFloatPointer buf);
  }

  interface GetDoubleArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JDoubleArray array, int start, int len, CDoublePointer buf);
  }

  interface SetBooleanArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JBooleanArray array, int start, int len, CCharPointer buf);
  }

  interface SetByteArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JByteArray array, int start, int len, CCharPointer buf);
  }

  interface SetCharArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JCharArray array, int start, int len, CShortPointer buf);
  }

  interface SetShortArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JShortArray array, int start, int len, CShortPointer buf);
  }

  interface SetIntArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JIntArray array, int start, int len, CIntPointer buf);
  }

  interface SetLongArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JLongArray array, int start, int len, CLongPointer buf);
  }

  interface SetFloatArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JFloatArray array, int start, int len, CFloatPointer buf);
  }

  interface SetDoubleArrayRegion extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JDoubleArray array, int start, int len, CDoublePointer buf);
  }

  interface ReleaseStringChars extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JString string, CShortPointer chars);
  }

  interface ReleaseStringUTFChars extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JString string, CCharPointer chars);
  }

  interface SetObjectArrayElement extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JObjectArray array, int index, JObject val);
  }

  interface Throw extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JThrowable throwable);

    @InvokeCFunctionPointer(transition = Transition.NO_TRANSITION)
    int callNoTransition(JNIEnv env, JThrowable throwable);
  }

  interface GetDirectBufferAddress extends CFunctionPointer {

    @InvokeCFunctionPointer
    VoidPointer call(JNIEnv env, JObject buf);
  }

  interface IsInstanceOf extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env, JObject o, JClass c);
  }

  interface GetStaticFieldID extends CFunctionPointer {

    @InvokeCFunctionPointer
    JFieldID call(JNIEnv env, JClass clazz, CCharPointer name, CCharPointer sig);
  }

  interface GetFieldID extends CFunctionPointer {

    @InvokeCFunctionPointer
    JFieldID call(JNIEnv env, JClass c, CCharPointer name, CCharPointer sig);
  }

  interface GetStaticObjectField extends CFunctionPointer {

    @InvokeCFunctionPointer
    JObject call(JNIEnv env, JClass clazz, JFieldID fieldID);
  }

  interface GetIntField extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JObject o, JFieldID fieldId);
  }

  interface GetStaticBooleanField extends CFunctionPointer {

    @InvokeCFunctionPointer
    boolean call(JNIEnv env, JClass clazz, JFieldID fieldID);
  }

  interface SetStaticBooleanField extends CFunctionPointer {

    @InvokeCFunctionPointer
    void call(JNIEnv env, JClass clazz, JFieldID fieldID, boolean value);
  }

  interface GetJavaVM extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JNIEnv env, JavaVMPointer javaVMOut);
  }

  interface AttachCurrentThread extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JavaVM vm, JNIEnvPointer envOut, JavaVMAttachArgs args);
  }

  interface AttachCurrentThreadAsDaemon extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JavaVM vm, JNIEnvPointer envOut, JavaVMAttachArgs args);
  }

  interface DetachCurrentThread extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JavaVM vm);
  }

  interface GetEnv extends CFunctionPointer {

    @InvokeCFunctionPointer
    int call(JavaVM vm, JNIEnvPointer envOut, int version);
  }
}
