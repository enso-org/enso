package org.enso.jvm.channel;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.struct.CField;
import org.graalvm.nativeimage.c.struct.CStruct;
import org.graalvm.word.PointerBase;

@CContext(value = JNIDirectives.class)
@CStruct(value = "JNINativeInterface_", addStructKeyword = true)
public interface JNINativeInterface extends PointerBase {

  @CField(value = "NewString")
  JNI.NewString getNewString();

  @CField(value = "GetStringLength")
  JNI.GetStringLength getGetStringLength();

  @CField(value = "GetStringChars")
  JNI.GetStringChars getGetStringChars();

  @CField(value = "ReleaseStringChars")
  JNI.ReleaseStringChars getReleaseStringChars();

  @CField(value = "NewStringUTF")
  JNI.NewStringUTF8 getNewStringUTF();

  @CField(value = "GetStringUTFLength")
  JNI.GetStringUTFLength getGetStringUTFLength();

  @CField(value = "GetStringUTFChars")
  JNI.GetStringUTFChars getGetStringUTFChars();

  @CField(value = "ReleaseStringUTFChars")
  JNI.ReleaseStringUTFChars getReleaseStringUTFChars();

  @CField(value = "GetArrayLength")
  JNI.GetArrayLength getGetArrayLength();

  @CField(value = "NewLocalRef")
  JNI.NewLocalRef getNewLocalRef();

  @CField(value = "NewObjectArray")
  JNI.NewObjectArray getNewObjectArray();

  @CField(value = "NewBooleanArray")
  JNI.NewBooleanArray getNewBooleanArray();

  @CField(value = "NewByteArray")
  JNI.NewByteArray getNewByteArray();

  @CField(value = "NewCharArray")
  JNI.NewCharArray getNewCharArray();

  @CField(value = "NewShortArray")
  JNI.NewShortArray getNewShortArray();

  @CField(value = "NewIntArray")
  JNI.NewIntArray getNewIntArray();

  @CField(value = "NewLongArray")
  JNI.NewLongArray getNewLongArray();

  @CField(value = "NewFloatArray")
  JNI.NewFloatArray getNewFloatArray();

  @CField(value = "NewDoubleArray")
  JNI.NewDoubleArray getNewDoubleArray();

  @CField(value = "GetObjectArrayElement")
  JNI.GetObjectArrayElement getGetObjectArrayElement();

  @CField(value = "SetObjectArrayElement")
  JNI.SetObjectArrayElement getSetObjectArrayElement();

  @CField(value = "GetBooleanArrayElements")
  JNI.GetBooleanArrayElements getGetBooleanArrayElements();

  @CField(value = "GetByteArrayElements")
  JNI.GetByteArrayElements getGetByteArrayElements();

  @CField(value = "GetCharArrayElements")
  JNI.GetCharArrayElements getGetCharArrayElements();

  @CField(value = "GetShortArrayElements")
  JNI.GetShortArrayElements getGetShortArrayElements();

  @CField(value = "GetIntArrayElements")
  JNI.GetIntArrayElements getGetIntArrayElements();

  @CField(value = "GetLongArrayElements")
  JNI.GetLongArrayElements getGetLongArrayElements();

  @CField(value = "GetFloatArrayElements")
  JNI.GetFloatArrayElements getGetFloatArrayElements();

  @CField(value = "GetDoubleArrayElements")
  JNI.GetDoubleArrayElements getGetDoubleArrayElements();

  @CField(value = "ReleaseBooleanArrayElements")
  JNI.ReleaseBooleanArrayElements getReleaseBooleanArrayElements();

  @CField(value = "ReleaseByteArrayElements")
  JNI.ReleaseByteArrayElements getReleaseByteArrayElements();

  @CField(value = "ReleaseCharArrayElements")
  JNI.ReleaseCharArrayElements getReleaseCharArrayElements();

  @CField(value = "ReleaseShortArrayElements")
  JNI.ReleaseShortArrayElements getReleaseShortArrayElements();

  @CField(value = "ReleaseIntArrayElements")
  JNI.ReleaseIntArrayElements getReleaseIntArrayElements();

  @CField(value = "ReleaseLongArrayElements")
  JNI.ReleaseLongArrayElements getReleaseLongArrayElements();

  @CField(value = "ReleaseFloatArrayElements")
  JNI.ReleaseFloatArrayElements getReleaseFloatArrayElements();

  @CField(value = "ReleaseDoubleArrayElements")
  JNI.ReleaseDoubleArrayElements getReleaseDoubleArrayElements();

  @CField(value = "GetBooleanArrayRegion")
  JNI.GetBooleanArrayRegion getGetBooleanArrayRegion();

  @CField(value = "GetByteArrayRegion")
  JNI.GetByteArrayRegion getGetByteArrayRegion();

  @CField(value = "GetCharArrayRegion")
  JNI.GetCharArrayRegion getGetCharArrayRegion();

  @CField(value = "GetShortArrayRegion")
  JNI.GetShortArrayRegion getGetShortArrayRegion();

  @CField(value = "GetIntArrayRegion")
  JNI.GetIntArrayRegion getGetIntArrayRegion();

  @CField(value = "GetLongArrayRegion")
  JNI.GetLongArrayRegion getGetLongArrayRegion();

  @CField(value = "GetFloatArrayRegion")
  JNI.GetFloatArrayRegion getGetFloatArrayRegion();

  @CField(value = "GetDoubleArrayRegion")
  JNI.GetDoubleArrayRegion getGetDoubleArrayRegion();

  @CField(value = "SetBooleanArrayRegion")
  JNI.SetBooleanArrayRegion getSetBooleanArrayRegion();

  @CField(value = "SetByteArrayRegion")
  JNI.SetByteArrayRegion getSetByteArrayRegion();

  @CField(value = "SetCharArrayRegion")
  JNI.SetCharArrayRegion getSetCharArrayRegion();

  @CField(value = "SetShortArrayRegion")
  JNI.SetShortArrayRegion getSetShortArrayRegion();

  @CField(value = "SetIntArrayRegion")
  JNI.SetIntArrayRegion getSetIntArrayRegion();

  @CField(value = "SetLongArrayRegion")
  JNI.SetLongArrayRegion getSetLongArrayRegion();

  @CField(value = "SetFloatArrayRegion")
  JNI.SetFloatArrayRegion getSetFloatArrayRegion();

  @CField(value = "SetDoubleArrayRegion")
  JNI.SetDoubleArrayRegion getSetDoubleArrayRegion();

  @CField(value = "FindClass")
  JNI.FindClass getFindClass();

  @CField(value = "DefineClass")
  JNI.DefineClass getDefineClass();

  @CField(value = "IsSameObject")
  JNI.IsSameObject getIsSameObject();

  @CField(value = "GetObjectClass")
  JNI.GetObjectClass getGetObjectClass();

  @CField(value = "NewGlobalRef")
  JNI.NewGlobalRef getNewGlobalRef();

  @CField(value = "DeleteGlobalRef")
  JNI.DeleteGlobalRef getDeleteGlobalRef();

  @CField(value = "NewWeakGlobalRef")
  JNI.NewWeakGlobalRef getNewWeakGlobalRef();

  @CField(value = "DeleteWeakGlobalRef")
  JNI.DeleteWeakGlobalRef getDeleteWeakGlobalRef();

  @CField(value = "DeleteLocalRef")
  JNI.DeleteLocalRef getDeleteLocalRef();

  @CField(value = "PushLocalFrame")
  JNI.PushLocalFrame getPushLocalFrame();

  @CField(value = "PopLocalFrame")
  JNI.PopLocalFrame getPopLocalFrame();

  @CField(value = "NewObjectA")
  JNI.NewObjectA getNewObjectA();

  @CField(value = "GetStaticMethodID")
  JNI.GetStaticMethodID getGetStaticMethodID();

  @CField(value = "GetMethodID")
  JNI.GetMethodID getGetMethodID();

  @CField(value = "GetStaticFieldID")
  JNI.GetStaticFieldID getGetStaticFieldID();

  @CField(value = "GetFieldID")
  JNI.GetFieldID getGetFieldID();

  @CField(value = "CallStaticBooleanMethodA")
  JNI.CallStaticBooleanMethodA getCallStaticBooleanMethodA();

  @CField(value = "CallStaticIntMethodA")
  JNI.CallStaticIntMethodA getCallStaticIntMethodA();

  @CField(value = "CallStaticVoidMethodA")
  JNI.CallStaticVoidMethodA getCallStaticVoidMethodA();

  @CField(value = "CallStaticObjectMethodA")
  JNI.CallStaticObjectMethodA getCallStaticObjectMethodA();

  @CField(value = "CallStaticLongMethodA")
  JNI.CallStaticLongMethodA getCallStaticLongMethodA();

  @CField(value = "CallObjectMethodA")
  JNI.CallObjectMethodA getCallObjectMethodA();

  @CField(value = "CallVoidMethodA")
  JNI.CallVoidMethodA getCallVoidMethodA();

  @CField(value = "CallBooleanMethodA")
  JNI.CallBooleanMethodA getCallBooleanMethodA();

  @CField(value = "CallShortMethodA")
  JNI.CallShortMethodA getCallShortMethodA();

  @CField(value = "CallIntMethodA")
  JNI.CallIntMethodA getCallIntMethodA();

  @CField(value = "CallLongMethodA")
  JNI.CallLongMethodA getCallLongMethodA();

  @CField(value = "CallDoubleMethodA")
  JNI.CallDoubleMethodA getCallDoubleMethodA();

  @CField(value = "CallFloatMethodA")
  JNI.CallFloatMethodA getCallFloatMethodA();

  @CField(value = "CallByteMethodA")
  JNI.CallByteMethodA getCallByteMethodA();

  @CField(value = "CallCharMethodA")
  JNI.CallCharMethodA getCallCharMethodA();

  @CField(value = "GetStaticObjectField")
  JNI.GetStaticObjectField getGetStaticObjectField();

  @CField(value = "GetIntField")
  JNI.GetIntField getGetIntField();

  @CField(value = "GetStaticBooleanField")
  JNI.GetStaticBooleanField getGetStaticBooleanField();

  @CField(value = "SetStaticBooleanField")
  JNI.SetStaticBooleanField getSetStaticBooleanField();

  @CField(value = "ExceptionCheck")
  JNI.ExceptionCheck getExceptionCheck();

  @CField(value = "ExceptionOccurred")
  JNI.ExceptionOccurred getExceptionOccurred();

  @CField(value = "ExceptionClear")
  JNI.ExceptionClear getExceptionClear();

  @CField(value = "ExceptionDescribe")
  JNI.ExceptionDescribe getExceptionDescribe();

  @CField(value = "Throw")
  JNI.Throw getThrow();

  @CField(value = "GetObjectRefType")
  JNI.GetObjectRefType getGetObjectRefType();

  @CField(value = "GetDirectBufferAddress")
  JNI.GetDirectBufferAddress getGetDirectBufferAddress();

  @CField(value = "IsInstanceOf")
  JNI.IsInstanceOf getIsInstanceOf();

  @CField(value = "GetJavaVM")
  JNI.GetJavaVM getGetJavaVM();
}
