package org.enso.nativeimage.workarounds;

import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.word.PointerBase;

// Refer to https://github.com/oracle/graal/blob/master/substratevm/src/com.oracle.svm.tutorial/src/com/oracle/svm/tutorial/CInterfaceTutorial.java
public class AnsiTerm {
  @CFunction
  public static native PointerBase GetStdHandle(int nStdHandle);
//  @CFunction
//  public static native boolean GetConsoleMode(PointerBase hConsoleHandle, int dwMode);
  @CFunction
  public static native boolean SetConsoleMode(PointerBase hConsoleHandle, int dwMode);

  public static int STD_ERROR_HANDLE = -12;


}
