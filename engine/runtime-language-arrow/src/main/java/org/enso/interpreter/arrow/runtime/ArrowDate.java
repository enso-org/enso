package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.LocalDate;

@ExportLibrary(InteropLibrary.class)
public class ArrowDate implements TruffleObject {
  private LocalDate date;

  public ArrowDate(LocalDate date) {
    this.date = date;
  }

  @ExportMessage
  public boolean isDate() {
    return true;
  }

  @ExportMessage
  public LocalDate asDate() {
    return date;
  }
}
