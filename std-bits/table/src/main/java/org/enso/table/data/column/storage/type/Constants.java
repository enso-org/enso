package org.enso.table.data.column.storage.type;

public class Constants {
  public static final Boolean BOOLEAN = new Boolean();
  public static final Integer INTEGER_64 = new Integer(Bits.BITS_8);
  public static final Float FLOAT_64 = new Float(Bits.BITS_64);
  public static final Text STRING = new Text(-1, false);
  public static final Date DATE = new Date();
  public static final TimeOfDay TIME_OF_DAY = new TimeOfDay();
  public static final DateTime DATE_TIME = new DateTime();
  public static final AnyObject ANY_OBJECT = new AnyObject();
}
