package org.apache.poi.hssf.util;

/* ====================================================================
   Licensed to the Apache Software Foundation (ASF) under one or more
   contributor license agreements.  See the NOTICE file distributed with
   this work for additional information regarding copyright ownership.
   The ASF licenses this file to You under the Apache License, Version 2.0
   (the "License"); you may not use this file except in compliance with
   the License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
==================================================================== */

import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import org.apache.poi.ss.usermodel.Color;

/**
 * Intends to provide support for the very evil index to triplet issue and will likely replace the
 * color constants interface for HSSF 2.0. This class contains static inner class members for
 * representing colors. Each color has an index (for the standard palette in Excel (tm) ), native
 * (RGB) triplet and string triplet. The string triplet is as the color would be represented by
 * Gnumeric. Having (string) this here is a bit of a collision of function between HSSF and the
 * HSSFSerializer but I think its a reasonable one in this case.
 */
public class HSSFColor implements Color {

  private static Map<Integer, HSSFColor> indexHash;
  private static Map<HSSFColorPredefined, HSSFColor> enumList;

  private final int index;
  private final int index2;
  private final int rgb;

  /**
   * Predefined HSSFColors with their given palette index (and an optional 2nd index)
   *
   * @since POI 3.16 beta 2
   */
  public enum HSSFColorPredefined {
    BLACK(0x08, -1, 0x000000),
    BROWN(0x3C, -1, 0x993300),
    OLIVE_GREEN(0x3B, -1, 0x333300),
    DARK_GREEN(0x3A, -1, 0x003300),
    DARK_TEAL(0x38, -1, 0x003366),
    DARK_BLUE(0x12, 0x20, 0x000080),
    INDIGO(0x3E, -1, 0x333399),
    GREY_80_PERCENT(0x3F, -1, 0x333333),
    ORANGE(0x35, -1, 0xFF6600),
    DARK_YELLOW(0x13, -1, 0x808000),
    GREEN(0x11, -1, 0x008000),
    TEAL(0x15, 0x26, 0x008080),
    BLUE(0x0C, 0x27, 0x0000FF),
    BLUE_GREY(0x36, -1, 0x666699),
    GREY_50_PERCENT(0x17, -1, 0x808080),
    RED(0x0A, -1, 0xFF0000),
    LIGHT_ORANGE(0x34, -1, 0xFF9900),
    LIME(0x32, -1, 0x99CC00),
    SEA_GREEN(0x39, -1, 0x339966),
    AQUA(0x31, -1, 0x33CCCC),
    LIGHT_BLUE(0x30, -1, 0x3366FF),
    VIOLET(0x14, 0x24, 0x800080),
    GREY_40_PERCENT(0x37, -1, 0x969696),
    PINK(0x0E, 0x21, 0xFF00FF),
    GOLD(0x33, -1, 0xFFCC00),
    YELLOW(0x0D, 0x22, 0xFFFF00),
    BRIGHT_GREEN(0x0B, -1, 0x00FF00),
    TURQUOISE(0x0F, 0x23, 0x00FFFF),
    DARK_RED(0x10, 0x25, 0x800000),
    SKY_BLUE(0x28, -1, 0x00CCFF),
    PLUM(0x3D, 0x19, 0x993366),
    GREY_25_PERCENT(0x16, -1, 0xC0C0C0),
    ROSE(0x2D, -1, 0xFF99CC),
    LIGHT_YELLOW(0x2B, -1, 0xFFFF99),
    LIGHT_GREEN(0x2A, -1, 0xCCFFCC),
    LIGHT_TURQUOISE(0x29, 0x1B, 0xCCFFFF),
    PALE_BLUE(0x2C, -1, 0x99CCFF),
    LAVENDER(0x2E, -1, 0xCC99FF),
    WHITE(0x09, -1, 0xFFFFFF),
    CORNFLOWER_BLUE(0x18, -1, 0x9999FF),
    LEMON_CHIFFON(0x1A, -1, 0xFFFFCC),
    MAROON(0x19, -1, 0x7F0000),
    ORCHID(0x1C, -1, 0x660066),
    CORAL(0x1D, -1, 0xFF8080),
    ROYAL_BLUE(0x1E, -1, 0x0066CC),
    LIGHT_CORNFLOWER_BLUE(0x1F, -1, 0xCCCCFF),
    TAN(0x2F, -1, 0xFFCC99),

    /**
     * Special Default/Normal/Automatic color.
     *
     * <p><i>Note:</i> This class is NOT in the default Map returned by HSSFColor. The index is a
     * special case which is interpreted in the various setXXXColor calls.
     */
    AUTOMATIC(0x40, -1, 0x000000);

    private final HSSFColor color;

    HSSFColorPredefined(int index, int index2, int rgb) {
      this.color = new HSSFColor(index, index2, rgb);
    }

    /**
     * @see HSSFColor#getIndex()
     */
    public short getIndex() {
      return color.getIndex();
    }

    /**
     * @see HSSFColor#getIndex2()
     */
    public short getIndex2() {
      return color.getIndex2();
    }

    /**
     * @see HSSFColor#getTriplet()
     */
    public short[] getTriplet() {
      return color.getTriplet();
    }

    /**
     * @see HSSFColor#getHexString()
     */
    public String getHexString() {
      return color.getHexString();
    }

    /**
     * @return (a copy of) the HSSFColor assigned to the enum
     */
    public HSSFColor getColor() {
      return new HSSFColor(getIndex(), getIndex2(), color.rgb);
    }
  }

  /** Creates a new instance of HSSFColor */
  public HSSFColor() {
    // automatic index
    this(0x40, -1, null);
  }

  public HSSFColor(int index, int index2, java.awt.Color color) {
    this.index = index;
    this.index2 = index2;
    if (color != null) throw new IllegalArgumentException("Unexpected color: " + color);
    this.rgb = 0x000000;
  }

  HSSFColor(int index, int index2, int rgb) {
    this.index = index;
    this.index2 = index2;
    this.rgb = 0xff000000 | rgb;
  }

  /**
   * This function returns all the colours in an unmodifiable Map. The map is cached on first use.
   *
   * @return a Map containing all colours keyed by {@code Integer} excel-style palette indexes
   */
  public static synchronized Map<Integer, HSSFColor> getIndexHash() {
    if (indexHash == null) {
      indexHash = Collections.unmodifiableMap(createColorsByIndexMap());
    }

    return indexHash;
  }

  /**
   * This function returns all the Colours, stored in a Map that can be edited. No caching is
   * performed. If you don't need to edit the table, then call {@link #getIndexHash()} which returns
   * a statically cached immutable map of colours.
   */
  public static Map<Integer, HSSFColor> getMutableIndexHash() {
    return createColorsByIndexMap();
  }

  private static Map<Integer, HSSFColor> createColorsByIndexMap() {
    Map<HSSFColorPredefined, HSSFColor> eList = mapEnumToColorClass();
    Map<Integer, HSSFColor> result = new HashMap<>(eList.size() * 3 / 2);

    for (Map.Entry<HSSFColorPredefined, HSSFColor> colorRef : eList.entrySet()) {
      Integer index1 = (int) colorRef.getKey().getIndex();
      if (!result.containsKey(index1)) {
        result.put(index1, colorRef.getValue());
      }
      Integer index2 = (int) colorRef.getKey().getIndex2();
      if (index2 != -1 && !result.containsKey(index2)) {
        result.put(index2, colorRef.getValue());
      }
    }
    return result;
  }

  /**
   * this function returns all colors in a hastable. It's not implemented as a static
   * member/statically initialized because that would be dirty in a server environment as it is
   * intended. This means you'll eat the time it takes to create it once per request but you will
   * not hold onto it if you have none of those requests.
   *
   * @return a Map containing all colors keyed by String gnumeric-like triplets
   */
  public static Map<String, HSSFColor> getTripletHash() {
    return createColorsByHexStringMap();
  }

  private static Map<String, HSSFColor> createColorsByHexStringMap() {
    Map<HSSFColorPredefined, HSSFColor> eList = mapEnumToColorClass();
    Map<String, HSSFColor> result = new HashMap<>(eList.size());

    for (Map.Entry<HSSFColorPredefined, HSSFColor> colorRef : eList.entrySet()) {
      String hexString = colorRef.getKey().getHexString();
      if (!result.containsKey(hexString)) {
        result.put(hexString, colorRef.getValue());
      }
    }
    return result;
  }

  /** Maps the Enums to the HSSFColor, in cases of user code evaluating the classname */
  private static synchronized Map<HSSFColorPredefined, HSSFColor> mapEnumToColorClass() {
    if (enumList == null) {
      enumList = new EnumMap<>(HSSFColorPredefined.class);
      // AUTOMATIC is not add to list
      addHSSFColorPredefined(HSSFColorPredefined.BLACK);
      addHSSFColorPredefined(HSSFColorPredefined.BROWN);
      addHSSFColorPredefined(HSSFColorPredefined.OLIVE_GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.DARK_GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.DARK_TEAL);
      addHSSFColorPredefined(HSSFColorPredefined.DARK_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.INDIGO);
      addHSSFColorPredefined(HSSFColorPredefined.GREY_80_PERCENT);
      addHSSFColorPredefined(HSSFColorPredefined.ORANGE);
      addHSSFColorPredefined(HSSFColorPredefined.DARK_YELLOW);
      addHSSFColorPredefined(HSSFColorPredefined.GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.TEAL);
      addHSSFColorPredefined(HSSFColorPredefined.BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.BLUE_GREY);
      addHSSFColorPredefined(HSSFColorPredefined.GREY_50_PERCENT);
      addHSSFColorPredefined(HSSFColorPredefined.RED);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_ORANGE);
      addHSSFColorPredefined(HSSFColorPredefined.LIME);
      addHSSFColorPredefined(HSSFColorPredefined.SEA_GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.AQUA);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.VIOLET);
      addHSSFColorPredefined(HSSFColorPredefined.GREY_40_PERCENT);
      addHSSFColorPredefined(HSSFColorPredefined.PINK);
      addHSSFColorPredefined(HSSFColorPredefined.GOLD);
      addHSSFColorPredefined(HSSFColorPredefined.YELLOW);
      addHSSFColorPredefined(HSSFColorPredefined.BRIGHT_GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.TURQUOISE);
      addHSSFColorPredefined(HSSFColorPredefined.DARK_RED);
      addHSSFColorPredefined(HSSFColorPredefined.SKY_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.PLUM);
      addHSSFColorPredefined(HSSFColorPredefined.GREY_25_PERCENT);
      addHSSFColorPredefined(HSSFColorPredefined.ROSE);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_YELLOW);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_GREEN);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_TURQUOISE);
      addHSSFColorPredefined(HSSFColorPredefined.PALE_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.LAVENDER);
      addHSSFColorPredefined(HSSFColorPredefined.WHITE);
      addHSSFColorPredefined(HSSFColorPredefined.CORNFLOWER_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.LEMON_CHIFFON);
      addHSSFColorPredefined(HSSFColorPredefined.MAROON);
      addHSSFColorPredefined(HSSFColorPredefined.ORCHID);
      addHSSFColorPredefined(HSSFColorPredefined.CORAL);
      addHSSFColorPredefined(HSSFColorPredefined.ROYAL_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.LIGHT_CORNFLOWER_BLUE);
      addHSSFColorPredefined(HSSFColorPredefined.TAN);
    }
    return enumList;
  }

  private static void addHSSFColorPredefined(HSSFColorPredefined color) {
    enumList.put(color, color.getColor());
  }

  /**
   * returns color standard palette index
   *
   * @return index to the standard palette
   */
  public short getIndex() {
    return (short) index;
  }

  /**
   * returns alternative color standard palette index
   *
   * @return alternative index to the standard palette, if -1 this index is not defined
   */
  public short getIndex2() {
    return (short) index2;
  }

  private int getRed() {
    return (rgb >> 16) & 0xFF;
  }

  private int getGreen() {
    return (rgb >> 8) & 0xFF;
  }

  private int getBlue() {
    return (rgb >> 0) & 0xFF;
  }

  /**
   * returns RGB triplet (0, 0, 0)
   *
   * @return triplet representation like that in Excel
   */
  public short[] getTriplet() {
    return new short[] {(short) getRed(), (short) getGreen(), (short) getBlue()};
  }

  /**
   * returns colon-delimited hex string "0:0:0"
   *
   * @return a hex string exactly like a gnumeric triplet
   */
  public String getHexString() {
    return (Integer.toHexString(getRed() * 0x101)
            + ":"
            + Integer.toHexString(getGreen() * 0x101)
            + ":"
            + Integer.toHexString(getBlue() * 0x101))
        .toUpperCase(Locale.ROOT);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    HSSFColor hssfColor = (HSSFColor) o;

    if (index != hssfColor.index) return false;
    if (index2 != hssfColor.index2) return false;
    return Objects.equals(rgb, hssfColor.rgb);
  }

  @Override
  public int hashCode() {
    return Objects.hash(rgb, index, index2);
  }

  /**
   * Checked type cast {@code color} to an HSSFColor.
   *
   * @param color the color to type cast
   * @return the type casted color
   * @throws IllegalArgumentException if color is null or is not an instance of HSSFColor
   */
  public static HSSFColor toHSSFColor(Color color) {
    // FIXME: this method would be more useful if it could convert any Color to an HSSFColor
    // Currently the only benefit of this method is to throw an IllegalArgumentException
    // instead of a ClassCastException.
    if (color != null && !(color instanceof HSSFColor)) {
      throw new IllegalArgumentException(
          "Only HSSFColor objects are supported, but had " + color.getClass());
    }
    return (HSSFColor) color;
  }
}
