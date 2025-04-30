/*
 *  ====================================================================
 *    Licensed to the Apache Software Foundation (ASF) under one or more
 *    contributor license agreements.  See the NOTICE file distributed with
 *    this work for additional information regarding copyright ownership.
 *    The ASF licenses this file to You under the Apache License, Version 2.0
 *    (the "License"); you may not use this file except in compliance with
 *    the License.  You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 * ====================================================================
 */

package org.apache.poi.util;

import static org.apache.commons.io.output.NullOutputStream.NULL_OUTPUT_STREAM;

import java.awt.Color;
import java.awt.geom.AffineTransform;
import java.awt.geom.Dimension2D;
import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.PackedColorModel;
import java.io.Closeable;
import java.io.File;
import java.io.FileOutputStream;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.poi.common.usermodel.GenericRecord;
import org.apache.poi.util.GenericRecordUtil.AnnotatedFlag;

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
@Beta
public class GenericRecordJsonWriter implements Closeable {
    private static final String TABS;
    private static final String ZEROS = "0000000000000000";
    private static final Pattern ESC_CHARS = Pattern.compile("[\"\\p{Cntrl}\\\\]");
    private static final String NL = System.getProperty("line.separator");


    @FunctionalInterface
    protected interface GenericRecordHandler {
        /**
         * Handler method
         *
         * @param record the parent record, applied via instance method reference
         * @param name the name of the property
         * @param object the value of the property
         * @return {@code true}, if the element was handled and output produced,
         *   The provided methods can be overridden and a implementation can return {@code false},
         *   if the element hasn't been written to the stream
         */
        boolean print(GenericRecordJsonWriter record, String name, Object object);
    }

    private static final List<Map.Entry<Class<?>,GenericRecordHandler>> handler = new ArrayList<>();

    static {
        char[] t = new char[255];
        Arrays.fill(t, '\t');
        TABS = new String(t);
        handler(String.class, GenericRecordJsonWriter::printObject);
        handler(Number.class, GenericRecordJsonWriter::printNumber);
        handler(Boolean.class, GenericRecordJsonWriter::printBoolean);
        handler(List.class, GenericRecordJsonWriter::printList);
        handler(GenericRecord.class, GenericRecordJsonWriter::printGenericRecord);
        handler(AnnotatedFlag.class, GenericRecordJsonWriter::printAnnotatedFlag);
        handler(byte[].class, GenericRecordJsonWriter::printBytes);
        handler(Point2D.class, GenericRecordJsonWriter::printPoint);
        handler(Dimension2D.class, GenericRecordJsonWriter::printDimension);
        handler(Rectangle2D.class, GenericRecordJsonWriter::printRectangle);
        handler(Path2D.class, GenericRecordJsonWriter::printPath);
        handler(AffineTransform.class, GenericRecordJsonWriter::printAffineTransform);
        handler(Color.class, GenericRecordJsonWriter::printColor);
        handler(BufferedImage.class, GenericRecordJsonWriter::printImage);
        handler(Array.class, GenericRecordJsonWriter::printArray);
        handler(Object.class, GenericRecordJsonWriter::printObject);
    }

    private static void handler(Class<?> c, GenericRecordHandler printer) {
        handler.add(new AbstractMap.SimpleEntry<>(c,printer));
    }

    protected final AppendableWriter aw;
    protected final PrintWriter fw;
    protected int indent = 0;
    protected boolean withComments = true;
    protected int childIndex = 0;

    public GenericRecordJsonWriter(File fileName) throws IOException {
        OutputStream os = ("null".equals(fileName.getName())) ? NULL_OUTPUT_STREAM : new FileOutputStream(fileName);
        aw = new AppendableWriter(new OutputStreamWriter(os, StandardCharsets.UTF_8));
        fw = new PrintWriter(aw);
    }

    public GenericRecordJsonWriter(Appendable buffer) {
        aw = new AppendableWriter(buffer);
        fw = new PrintWriter(aw);
    }

    public static String marshal(GenericRecord record) {
        return marshal(record, true);
    }

    public static String marshal(GenericRecord record, boolean withComments) {
        final StringBuilder sb = new StringBuilder();
        try (GenericRecordJsonWriter w = new GenericRecordJsonWriter(sb)) {
            w.setWithComments(withComments);
            w.write(record);
            return sb.toString();
        } catch (IOException e) {
            return "{}";
        }
    }

    public void setWithComments(boolean withComments) {
        this.withComments = withComments;
    }

    @Override
    public void close() throws IOException {
        fw.close();
    }

    protected String tabs() {
        return TABS.substring(0, Math.min(indent, TABS.length()));
    }

    public void write(GenericRecord record) {
        final String tabs = tabs();
        Enum<?> type = record.getGenericRecordType();
        String recordName = (type != null) ? type.name() : record.getClass().getSimpleName();
        fw.append(tabs);
        fw.append("{");
        if (withComments) {
            fw.append("   /* ");
            fw.append(recordName);
            if (childIndex > 0) {
                fw.append(" - index: ");
                fw.print(childIndex);
            }
            fw.append(" */");
        }
        fw.println();

        boolean hasProperties = writeProperties(record);
        fw.println();

        writeChildren(record, hasProperties);

        fw.append(tabs);
        fw.append("}");
    }

    protected boolean writeProperties(GenericRecord record) {
        Map<String, Supplier<?>> prop = record.getGenericProperties();
        if (prop == null || prop.isEmpty()) {
            return false;
        }

        final int oldChildIndex = childIndex;
        childIndex = 0;
        long cnt = prop.entrySet().stream().filter(e -> writeProp(e.getKey(),e.getValue())).count();
        childIndex = oldChildIndex;

        return cnt > 0;
    }


    protected boolean writeChildren(GenericRecord record, boolean hasProperties) {
        List<? extends GenericRecord> list = record.getGenericChildren();
        if (list == null || list.isEmpty()) {
            return false;
        }

        indent++;
        aw.setHoldBack(tabs() + (hasProperties ? ", " : "") + "\"children\": [" + NL);
        final int oldChildIndex = childIndex;
        childIndex = 0;
        long cnt = list.stream().filter(l -> writeValue(null, l) && ++childIndex > 0).count();
        childIndex = oldChildIndex;
        aw.setHoldBack(null);

        if (cnt > 0) {
            fw.println();
            fw.println(tabs() + "]");
        }
        indent--;

        return cnt > 0;
    }

    public void writeError(String errorMsg) {
        fw.append("{ error: ");
        printObject("error", errorMsg);
        fw.append(" }");
    }

    protected boolean writeProp(String name, Supplier<?> value) {
        final boolean isNext = (childIndex>0);
        aw.setHoldBack(isNext ? NL + tabs() + "\t, " : tabs() + "\t  ");
        final int oldChildIndex = childIndex;
        childIndex = 0;
        boolean written = writeValue(name, value.get());
        childIndex = oldChildIndex + (written ? 1 : 0);
        aw.setHoldBack(null);
        return written;
    }

    protected boolean writeValue(String name, Object o) {
        if (childIndex > 0) {
            aw.setHoldBack(",");
        }

        GenericRecordHandler grh = (o == null)
            ? GenericRecordJsonWriter::printNull
            : handler.stream().filter(h -> matchInstanceOrArray(h.getKey(), o)).
            findFirst().map(Map.Entry::getValue).orElse(null);

        boolean result = grh != null && grh.print(this, name, o);
        aw.setHoldBack(null);
        return result;
    }

    protected static boolean matchInstanceOrArray(Class<?> key, Object instance) {
        return key.isInstance(instance) || (Array.class.equals(key) && instance.getClass().isArray());
    }

    protected void printName(String name) {
        fw.print(name != null ? "\""+name+"\": " : "");
    }

    protected boolean printNull(String name, Object o) {
        printName(name);
        fw.write("null");
        return true;
    }

    @SuppressWarnings("java:S3516")
    protected boolean printNumber(String name, Object o) {
        Number n = (Number)o;
        printName(name);

        if (o instanceof Float) {
            fw.print(n.floatValue());
            return true;
        } else if (o instanceof Double) {
            fw.print(n.doubleValue());
            return true;
        }

        fw.print(n.longValue());

        final int size;
        if (n instanceof Byte) {
            size = 2;
        } else if (n instanceof Short) {
            size = 4;
        } else if (n instanceof Integer) {
            size = 8;
        } else if (n instanceof Long) {
            size = 16;
        } else {
            size = -1;
        }

        long l = n.longValue();
        if (withComments && size > 0 && (l < 0 || l > 9)) {
            fw.write(" /* 0x");
            fw.write(trimHex(l, size));
            fw.write(" */");
        }
        return true;
    }

    protected boolean printBoolean(String name, Object o) {
        printName(name);
        fw.write(((Boolean)o).toString());
        return true;
    }

    protected boolean printList(String name, Object o) {
        printName(name);
        fw.println("[");
        int oldChildIndex = childIndex;
        childIndex = 0;
        ((List<?>)o).forEach(e -> { writeValue(null, e); childIndex++; });
        childIndex = oldChildIndex;
        fw.write(tabs() + "\t]");
        return true;
    }

    protected boolean printGenericRecord(String name, Object o) {
        printName(name);
        this.indent++;
        write((GenericRecord) o);
        this.indent--;
        return true;
    }

    protected boolean printAnnotatedFlag(String name, Object o) {
        printName(name);
        AnnotatedFlag af = (AnnotatedFlag) o;
        fw.print(af.getValue().get().longValue());
        if (withComments) {
            fw.write(" /* ");
            fw.write(af.getDescription());
            fw.write(" */ ");
        }
        return true;
    }

    protected boolean printBytes(String name, Object o) {
        printName(name);
        fw.write('"');
        fw.write(Base64.getEncoder().encodeToString((byte[]) o));
        fw.write('"');
        return true;
    }

    protected boolean printPoint(String name, Object o) {
        printName(name);
        Point2D p = (Point2D)o;
        fw.write("{ \"x\": "+p.getX()+", \"y\": "+p.getY()+" }");
        return true;
    }

    protected boolean printDimension(String name, Object o) {
        printName(name);
        Dimension2D p = (Dimension2D)o;
        fw.write("{ \"width\": "+p.getWidth()+", \"height\": "+p.getHeight()+" }");
        return true;
    }

    protected boolean printRectangle(String name, Object o) {
        printName(name);
        Rectangle2D p = (Rectangle2D)o;
        fw.write("{ \"x\": "+p.getX()+", \"y\": "+p.getY()+", \"width\": "+p.getWidth()+", \"height\": "+p.getHeight()+" }");
        return true;
    }

    protected boolean printPath(String name, Object o) {
        printName(name);
        final PathIterator iter = ((Path2D)o).getPathIterator(null);
        final double[] pnts = new double[6];
        fw.write("[");

        indent += 2;
        String t = tabs();
        indent -= 2;

        boolean isNext = false;
        while (!iter.isDone()) {
            fw.println(isNext ? ", " : "");
            fw.print(t);
            isNext = true;
            final int segType = iter.currentSegment(pnts);
            fw.append("{ \"type\": ");
            switch (segType) {
                case PathIterator.SEG_MOVETO:
                    fw.write("\"move\", \"x\": "+pnts[0]+", \"y\": "+pnts[1]);
                    break;
                case PathIterator.SEG_LINETO:
                    fw.write("\"lineto\", \"x\": "+pnts[0]+", \"y\": "+pnts[1]);
                    break;
                case PathIterator.SEG_QUADTO:
                    fw.write("\"quad\", \"x1\": "+pnts[0]+", \"y1\": "+pnts[1]+", \"x2\": "+pnts[2]+", \"y2\": "+pnts[3]);
                    break;
                case PathIterator.SEG_CUBICTO:
                    fw.write("\"cubic\", \"x1\": "+pnts[0]+", \"y1\": "+pnts[1]+", \"x2\": "+pnts[2]+", \"y2\": "+pnts[3]+", \"x3\": "+pnts[4]+", \"y3\": "+pnts[5]);
                    break;
                case PathIterator.SEG_CLOSE:
                    fw.write("\"close\"");
                    break;
            }
            fw.append(" }");
            iter.next();
        }

        fw.write("]");
        return true;
    }

    protected boolean printObject(String name, Object o) {
        printName(name);
        fw.write('"');

        final String str = o.toString();
        final Matcher m = ESC_CHARS.matcher(str);
        int pos = 0;
        while (m.find()) {
            fw.append(str, pos, m.start());
            String match = m.group();
            switch (match) {
                case "\n":
                    fw.write("\\\\n");
                    break;
                case "\r":
                    fw.write("\\\\r");
                    break;
                case "\t":
                    fw.write("\\\\t");
                    break;
                case "\b":
                    fw.write("\\\\b");
                    break;
                case "\f":
                    fw.write("\\\\f");
                    break;
                case "\\":
                    fw.write("\\\\\\\\");
                    break;
                case "\"":
                    fw.write("\\\\\"");
                    break;
                default:
                    fw.write("\\\\u");
                    fw.write(trimHex(match.charAt(0), 4));
                    break;
            }
            pos = m.end();
        }
        fw.append(str, pos, str.length());
        fw.write('"');
        return true;
    }

    protected boolean printAffineTransform(String name, Object o) {
        printName(name);
        AffineTransform xForm = (AffineTransform)o;
        fw.write(
            "{ \"scaleX\": "+xForm.getScaleX()+
            ", \"shearX\": "+xForm.getShearX()+
            ", \"transX\": "+xForm.getTranslateX()+
            ", \"scaleY\": "+xForm.getScaleY()+
            ", \"shearY\": "+xForm.getShearY()+
            ", \"transY\": "+xForm.getTranslateY()+" }");
        return true;
    }

    protected boolean printColor(String name, Object o) {
        printName(name);

        final int rgb = ((Color)o).getRGB();
        fw.print(rgb);

        if (withComments) {
            fw.write(" /* 0x");
            fw.write(trimHex(rgb, 8));
            fw.write(" */");
        }
        return true;
    }

    protected boolean printArray(String name, Object o) {
        printName(name);
        fw.write("[");
        int length = Array.getLength(o);
        final int oldChildIndex = childIndex;
        for (childIndex=0; childIndex<length; childIndex++) {
            writeValue(null, Array.get(o, childIndex));
        }
        childIndex = oldChildIndex;
        fw.write(tabs() + "\t]");
        return true;
    }

    protected boolean printImage(String name, Object o) {
        BufferedImage img = (BufferedImage)o;

        final String[] COLOR_SPACES = {
            "XYZ","Lab","Luv","YCbCr","Yxy","RGB","GRAY","HSV","HLS","CMYK","Unknown","CMY","Unknown"
        };

        final String[] IMAGE_TYPES = {
                "CUSTOM","INT_RGB","INT_ARGB","INT_ARGB_PRE","INT_BGR","3BYTE_BGR","4BYTE_ABGR","4BYTE_ABGR_PRE",
                "USHORT_565_RGB","USHORT_555_RGB","BYTE_GRAY","USHORT_GRAY","BYTE_BINARY","BYTE_INDEXED"
        };

        printName(name);
        ColorModel cm = img.getColorModel();
        String colorType =
            (cm instanceof IndexColorModel) ? "indexed" :
            (cm instanceof ComponentColorModel) ? "component" :
            (cm instanceof DirectColorModel) ? "direct" :
            (cm instanceof PackedColorModel) ? "packed" : "unknown";
        fw.write(
            "{ \"width\": "+img.getWidth()+
            ", \"height\": "+img.getHeight()+
            ", \"type\": \""+IMAGE_TYPES[img.getType()]+"\""+
            ", \"colormodel\": \""+colorType+"\""+
            ", \"pixelBits\": "+cm.getPixelSize()+
            ", \"numComponents\": "+cm.getNumComponents()+
            ", \"colorSpace\": \""+COLOR_SPACES[Math.min(cm.getColorSpace().getType(),12)]+"\""+
            ", \"transparency\": "+cm.getTransparency()+
            ", \"alpha\": "+cm.hasAlpha()+
            "}"
        );
        return true;
    }

    static String trimHex(final long l, final int size) {
        final String b = Long.toHexString(l);
        int len = b.length();
        return ZEROS.substring(0, Math.max(0,size-len)) + b.substring(Math.max(0,len-size), len);
    }

    static class AppendableWriter extends Writer {
        private final Appendable appender;
        private final Writer writer;
        private String holdBack;

        AppendableWriter(Appendable buffer) {
            super(buffer);
            this.appender = buffer;
            this.writer = null;
        }

        AppendableWriter(Writer writer) {
            super(writer);
            this.appender = null;
            this.writer = writer;
        }

        void setHoldBack(String holdBack) {
            this.holdBack = holdBack;
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            if (holdBack != null) {
                if (appender != null) {
                    appender.append(holdBack);
                } else if (writer != null) {
                    writer.write(holdBack);
                }
                holdBack = null;
            }

            if (appender != null) {
                appender.append(String.valueOf(cbuf), off, len);
            } else if (writer != null) {
                writer.write(cbuf, off, len);
            }
        }

        @Override
        public void flush() throws IOException {
            Object o = (appender != null) ? appender : writer;
            if (o instanceof Flushable) {
                ((Flushable)o).flush();
            }
        }

        @Override
        public void close() throws IOException {
            flush();
            Object o = (appender != null) ? appender : writer;
            if (o instanceof Closeable) {
                ((Closeable)o).close();
            }
        }
    }
}
