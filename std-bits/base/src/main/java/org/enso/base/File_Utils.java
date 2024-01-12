package org.enso.base;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.enso.base.arrays.LongArrayList;
import org.graalvm.polyglot.Context;

public class File_Utils {
    private static ByteArrayOutputStream reusedLine = new ByteArrayOutputStream();

    //** Scan a file to find the next line break. */
    public static void scanLine(RandomAccessFile file) throws IOException {
        int c = file.read();
        while (c != -1 && c != '\n') {
            if (c == '\r') {
                c = file.read();
                if (c != '\n') {
                    file.seek(file.getFilePointer() - 1);
                }
                return;
            }

            c = file.read();
        }
    }

    //** Read a single line from the file at the current location. */
    public static synchronized String readLine(RandomAccessFile file, Charset charset) throws IOException {
        // Avoid allocating over and over.
        reusedLine.reset();

        int c = file.read();
        while (c != -1 && c != '\n') {
            if (c == '\r') {
                c = file.read();
                if (c != '\n') {
                    file.seek(file.getFilePointer() - 1);
                }
                break;
            }

            reusedLine.write(c);
            c = file.read();
        }

        return reusedLine.toString(charset);
    }

    public static void scanLines(RandomAccessFile file, LongArrayList rowMap, int endAt) throws IOException {
        long end = file.length();
        long size = rowMap.getSize();
        while (size <= endAt && file.getFilePointer() < end) {
            if (size % 100000 == 0) {
                Logger.getLogger("File_Utils.scanLines").log(Level.INFO, "Scanned Lines: {0}", size);
            }
            scanLine(file);
            rowMap.add(file.getFilePointer());
            size++;
            Context.getCurrent().safepoint();
        }

        if (size <= endAt && rowMap.get(rowMap.getSize()-1) != file.getFilePointer()) {
            rowMap.add(file.getFilePointer());
        }
    }
}
