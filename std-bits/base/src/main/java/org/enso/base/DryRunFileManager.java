package org.enso.base;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class DryRunFileManager {
    static final Map<String, String> files = new HashMap<>();

    /**
     * Creates a temporary file for the given name.
     *
     * @param name the path to the file to make a temporary file of.
     * @return the path to the temporary file.
     */
    public static String getTemporaryFile(String name) {
        return files.computeIfAbsent(name, k -> {
            if (files.containsValue(k)) {
                // Existing temporary file so return this.
                return k;
            }

            var filename = new File(k).getName();
            var lastDot = filename.lastIndexOf('.');
            var prefix = lastDot == -1 ? filename : filename.substring(0, lastDot);
            var extension = lastDot == -1 ? "" : filename.substring(lastDot);

            try {
                var temp = File.createTempFile(prefix, extension);
                temp.deleteOnExit();
                return temp.getAbsolutePath();
            } catch (IOException e) {
                return null;
            }
        });
    }
}
