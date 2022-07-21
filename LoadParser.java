
package org.enso.syntax2.parser;

import java.io.File;

class LoadParser {
    static {
        File parser = new File("target/rust/debug/libenso_parser.so");
        System.load(parser.getAbsolutePath());
    }
    
    public static void main(String[] args) {
        System.err.println("loaded: " + LoadParser.class.getName());
    }
}
