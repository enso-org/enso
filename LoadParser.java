
package org.enso.syntax2.parser;

import java.io.File;

class LoadParser {
    static {
        File parser = new File("target/rust/debug/libenso_parser.so");
        System.load(parser.getAbsolutePath());
    }
    
    private static native String hello(String name);
    
    public static void main(String[] args) {
        System.out.println("loaded: " + LoadParser.class.getName());
        var r = hello("Enso");
        System.out.println("Native method said: " + r);
    }
}
