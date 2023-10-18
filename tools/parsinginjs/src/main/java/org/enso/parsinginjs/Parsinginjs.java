package org.enso.parsinginjs;

import org.enso.compiler.core.EnsoParser;

public class Parsinginjs {
    static {
        try {
            Patching.init0();
        } catch (Throwable ex) {
            ex.printStackTrace();
        }
    }

    public static String parse(String code) {
        var p = new EnsoParser();
        var ir = p.compile(code);
        return ir.pretty();
    }

    public static void main(String[] args) throws Throwable {
        var ir = parse("""
        main = 42
        """);
        System.out.println(ir);
    }
}
