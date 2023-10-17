package org.enso.parsinginjs;

import org.enso.compiler.core.EnsoParser;

public class Parsinginjs {

    public static void main(String[] args) throws Throwable {
        Patching.init0();
        var p = new EnsoParser();
        var ir = p.compile("""
        main = 42
        """);
        System.out.println(ir.toString());
    }
}
