package org.enso.syntax2;

import org.enso.syntax2.Parser;

class LoadParser {
    public static void main(String[] args) {
        String code = "foo a b c = 23";
        try (Parser p = Parser.create()) {
            Tree tree = p.parse(code);
            System.out.println("parsing produced: " + tree);
        }
    }
}
