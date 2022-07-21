package org.enso.syntax2;

import org.enso.syntax2.serialization.Message;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

class LoadParser {
    static {
        File parser = new File("target/rust/debug/libenso_parser.so");
        System.load(parser.getAbsolutePath());
    }
    
    private static native ByteBuffer hello(ByteBuffer buf);
    
    public static void main(String[] args) {
        System.out.println("loaded: " + LoadParser.class.getName());
        ByteBuffer buf = ByteBuffer.allocateDirect(256);
        buf.asCharBuffer().put("foo a b c = 23");
        var r = hello(buf);
        for (var i = 0; i < r.limit(); i++) {
            if (i % 16 != 0) {
                System.out.print(" ");
            } else {
                System.out.print("\n");
            }
            int b = r.get(i);
            if (b < 0) {
                b += 256;
            }
            var txt = ("00" + Integer.toHexString(b));
            System.out.print(txt.substring(txt.length() - 2));
        }
        System.out.print("\n");
        Message message = new Message(r.order(ByteOrder.LITTLE_ENDIAN), buf, 0);
        Tree tree = Tree.deserialize(message);
        System.out.println("Native method said: " + tree);
    }
}
