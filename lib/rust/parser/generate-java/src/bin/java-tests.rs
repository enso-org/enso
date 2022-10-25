//! Generates Java format tests.
//!
//! Usage:
//! ```console
//! java-tests > GeneratedFormatTests.java
//! javac -d generated-java/ GeneratedFormatTests.java && java GeneratedFormatTests
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]



// ============================
// === Java Test Generation ===
// ============================

fn main() {
    let cases = enso_parser_generate_java::generate_testcases();
    let fmt_case = |case: &[u8]| {
        let case: Vec<_> = case.iter().map(|byte| (*byte as i8).to_string()).collect();
        format!("{{{}}}", case.join(", "))
    };
    let package = enso_parser_generate_java::PACKAGE;
    let serialization = enso_parser_generate_java::SERIALIZATION_SUPPORT;
    println!("package {package};");
    println!();
    println!("import {package}.Tree;");
    println!("import {serialization}.Message;",);
    println!("import java.nio.ByteBuffer;");
    println!("import java.nio.ByteOrder;");
    println!();
    println!("class GeneratedFormatTests {{");
    println!("    private static java.util.Vector<byte[]> accept;");
    println!("    private static java.util.Vector<byte[]> reject;");
    for (i, case) in cases.accept.iter().enumerate() {
        println!("    private static byte[] accept{i}() {{");
        println!("        return new byte[] {};", fmt_case(case));
        println!("    }}");
    }
    for (i, case) in cases.reject.iter().enumerate() {
        println!("    private static byte[] reject{i}() {{");
        println!("        return new byte[] {};", fmt_case(case));
        println!("    }}");
    }
    println!("    static {{");
    println!("        accept = new java.util.Vector<byte[]>();");
    for (i, _) in cases.accept.iter().enumerate() {
        println!("        accept.add(accept{i}());");
    }
    println!("        reject = new java.util.Vector<byte[]>();");
    for (i, _) in cases.reject.iter().enumerate() {
        println!("        reject.add(reject{i}());");
    }
    println!("    }}");
    println!();
    println!("    public static void main(String[] args) {{");
    println!("        int result = 0;");
    println!("        for (byte[] testCase : accept) {{");
    println!("            ByteBuffer buffer = ByteBuffer.wrap(testCase);");
    println!("            buffer.order(ByteOrder.LITTLE_ENDIAN);");
    println!("            CharSequence context = \"\";");
    println!("            Message message = new Message(buffer, context, 0, 0);");
    println!("            try {{");
    println!("                Tree tree = Tree.deserialize(message);");
    println!("                System.out.println(\"- pass\");");
    println!("            }} catch (RuntimeException e) {{");
    println!("                System.out.println(\"- fail:\");");
    println!("                e.printStackTrace();");
    println!("                result = 1;");
    println!("            }}");
    println!("        }}");
    println!("        for (byte[] testCase : reject) {{");
    println!("            ByteBuffer buffer = ByteBuffer.wrap(testCase);");
    println!("            buffer.order(ByteOrder.LITTLE_ENDIAN);");
    println!("            CharSequence context = \"\";");
    println!("            Message message = new Message(buffer, context, 0, 0);");
    println!("            try {{");
    println!("                Tree tree = Tree.deserialize(message);");
    println!("                System.out.println(\"- fail: accepted\");");
    println!("                result = 1;");
    println!("            }} catch ({serialization}.FormatException e) {{");
    println!("                System.out.println(\"- pass: (rejected)\");");
    println!("            }} catch (RuntimeException e) {{");
    println!("                System.out.println(\"- fail: wrong exception: \");");
    println!("                e.printStackTrace();");
    println!("                result = 1;");
    println!("            }}");
    println!("        }}");
    println!("        System.exit(result);");
    println!("    }}");
    println!("}}");
}
