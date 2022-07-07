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



// ============================
// === Java Test Generation ===
// ============================

fn main() {
    let cases = enso_parser_generate_java::generate_testcases();
    let fmt_cases = |cases: &[Vec<u8>]| {
        let cases: Vec<_> = cases
            .iter()
            .map(|case| {
                let case: Vec<_> = case.iter().map(|byte| (*byte as i8).to_string()).collect();
                format!("{{{}}}", case.join(", "))
            })
            .collect();
        cases.join(", ")
    };
    let accept = fmt_cases(&cases.accept);
    let reject = fmt_cases(&cases.reject);
    let package = enso_parser_generate_java::PACKAGE;
    let serialization = enso_parser_generate_java::SERIALIZATION_SUPPORT;
    println!("import {package}.Tree;");
    println!("import {serialization}.Message;",);
    println!("import java.nio.ByteBuffer;");
    println!("import java.nio.ByteOrder;");
    println!();
    println!("class GeneratedFormatTests {{");
    println!("    public static void main(String[] args) {{");
    println!("        byte[][] accept = {{{accept}}};");
    println!("        byte[][] reject = {{{reject}}};");
    println!("        int result = 0;");
    println!("        for (int i = 0; i < accept.length; i++) {{");
    println!("            ByteBuffer buffer = ByteBuffer.wrap(accept[i]);");
    println!("            buffer.order(ByteOrder.LITTLE_ENDIAN);");
    println!("            ByteBuffer context = ByteBuffer.allocate(0);");
    println!("            Message message = new Message(buffer, context, 0);");
    println!("            try {{");
    println!("                Tree tree = Tree.deserialize(message);");
    println!("                System.out.print(\"- pass: \");");
    println!("                System.out.println(tree.toString());");
    println!("            }} catch (RuntimeException e) {{");
    println!("                System.out.println(\"- fail:\");");
    println!("                e.printStackTrace();");
    println!("                result = 1;");
    println!("            }}");
    println!("        }}");
    println!("        for (int i = 0; i < reject.length; i++) {{");
    println!("            ByteBuffer buffer = ByteBuffer.wrap(reject[i]);");
    println!("            buffer.order(ByteOrder.LITTLE_ENDIAN);");
    println!("            ByteBuffer context = ByteBuffer.allocate(0);");
    println!("            Message message = new Message(buffer, context, 0);");
    println!("            try {{");
    println!("                Tree tree = Tree.deserialize(message);");
    println!("                System.out.print(\"- fail: accepted: \");");
    println!("                System.out.println(tree.toString());");
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
