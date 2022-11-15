
import org.enso.syntax2.Parser;

/**
 * <h1>Verify this runs</h1>
 * Use:
 * <pre>
 * lib/rust/parser/target/scala-2.13/classes$ /graalvm/bin/javac Load.java
 * lib/rust/parser/target/scala-2.13/classes$ /graalvm/bin/java Load
 * </pre>
 * to execute the parser and print the result.
 *
 * <h1>Try Native Image</h1>
 * Use:
 * <pre>
 * lib/rust/parser/target/scala-2.13/classes$ /graalvm/bin/javac Load.java
 * lib/rust/parser/target/scala-2.13/classes$ /graalvm/bin/native-image -cp . Load && ./load
 * </pre>
 * 
 * <h1>Try more complex Native Image</h1>
 * <pre>
 * $ /graalvm/bin/javac Load.java
 * $ /graalvm/bin/native-image \
 *   --initialize-at-build-time \
 *   --initialize-at-run-time=org.enso.syntax2.Parser \
 *   -cp . Load
 * </pre>
 */
class Load {
  static {
    System.out.println("Initializing Load");
  }
  public static void main(String... args) {
    var tree = Parser.create().parse("main = 42");
    System.out.println(tree);
  }
}