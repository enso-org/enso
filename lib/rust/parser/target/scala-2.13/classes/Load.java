
import org.enso.syntax2.Parser;

class Load {
  public static void main(String... args) {
    var tree = Parser.create().parse("main = 42");
    System.out.println(tree);
  }
}