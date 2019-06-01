
// import org.enso.Yylex

package org.enso.syntax.text.lexer
import java.io.{StringReader, StringWriter}
// import org.enso.syntax.text.lexer.{Token}

object Main extends App {
  println("Hello, World!!")
  val firstFilePos = 0
  val encodingName = "UTF-8"
  // if (argv[0].equals("--encoding")) {
  //   firstFilePos = 2
  //   encodingName = argv[1]
  //   try {
  //     java.nio.charset.Charset.forName(encodingName) // Side-effect: is encodingName valid? 
  //   } catch (Exception e) {
  //     System.out.println("Invalid encoding '" + encodingName + "'")
  //     return
  //   }
  // }
  // for (int i = firstFilePos i < argv.length i++) {
    // Yylex scanner = null
    // // try {
    //   // java.io.FileInputStream stream = new java.io.FileInputStream(argv[i])
    //   // java.io.Reader reader = new java.io.InputStreamReader(stream, encodingName)
      val reader = new StringReader("15 17")
      val scanner = new Lexer(reader)
      do {
        System.out.println(scanner.yylex())
      } while (!scanner.done())

    // }
    // catch (java.io.FileNotFoundException e) {
    //   System.out.println("File not found : \""+argv[i]+"\"")
    // }
    // catch (java.io.IOException e) {
    //   System.out.println("IO error scanning file \""+argv[i]+"\"")
    //   System.out.println(e)
    // }
    // catch (Exception e) {
    //   System.out.println("Unexpected exception:")
    //   e.printStackTrace()
    // }
  // }
}