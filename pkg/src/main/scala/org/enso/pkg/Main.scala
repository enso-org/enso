package org.enso.pkg

import java.io.File

object Main extends App {
  override def main(args: Array[String]): Unit = {
    Package.getOrCreate(
      new File("/Users/marcinkostrzewa/765Luna__$%%$#Project")
    )
    Package.getOrCreate(
      new File("/Users/marcinkostrzewa/proper_%%$##%#project")
    )
    Package.getOrCreate(new File("/Users/marcinkostrzewa/Properproject"))
  }
}
