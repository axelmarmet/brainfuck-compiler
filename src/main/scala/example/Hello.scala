package example

import scanner.Scanner
import java.io.File
import java.io._


object Hello extends Greeting with App {
  Scanner.getTokens("src/main/resources/test.txt").foreach((token) => println(token.getClass.getName))
}

trait Greeting {
  lazy val greeting: String = "hello"
}
