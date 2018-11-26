package scanner

import scala.io.Source
import java.io.File

abstract class Token
case class PLUS_TOKEN() extends Token
case class MINUS_TOKEN() extends Token
case class L_PARENTHESIS_TOKEN() extends Token
case class R_PARENTHESIS_TOKEN() extends Token
case class L_CURLY_TOKEN() extends Token
case class R_CURLY_TOKEN() extends Token
case class EQUAL_TOKEN() extends Token
case class IF_TOKEN() extends Token
case class ELSE_TOKEN() extends Token
case class PRINT_TOKEN() extends Token
case class LITTERAL_TOKEN(value : String) extends Token
case class NUMBER_TOKEN(value : Int) extends Token
case class BRAINFUCK_TOKEN(value : String) extends Token

object Scanner {

  def getTokens(name : String) : List[Token] = {
    def tokenize(s : String) : Token = s match {
      case "{" => L_CURLY_TOKEN()
      case "}" => R_CURLY_TOKEN()
      case "(" => L_PARENTHESIS_TOKEN()
      case ")" => R_PARENTHESIS_TOKEN()
      case "=" => EQUAL_TOKEN()
      case "+" => PLUS_TOKEN()
      case "-" => MINUS_TOKEN()
      case "if" => IF_TOKEN()
      case "else" => ELSE_TOKEN() 
      case "print" => PRINT_TOKEN()
      case variable => if(variable.charAt(0).isDigit) NUMBER_TOKEN(variable.toInt)
                       else if(variable.charAt(0).isLetter) LITTERAL_TOKEN(variable)
                       else BRAINFUCK_TOKEN(variable)
    }
    val code = Source.fromFile(name).mkString.filter((char) => char != ' ' && char != '\t' && char != '\n')
                                             .replace("{", "~{~")
                                             .replace("}", "~}~")
                                             .replace("(", "~(~")
                                             .replace(")", "~)~")
                                             .replace("+", "~+~")    
                                             .replace("-", "~-~")    
                                             .replace("=", "~=~")    
                                             .split('~').toList.filter(_ != "")
    code.map(tokenize)
  } 

}