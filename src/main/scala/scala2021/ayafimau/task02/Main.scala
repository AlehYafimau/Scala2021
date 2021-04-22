package scala2021.ayafimau.task02

import scala.annotation.tailrec

object Main extends App {

  val testStrings = Array("if((2+x)*(3-y)==3)",
    "Я сказал ему (это еще (не) сделано). (Но он не послушал)",
    ":-)",
    "())("
  )
  testStrings.foreach(x => println(isBalancedParentheses(x.toList)))

  def isBalancedParentheses(source: List[Char]): Boolean = {

    @tailrec def isBalancedInternal(sourceTail: List[Char], parenthAccum: List[Char]): Boolean = {
      sourceTail match {
        case '(' :: rest => isBalancedInternal(rest, '(' :: parenthAccum)
        case ')' :: rest => parenthAccum match {
          case '(' :: restOfAccum => isBalancedInternal(rest, restOfAccum)
          case _ => false
        }
        case Nil => true
        case _ :: rest => isBalancedInternal(rest, parenthAccum)
      }
    }

    isBalancedInternal(source, Nil)
  }
}