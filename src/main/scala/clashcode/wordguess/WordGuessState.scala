package clashcode.wordguess

import akka.actor.Actor
import java.io.FileWriter

/**
 * User: Björn Reimer
 * Date: 3/8/14
 * Time: 12:59 AM
 */

case class Word(state: Seq[Option[Char]], notTried: List[Char], id: Int)


case class Guess(letters: Seq[Option[Char]], id: Int)
case class Solution(letters: Seq[Option[Char]], id: Int)

class WordGuessState extends Actor {

  var maxCount = 0
  var allLetters: List[Char] = "ETAONISHRLDUCMWYFGPBVKJXQZ".toList

  implicit var globalState: List[Word] = List()

  def receive = {
    case guess: Guess =>
      maxCount = Math.max(maxCount, guess.id)
      sender ! nextTry(guess)
    case solution: Solution =>
      val word = solution.letters.map{_.get}.mkString
      println("SUCCESS: " + word)
      val fw = new FileWriter("words")
      fw.append(word + "\n")
      fw.close()

      maxCount = Math.max(maxCount, solution.id)
      globalState = globalState.updated(
        globalState.indexWhere(_.id == solution.id),
        new Word(solution.letters, solution.letters.map{_.get}.toList, solution.id)
      )
  }

  def nextTry(guess: Guess): Char = {
    val (word, rest) = globalState.partition(_.id == guess.id)

    word.length match {
      case 0 =>
        globalState = rest :+ new Word(guess.letters, allLetters.tail, guess.id)
        allLetters.head
      case 1 =>
        val (next: Char, notTried: List[Char]) = word(0).state.forall(_.isDefined) match {
          case false => (word(0).notTried.head, word(0).notTried.tail)
          case true =>
            val result = word(0).state.map {
              _.get
            }
//            println("found result: ----- " + result.mkString + " -----")
            result.forall(!word(0).notTried.contains(_)) match {
              case true => (result.head, result.tail.toList)
              case false =>(word(0).notTried.head, word(0).notTried.tail)
            }
        }
        println("Stored: "+ globalState.length + " of " + maxCount + " - id: " + guess.id + " state: " + word(0).state.map {
          _.getOrElse('.')
        }.mkString + "  \tnotTried: " + notTried.mkString)
        val newState = word(0).state.zip(guess.letters).map {
          case (s, n) => s.orElse(n)
        }
        val newWord = word(0).copy(notTried = notTried, state = newState)
        globalState = rest :+ newWord
        next
      case _ => println("more than one match for gameid"); 'a'
    }
  }


}
