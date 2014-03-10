package clashcode.wordguess

import akka.actor.{ActorRef, Actor}
import java.io.FileWriter
import play.api.libs.json.{JsObject, Reads, Json, Writes}
import clashcode.wordguess.messages.SendToAll

/**
 * User: Björn Reimer
 * Date: 3/8/14
 * Time: 12:59 AM
 */

case class Word(state: Seq[Option[Char]], notTried: List[Char], id: Int)

object Word {

  def getLetters: List[Char] = "ETAONISHRLDUCMWYFGPBVKJXQZ".toList

  implicit val writes: Writes[Word] = Writes[Word] { word =>
    Json.obj("works" -> word.state.map{_.getOrElse(".")}.mkString) ++
    Json.obj("notWorks" -> getLetters.diff(word.notTried ++ word.state.filter(_.isDefined).map(_.get).toList).mkString) ++
    Json.obj("id" -> word.id) ++
    Json.obj("from" -> "reimerei")
  }

  def fromJson(js: JsObject):Word = {
    val state : Seq[Option[Char]] = Seq.fill(50){None}
    val notWorks: List[Char] = (js \ "notWorks").asOpt[String].getOrElse("").toList
    val works: List[Char] = (js \ "works").asOpt[String].getOrElse("").toList
    val notTried: List[Char] = works ++ getLetters.diff(notWorks)
    val id: Int = (js \ "id").asOpt[Int].getOrElse(0)
    new Word(state, notTried, id)
  }
}


case class Guess(letters: Seq[Option[Char]], id: Int)

case class Solution(letters: Seq[Option[Char]], id: Int)
case class GameLostMessage(id: Int, gameServer: ActorRef)



class WordGuessState extends Actor {

  var successCount = 0

    def getLetters(i: Int): List[Char] = "ETAONISHRLDUCMWYFGPBVKJXQZ".toList
//  def getLetters(wordLength: Int): List[Char] = {
//    val string = wordLength match {
//      case 1 => "AI"
//      case 2 => "AOEIMHNUSTYBLPXDFRWGJK"
//      case 3 => "AEOITSUPRNDBGMYLHWFCKXVJZQ"
//      case 4 => "AESOIRLTNUDPMHCBKGYWFVJZXQ"
//      case 5 => "SEAROILTNUDCYPMHGBKFWVZXJQ"
//      case 6 => "ESARIOLNTDUCMPGHBYKFWVZXJQ"
//      case 7 => "ESIARNTOLDUCGPMHBYFKWVZXJQ"
//      case 8 => "ESIARNTOLDCUGMPHBYFKWVZXQJ"
//      case 9 => "ESIRANTOLCDUGMPHBYFVKWZXQJ"
//      case 10 => "EISRANTOLCDUGMPHBYFVKWZXQJ"
//      case 11 => "EISNARTOLCUDPMGHBYFVKWZXQJ"
//      case 12 => "EISNTAROLCPUMDGHYBVFZKWXQJ"
//      case 13 => "IENTSAORLCPUMGDHYBVFZXKWQJ"
//      case 14 => "IETSNAORLCPUMDHGYBVFZXKWQJ"
//      case 15 => "IETNSOARLCPUMDHGYBVFZXWKQJ"
//      case 16 => "IETSNAORLCPUMHDYGBVFZXWQKJ"
//      case 17 => "IETNSOARLCPUMHDGYBVFZXQWJK"
//      case 18 => "ISETONRALCPMUHDGYBVZFXQWKJ"
//      case 19 => "IETONASRLCPMUHDGYBVFZXKJQW"
//      case _ =>  "IOETRSANCLPHUMYDGBZVFKXJQW"
//    }
//    string.toList
//  }

  implicit var globalState: List[Word] = List()

  def receive = {
    case guess: Guess =>
      sender ! nextTry(guess)
    case solution: Solution =>
      val word = solution.letters.map {
        _.get
      }.mkString
      successCount += 1
      globalState = globalState.updated(
        globalState.indexWhere(_.id == solution.id),
        new Word(solution.letters, solution.letters.map {
          _.get
        }.toList, solution.id)
      )
    case GameLostMessage(id, gameServer) => {
      globalState.find(_.id == id) match {
        case None => println("fail")
        case Some(w) => gameServer ! SendToAll(Json.toJson(w).toString)
      }
    }
  }

  def nextTry(guess: Guess): Char = {
    val (word, rest) = globalState.partition(_.id == guess.id)

    word.length match {
      case 0 =>
        val letters = getLetters(guess.letters.length)
        globalState = rest :+ new Word(guess.letters, letters.tail, guess.id)
        letters.head
      case 1 =>
        val newState = word(0).state.zip(guess.letters).map {
          case (s, n) => s.orElse(n)
        }

        val (next: Char, notTried: List[Char]) = newState.forall(_.isDefined) match {
          case false => (word(0).notTried.head, word(0).notTried.tail)
          case true =>
            val result = newState.map(_.get)
            result.forall(!word(0).notTried.contains(_)) match {
              case true => (result.head, result.tail.toList)
              case false => (word(0).notTried.head, word(0).notTried.tail)
            }
        }

        println(
          "Stored: " + globalState.length +
            " success: " +successCount +
            "  ::   " + newState.map(_.getOrElse('.')).mkString + "  ::" +
            " notTried: " + notTried.mkString)

        val newWord = word(0).copy(notTried = notTried, state = newState)
        globalState = rest :+ newWord
        next
      case _ => println("more than one match for gameid"); 'a'
    }
  }
}
