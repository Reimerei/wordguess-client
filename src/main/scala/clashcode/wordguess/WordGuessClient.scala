package clashcode.wordguess

import akka.actor._
import akka.event.Logging
import scala.collection.mutable

import messages._
import java.io.FileWriter

class WordGuesserClient(playerName: String, gameServer: ActorRef) extends Actor {
      
  // IMPORTANT: 
  // 1) start by requesting a game to the server
  // 2) the workflow is ASYNCHRONOUS; so don't participate in a game
  // until you know you are in one.

  // Main methods at your disposal:
  requestGame()
  // makeGuess('a')

  val gameStateActor = context.system.actorOf(Props(classOf[WordGuessState]))
  var maxCount  = 0

  override def receive = {

    case status: GameStatus => {
      maxCount = Math.max(maxCount, status.gameId)
      gameStateActor ! new Guess(status.letters, status.gameId)
    }

    case nextTry: Char => {
      makeGuess(nextTry)
    }

    case GameWon(status) => {
      gameStateActor ! new Solution(status.letters, status.gameId)
      requestGame()
    }

    case GameLost(status) => {

      gameStateActor ! new GameLostMessage(status.gameId, gameServer)
      requestGame()
    }


    // If there are no more available games (rare, but could happen)
    case NoAvailableGames() => {
      println("moep")
      requestGame()
    }
    // If the client (you) made a guess although no game was requested (or is over)
    case NotPlayingError() => {
      println("FAIL")
      requestGame()
    }
    // When an chat message arrives 
    case MsgToAll(msg) => {
      println(msg)
    }
  }

  // Request a game from the server; start by doing this
  def requestGame() {
    gameServer ! RequestGame(playerName)
  }
  // You try to guess the word by making guesses
  def makeGuess(letter: Char) {
    gameServer ! MakeGuess(letter)
  }
  // You can stop your local app with this (shutdown the actor-system)
  def stopApplication() {
    context.system.shutdown()
  }
  // You can send a message to all other players (to chat?)
  def broadCastMsg(msg: String) {
    gameServer ! SendToAll(msg)
  }

}