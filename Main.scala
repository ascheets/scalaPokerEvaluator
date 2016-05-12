//trait giving access to card suit value maps
trait NeedsCardTypes {

  var suits: Map[Int, String] = Map()

  suits += (1 -> "Clubs")
  suits += (2 -> "Spades")
  suits += (3 -> "Hearts")
  suits += (4 -> "Diamonds")

  var values: Map[Int, String] = Map()

  values += (1 -> "Ace")
  values += (2 -> "Two")
  values += (3 -> "Three")
  values += (4 -> "Four")
  values += (5 -> "Five")
  values += (6 -> "Six")
  values += (7 -> "Seven")
  values += (8 -> "Eight")
  values += (9 -> "Nine")
  values += (10 -> "Ten")
  values += (11 -> "Jack")
  values += (12 -> "Queen")
  values += (13 -> "King")

}

//card class, value = ace, 2, 3, ..., king
class Card(val _suit: Int, val _value: Int) extends NeedsCardTypes {

  private var suit: Int = _suit
  private var value: Int = _value

  private var played: Boolean = false

  def setPlayed(_played: Boolean) {

    played = _played

  }

  def print() {

    println(values(value) + " of " + suits(suit))

  }


}

//deck class
class Deck {

  //deck initialization code

  //array to hold all cards in deck
  private val cards = new Array[Card](52)
  
  //private vars for deck configuring
  private var suit = 0;
  private var value = 0;
  private var count = 0;

  //cycle through 13 card values for four suits
  for(suit <- 1 to 4){

    for(value <- 1 to 13){

      //add card with this suit and value to the deck
      cards(count) = new Card(suit, value)

      //increase location in the deck
      count = count + 1

    }

  }

  //print all the cards in the deck
  def print() {

    for(c <- cards){

      c.print

    }

  }

}

class PokerEvaluator(val _numCards: Int) {

  private var numHands: Int = 0

  private

  def setNumberOfHandsToPlay(_numHands: Int) {

    numHands = _numHands

  }

  def playAndDisplay() {

    println("nothing to see here")

  }

  def addCardToHand(_value: Int, _suit: String) {

    println("nothing to see here")

  }


}

//this is the main module where
//main function is kept
object Main {

  def main(args: Array[String]) {

    val Deck = new Deck


    }
}
