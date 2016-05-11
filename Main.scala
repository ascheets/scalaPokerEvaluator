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

  var suit: Int = _suit
  var value: Int = _value

  var played: Boolean = false

  def setPlayed(_played: Boolean) {

    played = _played

  }

  def print() {

    println(values(value) + " of " + suits(suit))

  }


}

//deck class
class Deck {

  val cards = new Array[Card](52)
  
  var suit = 0;
  var value = 0;
  var count = 0;

  for(suit <- 1 to 4){

    for(value <- 1 to 13){

      cards(count) = new Card(suit, value)

      count = count + 1

    }

  }

  for(c <- cards){

    c.print

  }


}

//this is the main module where
//main function is kept
object Main {

  def main(args: Array[String]) {

    val Deck = new Deck


    }
}
