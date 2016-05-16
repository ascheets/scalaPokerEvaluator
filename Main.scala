import util.Random
import scala.util.Sorting

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
class Card(val _suit: Int, val _value: Int) extends NeedsCardTypes with Ordered[Card] {

  //private class members
  private var suit: Int = _suit
  private var value: Int = _value

  private var played: Boolean = false

  //this function allows sorting of hands
  def compare(that: Card) = this._value - that._value

  //getters and setters per usual
  def getSuit() : Int = {

    return suit
  }

  def getValue() : Int = {

    return value
  }

  def setPlayed(_played: Boolean) {

    played = _played

  }

  def getPlayed() : Boolean = {

    return played

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

  def reset() {

    for(x <- cards){

      x.setPlayed(false)

    }

  }

  def getIndex(suit : Int, value : Int) : Int = {

    var curIndex = 0;
    var retVal = 0;

    for(x <- cards){

      if(x.getSuit == suit && x.getValue == value) {

        retVal = curIndex

      }

      curIndex = curIndex + 1

    }

    return retVal

  }

  def at(index : Int) : Card = {    

    return cards(index)

  }

  //return the length of the deck
  def length() : Int = {

    return cards.length

  }

  //print all the cards in the deck
  def print() {

    for(c <- cards){

      c.print

    }

  }

}

class PokerEvaluator(val _numCards: Int) {

  var rand = new scala.util.Random

  var handTallies: Map[String, Int] = Map()

  handTallies += ("Royal Flush" -> 0)
  handTallies += ("Straight Flush" -> 0)
  handTallies += ("Four of a kind" -> 0)
  handTallies += ("Full house" -> 0)
  handTallies += ("Flush" -> 0)
  handTallies += ("Straight" -> 0)
  handTallies += ("Three of a kind" -> 0)
  handTallies += ("Two pair" -> 0)
  handTallies += ("Pair" -> 0)
  handTallies += ("High Card" -> 0)

  //number of hands to play
  private var numHands: Int = 100

  //number of cards in each hand
  private var numCards = _numCards

  //number of cards already in hand
  private var numInHand = 0

  //deck for use
  private val deck = new Deck

  //array to hold the hand of cards
  private var hand = new Array[Card](numCards)

  def setNumberOfHandsToPlay(_numHands: Int) {

    numHands = _numHands

  }

  def playAndDisplay() {

    //this will play all hands
    generateHands

    //display
    for((k,v) <- handTallies){

      var odds = v.toDouble/numHands.toDouble

      println(k + ": " + odds)

    }

  }

  def addCardToHand(_value: Int, _suit: String) {

    var trueSuit = 0;

    //convert suit string to int val
    if(_suit == "Clubs"){

      trueSuit = 1

    }
    else if(_suit == "Spades") {

      trueSuit = 2

    }
    else if(_suit == "Hearts") {

      trueSuit = 3

    }
    else if(_suit == "Diamonds") {

      trueSuit = 4

    }

    if(trueSuit != 0){

      //find index of new card in deck
      var index = deck.getIndex(trueSuit, _value);

      //has the card been played?
      if(deck.at(index).getPlayed == false) {

        hand(numInHand) = deck.at(index)
        deck.at(index).setPlayed(true)

        numInHand = numInHand + 1

      }

    }
    else {

      println("One of your input cards was not recognized, check your input params")

    }

  }

  //function if added card is jack, queen, king, etc...
  def addCardToHand(_value: String, _suit: String) {

    var newVal = 0

    //convert the input string to int value
    if(_value == "King") {

      newVal = 13

    }
    else if(_value == "Queen") {

      newVal = 12

    }
    else if(_value == "Jack") {

      newVal = 11

    }

    var trueSuit = 0;

    //convert suit string to int val
    if(_suit == "Clubs"){

      trueSuit = 1

    }
    else if(_suit == "Spades") {

      trueSuit = 2

    }
    else if(_suit == "Hearts") {

      trueSuit = 3

    }
    else if(_suit == "Diamonds") {

      trueSuit = 4

    }

    if(trueSuit != 0 && newVal != 0){

      //find index of new card in deck
      var index = deck.getIndex(trueSuit, newVal);

      //has the card been played?
      if(deck.at(index).getPlayed == false) {

        hand(numInHand) = deck.at(index)
        deck.at(index).setPlayed(true)

        numInHand = numInHand + 1

      }

    }
    else {

      println("One of your input cards was not recognized, check your input params")

    }

  }

  def generateHands() {

    //overarching loop that deals out all hands
    for(dealt <- 1 to numHands){

      //fill up the hand
      while(numInHand < numCards) {

        //pick a random index in the deck
        var index = Random.nextInt(deck.length);

        //has the card been played?
        if(deck.at(index).getPlayed == false) {

          hand(numInHand) = deck.at(index)
          deck.at(index).setPlayed(true)

          numInHand = numInHand + 1

        }

      }

      //reset the hand!
      //set num cards in hand to zero
      numInHand = 0
      //this function sets all cardsPlayed to false
      deck.reset

      //evaluate the hand
      evaluateHand


      //debugging, print hand
      // for(x <- hand) {

      //   x.print()

      // }

      //println


    }    

  }

  //this function checks type of hand then 
  //updates tally map to show result
  def evaluateHand() {

    //is it a royal flush?
    if(royalFlush){

      // println
      // println("ROYAL FLUSH")
      // println

      //update number of royalFlushes counted
      var cur: Int = handTallies("Royal Flush")
      cur = cur + 1
      handTallies += ("Royal Flush" -> cur)

    }
    else if(straightFlush){

      //println
      //println("STRAIGHT FLUSH")
      //println

      //update number counted
      var cur = handTallies("Straight Flush")
      cur = cur + 1
      handTallies += ("Straight Flush" -> cur)

    }
    else if(fourOfAKind){

      //println
      //println("FOUR OF A KIND")
      //println

      //update number
      var cur = handTallies("Four of a kind")
      cur = cur + 1
      handTallies += ("Four of a kind" -> cur)

    }
    else if(fullHouse){

      //println
      //println("FULLHOUSE")
      //println

      //update number counted
      var cur = handTallies("Full house")
      cur = cur + 1
      handTallies += ("Full house" -> cur)

    }
    else if(flush){

      //println
      //println("FLUSH")
      //println

      //update number counted
      var cur = handTallies("Flush")
      cur = cur + 1
      handTallies += ("Flush" -> cur)

    }
    else if(straight){

      //println
      //println("STRAIGHT")
      //println

      //update number counted
      var cur = handTallies("Straight")
      cur = cur + 1
      handTallies += ("Straight" -> cur)

      // println

      // //debugging, print hand
      // for(x <- hand) {

      //   x.print()

      // }

      // println


    }
    else if(threeOfAKind){

      //println
      //println("THREE OF A KIND")
      //println

      //update number counted
      var cur = handTallies("Three of a kind")
      cur = cur + 1
      handTallies += ("Three of a kind" -> cur)

    }
    else if(twoPair){

      //println
      //println("TWO PAIR")
      //println

      //update number counted
      var cur = handTallies("Two pair")
      cur = cur + 1
      handTallies += ("Two pair" -> cur)

    }
    else if(pair){

      //println
      //println("PAIR")
      //println

      //update number counted
      var cur = handTallies("Pair")
      cur = cur + 1
      handTallies += ("Pair" -> cur)

    }
    else{

      //println
      //println("HIGH CARD")
      //println

      //update number counted
      var cur = handTallies("High Card")
      cur = cur + 1
      handTallies += ("High Card" -> cur)

    }

  }

  def royalFlush() : Boolean = {

    var retVal = false;

    var suit = hand(0).getSuit;

    var numGood = 0;

    //println("Suit: " + suit)

    //for each card in the current hand
    for(x <- hand){

      //if the card is a ten, jack, queen, king or ace...
      if(x.getValue >= 10 || x.getValue == 1){

        //...and isn't an ace
        if(x.getSuit == suit){

          numGood = numGood + 1
          
        }

      }

    }

    if(numGood >= 5) {

      retVal = true

    }


    return retVal

  }

  def straightFlush() : Boolean = {

    var retVal = false;

    var numGood = 1;

    var curSuit = 0;

    //first, sort the hand by value
    scala.util.Sorting.quickSort(hand)

    //for each card in the current hand
    for(x <- 1 to (numCards - 1)){

      if(numGood == 1){

        curSuit = hand(x).getSuit

      }

      var dif = hand(x).getValue - hand(x-1).getValue

      if(dif == 1){

        //extra condition that suit must be same
        if(hand(x-1).getSuit == curSuit && hand(x).getSuit == curSuit) {

          numGood = numGood + 1

        }
        else{

          numGood = 1

        }

      }
      else{

        //since streak broken, reset numGood
        numGood = 1

      }

    }

    if(numGood >= 5) {

      retVal = true

    }

    return retVal


  }

  def fourOfAKind() : Boolean = {

    var retVal = false;

    var handVals: Map[Int, Int] = Map()

    handVals += (1 -> 0)
    handVals += (2 -> 0)
    handVals += (3 -> 0)
    handVals += (4 -> 0)
    handVals += (5 -> 0)
    handVals += (6 -> 0)
    handVals += (7 -> 0)
    handVals += (8 -> 0)
    handVals += (9 -> 0)
    handVals += (10 -> 0)
    handVals += (11 -> 0)
    handVals += (12 -> 0)
    handVals += (13 -> 0)

    //for each card in the current hand
    for(x <- hand){

      //get current num of this value in hand
      var num = handVals(x.getValue());

      //add one
      num = num + 1;

      //update map
      handVals += (x.getValue() -> num)

    }

    for((k,v) <- handVals) {

      if(v >= 4){

        retVal = true

      }

    }

    return retVal



  }

  def fullHouse() : Boolean = {

    var retVal = false;

    var handVals: Map[Int, Int] = Map()

    handVals += (1 -> 0)
    handVals += (2 -> 0)
    handVals += (3 -> 0)
    handVals += (4 -> 0)
    handVals += (5 -> 0)
    handVals += (6 -> 0)
    handVals += (7 -> 0)
    handVals += (8 -> 0)
    handVals += (9 -> 0)
    handVals += (10 -> 0)
    handVals += (11 -> 0)
    handVals += (12 -> 0)
    handVals += (13 -> 0)

    //for each card in the current hand
    for(x <- hand){

      //get current num of this value in hand
      var num = handVals(x.getValue());

      //add one
      num = num + 1;

      //update map
      handVals += (x.getValue() -> num)

    }

    var numPair = 0;
    var thrice = 0;

    for((k,v) <- handVals) {

      if(v == 2){

        numPair = numPair + 1

      }
      if(v == 3){

        thrice = thrice + 1

      }

    }

    if(thrice == 1 && numPair >= 1){

      retVal = true

    }

    return retVal



  }

  def flush() : Boolean = {

    var retVal = false;

    var suit = hand(0).getSuit;

    var numGood = 0;

    //for each card in the current hand
    for(x <- hand){

      //if the card doesn't match the first suit
      if(x.getSuit == suit){

        numGood = numGood + 1

      }

    }

    if(numGood >= 5) {

      retVal = true;

    }

    return retVal

  }

  def straight() : Boolean = {

    var retVal = false;

    var numGood = 1;

    //first, sort the hand by value
    scala.util.Sorting.quickSort(hand)

    //for each card in the current hand
    for(x <- 1 to (numCards - 1)){

      var dif = hand(x).getValue - hand(x-1).getValue

      if(dif == 1){

        numGood = numGood + 1

      }
      else{

        //since streak broken, reset numGood
        numGood = 1

      }

    }

    if(numGood >= 5) {

      retVal = true

    }

    return retVal

  }

  def threeOfAKind() : Boolean = {

    var retVal = false;

    var handVals: Map[Int, Int] = Map()

    handVals += (1 -> 0)
    handVals += (2 -> 0)
    handVals += (3 -> 0)
    handVals += (4 -> 0)
    handVals += (5 -> 0)
    handVals += (6 -> 0)
    handVals += (7 -> 0)
    handVals += (8 -> 0)
    handVals += (9 -> 0)
    handVals += (10 -> 0)
    handVals += (11 -> 0)
    handVals += (12 -> 0)
    handVals += (13 -> 0)

    //for each card in the current hand
    for(x <- hand){

      //get current num of this value in hand
      var num = handVals(x.getValue());

      //add one
      num = num + 1;

      //update map
      handVals += (x.getValue() -> num)

    }

    for((k,v) <- handVals) {

      if(v == 3){

          retVal = true

      }

    }

    return retVal



  }

  def twoPair() : Boolean = {

    var retVal = false;

    var handVals: Map[Int, Int] = Map()

    handVals += (1 -> 0)
    handVals += (2 -> 0)
    handVals += (3 -> 0)
    handVals += (4 -> 0)
    handVals += (5 -> 0)
    handVals += (6 -> 0)
    handVals += (7 -> 0)
    handVals += (8 -> 0)
    handVals += (9 -> 0)
    handVals += (10 -> 0)
    handVals += (11 -> 0)
    handVals += (12 -> 0)
    handVals += (13 -> 0)

    //for each card in the current hand
    for(x <- hand){

      //get current num of this value in hand
      var num = handVals(x.getValue());

      //add one
      num = num + 1;

      //update map
      handVals += (x.getValue() -> num)

    }

    var numPair = 0;

    for((k,v) <- handVals) {

      if(v == 2){

        numPair = numPair + 1

        if(numPair == 2) {
          retVal = true

        }

      }

    }

    return retVal



  }

  def pair() : Boolean = {

    var retVal = false;

    var handVals: Map[Int, Int] = Map()

    handVals += (1 -> 0)
    handVals += (2 -> 0)
    handVals += (3 -> 0)
    handVals += (4 -> 0)
    handVals += (5 -> 0)
    handVals += (6 -> 0)
    handVals += (7 -> 0)
    handVals += (8 -> 0)
    handVals += (9 -> 0)
    handVals += (10 -> 0)
    handVals += (11 -> 0)
    handVals += (12 -> 0)
    handVals += (13 -> 0)

    //for each card in the current hand
    for(x <- hand){

      //get current num of this value in hand
      var num = handVals(x.getValue());

      //add one
      num = num + 1;

      //update map
      handVals += (x.getValue() -> num)

    }

    for((k,v) <- handVals) {

      if(v == 2){

        retVal = true

      }

    }

    return retVal



  }

}

//this is the main module where
//main function is kept
object Main {

  def main(args: Array[String]) {

    val e1 = new PokerEvaluator(5);

    e1.setNumberOfHandsToPlay(100000)

    e1.playAndDisplay


    }
}
