# (defshef blackjack)

A practical session for functional programming approaches based on [this book](http://buildingskills.itmaybeahack.com/book/oodesign-3.1/html/index.html#blackjack). You might also find this non-programmer [overview of blackjack](https://www.pagat.com/banking/blackjack.html) useful.

The link above gives a good overview of the game itself, but neglects to frame this as a problem requiring a solution. It then goes on to model the game in detail, building a complete simulation.

Rather than take this approach, I've split the overall game into a series of sub-problems, which will gradually build up usable chunks of code that can be combined into a full game.

One thing that should become immediately apparent, is that this list is mostly verbs, whereas the original solution is discussed mostly in terms of nouns. I don't think this is necessarily an OO vs FP distinction.

## Pick an implementation language

 * Clojure
    * [Quickstart](./clojure/quickstart.md)
    * [Sample Solution](./clojure)
 * Haskell
    * [Quickstart](./haskell/quickstart.md)
    * (incomplete) [Sample Solution](./haskell)
 * Any Others? Pull requests welcome!

## Sub-problems

 * Modelling the base data
 * Calculate hand value
 * Shuffle and deal
 * Hit/Stand & basic gameplay
 * Display the game
 * Tying it all together
 * Extensions
   * Betting
   * A game session
   * Doubling
   * Splitting
   * Insurance
   * Multiple decks
   * Multiple players

### Modelling the base data

Much like the OO solution, we need to descide how to represent the entities involed in the game. Unlike the OO solution we'll aim to use primariliy the core data types or abstractions that exist in our programming language.

We will also initially define just enough data types for what we know we must care about, we can always extend these further when we discover more about the game later.

 * `Suit` can be Hearts, Clubs, Diamonds or Spades
 * `Rank` can be Ace, King, Queen, Jack, 10, 9, 8, 7, 6, 5, 4, 3 or 2
 * `Card` has a `Suit` and a `Rank`

Rather than attempt to model a `Deck` or a `Hand`, we'll just use collections of `Card`s.

### Calculate hand value

Given a hand of cards, we need to figure out what it's worth. We could treat this as just a number, but to cater for bust, blackjack and hard/soft aces, we'll treat this as a pair of number and optional qualifier.

Here are some examples you can use for testing. Ideally you should turn these into assertions that can be run.

* 2H 2C = 4
* 2H 2C 2D 2S = 4
* 2H 2C 7S 8D = 19
* KH 5C 6S = 21
* KH QC 3S = 23, bust
* 9H AC 3S KS = 23, bust
* JH 8C 9S = 27, bust
* AS KS = 21, blackjack

Getting all the hard/soft cases for aces right can be a bit fiddly, feel free to skip over these for now and come back to it when you have a working game.

* AS 5H = 16, soft
* AS 5H 7C = 13, hard
* AS AC = 12, soft
* AS AC AD AH = 14, soft
* AS AC KC = 12, hard
* AS AC AH AD KC = 14, hard
* AS AC AH AD KC JH = 24, bust
* 10H QC AS = 21, hard

### Shuffle and deal

We need some way of representing the deck of cards, and then shuffling it. Ideally just use a standard collection type, but we'll need a bit of code to generate all the possible cards.

If you're using a language which enforces referential transparency (like Haskell), then shuffling can be a bit tricky. I recommend having a small function which returns a new shuffled deck, and then have your `deal` function accept a shuffled deck. This will stop the randomness from "infecting" all your other functions with this impurity.

Once we have a shuffled deck, we need to deal. For now we'll just worry about one player against the dealer. Initially they need two cards each, dealt one at a time alternately. We should put this into some sort of datastructure to represent the state of the table (currently 3 fields: the deck, the dealer and the player).

### Hit / Stand & basic gameplay

Now that we've got a way to represent the state of the game, we need a way to advance it. We'll write some functions to represent the actions a player can take, creating a brand new version of the game state each time. The tools in most functional languages should push you towards an immutable-data approach like this.

I suggest introducing a new field to indicate what stage the game is in: player, dealer or done. This will allow us to write the logic for playing the dealer's hand independently, rather that having it hidden inside the hit and stand operations

* `hit` Take the card from the top of the deck and give it to the player
* If the player's hand value is bust after a hit, move to dealer stage
* `stand` Stop taking cards, move to dealer stage
* `play-dealer` The dealer takes cards until they have 17 or more, move to the done stage

It will also be useful to write a function which accepts a game in the done stage, and tells us whether the dealer or the player won (or if it was a draw). Here are some scenarios to check:

* dealer 18, player 17 = dealer wins
* player 19, dealer 18 = player wins
* player 18, dealer 18 = draw
* player 15, dealer 25 = player wins
* player 22, dealer 17 = dealer wins
* player 22, dealer 22 = dealer wins
* player blackjack, dealer 21 = player wins
* player 21, dealer blackjack = dealer wins
* player blackjack, dealer blackjack = draw

### Display the game

Now that we have a representation of the underlying data of the game, it'd be nice to show something to our would-be player.

In the previous section we introduced three different game stages, the display of the game will need to be slightly different depending on the game stage.

* `player` - When we're in the player stage we should only show one card from the dealer, and should not report their hand value.
* `dealer` - When in the dealer stage we can show the all of dealer's cards and their hand value
* `done` - When in the done stage we can reveal who the winner is

This is a bit of a rabbithole to implement, but the unicode consortium has designated some glyphs for playing cards. The [section from the book](http://buildingskills.itmaybeahack.com/book/oodesign-3.1/html/blackjack/card_deck_shoe.html#unicode-images) has the details. Although the [wikipedia page](https://en.wikipedia.org/wiki/Playing_cards_in_Unicode) might be easier to use. Alternatively, you can just use short strings like "AH" for ace of hearts etc.

Make sure you don't print out the next cards in the deck!

### Tying it all together

Now comes the tricky bit, especially if you're using a language that enforces purity at the type level.

So far we've implemented various pure functions that hopefully are recognisable as portions of an implementation of a blackjack game, but now we need to combine them together into a playable game. This will involve user input and output, which cannot be pure.

The missing piece we have is deciding which actions to offer the user based on the current state of the game. Along with this we'll need some sort of game loop. First we'll `deal` a hand and display the game to the player. We'll then offer them options until they're done. Once the player has either stood or bust, we'll play out the dealer's hand.

### A playable game!

If you've got this far then you should have a playable (albiet brief) game. For the most part, each section has consisted of writing new functions which build on the ones from before. The following sections expand on the game we have built mostly by modifying and extending.

***********

### Extensions

Currently left as an exercise for the reader.

## License

Copyright Glen Mailer

MIT Licensed
