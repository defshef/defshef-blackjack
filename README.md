# (defshef blackjack)

A practical session for functional programming approaches based on http://buildingskills.itmaybeahack.com/book/oodesign-3.1/html/index.html#blackjack

The link above gives a good overview of the game itself, but neglects to frame this as a problem requiring a solution. It then goes on to model the game in detail, building a complete simulation.

Rather than take this approach, I've split the overall game into a series of sub-problems, which will gradually build up usable chunks of code that can be combined into a full game.

One thing that should become immediately apparent, is that this list is mostly verbs, whereas the original solution is discussed mostly in terms of nouns. I don't think this is necessarily an OO vs FP distinction.

## Sub-problems

 * Modelling the base data
 * Calculate hand value
 * Shuffle and deal
 * Display the game
 * Hit/Stand
 * Betting, winning and losing
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
 * `Number` can be Ace, King, Queen, Jack, 10, 9, 8, 7, 6, 5, 4, 3 or 2
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
* JH 8C 9S = 27, bust
* AS KS = 21, blackjack
* AS 5H = 16, soft
* AS 5H 7C = 13, hard
* AS AC = 12, hard

### Shuffle and deal

We need some way of representing the deck of cards, and then shuffling it. Ideally just use a standard collection type, but we'll need a bit of code to generate all the possible cards.

Once we have a shuffled deck, we need to deal. For now we'll just worry about one player against the dealer. Initially they need two cards each, dealt one at a time alternately. We should put this into some sort of datastructure to represent the state of the table (currently 3 fields: the deck, the dealer and the player).

# License

Copyright Glen Mailer

MIT Licensed
