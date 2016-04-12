# Haskell Quickstart

 * [Learn Haskell in 10 minutes](https://wiki.haskell.org/Learn_Haskell_in_10_minutes)
 * [Find an appropriate tutorial](https://wiki.haskell.org/Meta-tutorial)
 * [Find available functions](https://www.haskell.org/hoogle/)

## Installing

 * [Stack](http://docs.haskellstack.org/en/stable/README/)

## To begin

```sh
stack new defshef-blackjack
cd defshef-blackjack
stack setup
```

## Running

We'll use the interactive prompt to interact easily with the application

```sh
stack ghci
```

You'll then be greeted with a prompt.

You can now run functions that you've defined in `defshef-blackjack/src/Lib.hs`.

To pull in the latest code from disk use `:reload`.
