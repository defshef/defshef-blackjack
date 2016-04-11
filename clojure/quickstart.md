# Clojure Quickstart

 * [Cheatsheet](http://clojure.org/api/cheatsheet)
 * [Introduction](http://clojure-doc.org/articles/tutorials/introduction.html)
 * [Language Overview](https://yogthos.github.io/ClojureDistilled.html)

## Requirements

 * JRE 1.8
 * [leiningen](http://leiningen.org/)

## To begin

```sh
lein new defshef.blackjack
cd defshef.blackjack
```

## Running

We'll use the repl to interact easily with the application

```sh
lein repl
```

You'll then be greeted with a `user=> ` prompt.

```clj
user=> (use 'defshef.blackjack :reload)
nil
```

You can now run functions that you've defined in `src/defshef/blackjack.clj`. Whenever you re-run that `(use)` line, your latest code will be loaded in.

## Want tests?

I recommend [Expectations](http://jayfields.com/expectations/).

### Install

```sh
lein change :dependencies conj '[expectations "2.0.9" :exclusions [org.clojure/clojure]]'
lein change :plugins conj '[lein-expectations "0.0.7"]'
```

### Require

Add the require to your namespace declaration

```clj
(ns defshef.blackjack
  (:require [expectations :refer :all]))
```

You can now write tests alongside your code as shown in the expectations docs.

### Run tests

On the commandline

```sh
lein expectations
```

In the repl

```clj
(expectations/run-all-tests)
```
