package helloWorld

import java.io.IOException
import kyo.* // https://getkyo.io/#/

val init: Unit < Sync = Console.printLine("Hello!")

val WHAT_IS_YOUR_NAME: String < (Sync & Abort[IOException]) =
  Console.print("Please type your name? > ").andThen(Console.readLine)

def hello(name: String): Unit < Sync =
  Console.printLine(s"--- AHOY ${name.toUpperCase}! ---")

//--- --- --- ---

// map map map
val program_0: Unit < (Sync & Abort[IOException]) =
  init.map: Unit =>
    WHAT_IS_YOUR_NAME.map: name =>
      hello(name)

// for-comp
val program_1 =
  for
    _    <- init
    name <- WHAT_IS_YOUR_NAME
  yield hello(name)

// single line
val program_2 =
  init.andThen(WHAT_IS_YOUR_NAME).map(hello)

// direct-syntax
val program_3 =
  direct:
    init.now
    val name = WHAT_IS_YOUR_NAME.now
    hello(name).now

// point free?
val program_4 =
  init *> WHAT_IS_YOUR_NAME >>> hello

//--- --- --- ---

object HelloWorld extends KyoApp:
  run:
    program_3

//Merci Valentin ! https://github.com/vil1/pointless, cela donne envie de faire kyo-pointless
extension [A, S](v: A < S)
  infix def >>>[B, S2](f: A => B < S2): B < (S & S2) = v.map(f)