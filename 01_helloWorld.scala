package helloWorld
// https://getkyo.io/#/

import kyo.*

val init =
  Console.printLine("Hello!")

val WHAT_IS_YOUR_NAME =
  Console.print("Please type your name? > ") *> Console.readLine

def hello(name: String) =
  Console.printLine(s"--- AHOY ${name.toUpperCase}! ---")

//--- --- --- ---

val program_map_map =
  init.map: Unit =>
    WHAT_IS_YOUR_NAME.map: name =>
      hello(name)

val program_for_comprehension =
  for
    _    <- init
    name <- WHAT_IS_YOUR_NAME
  yield hello(name)

val program_single_line =
  init.andThen(WHAT_IS_YOUR_NAME).map(hello)

val program_direct =
  direct:
    init.now
    val name = WHAT_IS_YOUR_NAME.now
    hello(name).now

val program_point_free =
  init *> WHAT_IS_YOUR_NAME >>> hello

//--- --- --- ---

object HelloWorld extends KyoApp:
  run:
    program_direct

//Merci Valentin ! https://github.com/vil1/pointless, cela donne envie de faire kyo-pointless
extension [A, S](v: A < S)
  infix def >>>[B, S2](f: A => B < S2): B < (S & S2) = v.map(f)