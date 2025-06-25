package arrowEffect

import kyo.*
import kyo.kernel.ArrowEffect

sealed trait Counter extends ArrowEffect[Const[Counter.Op], Const[Int]]

object Counter:
  enum Op derives CanEqual:
    case Inc, Get

  def increment: Int < Counter =
    ArrowEffect.suspend[Int](Tag[Counter], Op.Inc)

  def incrementDiscard: Unit < Counter =
    increment.unit

  def get: Int < Counter = ArrowEffect.suspend[Int](Tag[Counter], Op.Get)

opaque type Log = Sync

object Log:
  def log(str: String): Unit < Log = Console.printLine(str)

  def run[A, S](v: A < (Log & S)): A < (Sync & S) = v

val lyrics = Seq(
  "Regarde-toi, assise dans l'ombre",
  "À la lueur de nos mensonges",
  "Les mains glacées",
  "Jusqu'à l'ongle")

val program: Unit < (Log & Sync & Counter) =
  direct:
    Console.printLine("=== Kyo - le chemin ===").now

    lyrics.foreach: line =>
      Counter.incrementDiscard.now
      Console.printLine(s"♪ $line").now

    val n: Int = Counter.get.now
    Log.log(s"=== $n lines").now


object LeChemin extends KyoApp:
  program.handle(
    ArrowEffect.handleLoop(Tag[Counter], 0, _)(
      [_] => (input, state, cont) =>
        input match
          case Counter.Op.Get =>
            Loop.continue(state, cont(state))
          case Counter.Op.Inc =>
            val inc = state + 1
            Log.log(s"▷ c:$inc") *> Loop.continue(inc, cont(inc))
    ),
    Log.run,
    run)
