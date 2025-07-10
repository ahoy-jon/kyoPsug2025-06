package directDirect

import kyo.*

def f(i: Int): Int < Any = i * 2

def g(s: String): Maybe[Int] =
  Result(s.toInt).toMaybe

val prg: Unit < Sync = direct:
  val chunk: Chunk[Int] = Stream.init(Seq(1, 2, 3)).map(f).run.now

  chunk.foreach: x =>
    if x > 4 then
      Console.printLine(s"value: $x").now

  g("42").foreach: n =>
    Console.printLine(s"parsed: $n").now

@main def main() = KyoApp(prg).main(Array.empty)