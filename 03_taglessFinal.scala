package taglessFinal

import kyo.*

trait Counter[-S]:
  def increment: Int < S

  def get: Int < S

  final def incrementDiscard: Unit < S = increment.unit
end Counter

trait Of[F[-_]]:
  inline def apply[S](using f: F[S]): F[S] = f

object Counter extends Of[Counter]:
  def increment[S: Counter]: Int < S = Counter[S].increment

  def incrementDiscard[S: Counter]: Unit < S = increment.unit

  def get[S: Counter]: Int < S = Counter[S].get
end Counter

trait Log[-S]:
  def log(str: String): Unit < S

object Log extends Of[Log]:
  def log[S: Log](str: String): Unit < S = Log[S].log(str)

val lyrics = Seq(
  "Regarde-toi, assise dans l'ombre",
  "À la lueur de nos mensonges",
  "Les mains glacées",
  "Jusqu'à l'ongle"
)

def program[S: {Counter, Log}]: Unit < (S & Sync) =
  direct:
    Console.printLine("=== Kyo - le chemin ===").now

    lyrics.foreach: line =>
      Counter.incrementDiscard.now
      Console.printLine(s"♪ $line").now

    val n: Int = Counter.get.now
    Log.log(s"=== $n lines").now

object LeChemin extends KyoApp:
  given Log[Sync] with
    def log(str: String): Unit < Sync = Console.printLine(str)

  given [S: Log]: Counter[Var[Int] & S] with
    def increment: Int < (Var[Int] & S) =
      Var.update[Int](_ + 1).tap(i => Log.log(s"▷ c:$i"))

    def get: Int < Var[Int] = Var.get[Int]
  end given

  program.handle(
    Var.run(0),
    run
  )
end LeChemin
