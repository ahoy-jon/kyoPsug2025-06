package rotationIsolation

import kyo.*
import kyo.kernel.ArrowEffect

enum Error derives CanEqual:
  case bIsNotAllowed
  case StateTooBig
  case Oups
end Error

type State = Seq[String]
val emptyState: State = Chunk.empty

def addToState(str: String): Unit < Var[State] = Var.updateDiscard[State](state => state :+ str)

val oups: Nothing < Abort[Error] = Abort.fail(Error.Oups)

val addAndOups: Unit < (Var[State] & Abort[Error]) =
  addToState("oups").andThen(oups).unit

@main def check(): Unit =
  val abortVar: Result[Error, (State, Unit)] =
    addAndOups.handle(
      Var.runTuple(emptyState),
      Abort.run,
      _.debugValue
    ).eval

  assert(
    abortVar == Result.Failure(Error.Oups)
  )

  val varAbort: (State, Result[Error, Unit]) =
    addAndOups.handle(
      Abort.run(_), // parfois, il faut aider le compilateur, dommage
      Var.runTuple(emptyState),
      _.debugValue
    ).eval

  assert(varAbort == (Seq("oups"), Result.Failure(Error.Oups)))
end check

val prgVarChoice: String < (Choice & Var[State]) =
  val abc: String < Choice = Choice.eval("a", "b", "c")

  def addToState(str: String): Unit < Var[State] =
    def add(state: State) = state :+ str

    Var.updateDiscard(add)
  end addToState

  def dropIfStateContainsB: Unit < (Choice & Var[State]) =
    direct:
      val state = Var.get[State].now
      Choice.dropIf(state.contains("b")).now

  direct:
    val someChoice: String = abc.now
    addToState(someChoice).now
    dropIfStateContainsB.now
    someChoice

end prgVarChoice

def run(v: Any < Any): Unit = v.eval match
  case _: Unit =>
  case x       => println(s"exit: $x")

@main def choice(): Unit =
  run:
    prgVarChoice.handle(
      _.debugValue,
      // Var.isolate.merge[State](_ ++? _).run,
      Choice.run,
      Var.runTuple(Chunk("0_o")),
      _.debugValue
    )

end choice

extension [A](seq: Seq[A])
  // la flemme de faire un CRDT
  infix def ++?[B >: A](seq2: Seq[B]): Seq[B] = (seq ++ seq2).distinct

  def distinctKeepLast: Seq[A] = seq.reverse.distinct.reverse
end extension

def doSomethingWith(input: String): String < (Var[State] & Abort[Error]) =
  val checkState: Unit < (Var[State] & Abort[Error]) = direct:
    val state = Var.get[State].now
    if state.size > 4 then
      Abort.fail(Error.StateTooBig).now

    if state.size == 4 then
      Var.setDiscard(state.drop(1)).now

  end checkState

  direct:
    addToState(input).now
    if input == "b" then
      Abort.fail(Error.bIsNotAllowed).now

    if input == "c" then
      Abort.fail(Error.Oups).now

    checkState.now

    s"good $input"

end doSomethingWith

type Context = Var[State] & Abort[Error]

def prg(isolate: Isolate[Context, Context, Context]): Unit < (Var[State]) =

  def proc(str: String): Unit < (Var[State]) =
    doSomethingWith(str).handle(
      isolate.run,
      Abort.run,
      _.debugValue,
      _.unit
    )

  direct:
    proc("a").now
    proc("b").now
    proc("b").now
    proc("c").now
    proc("b").now
    Var.get[State].debugValue.unit.now
    proc("a").now

end prg

def noIsolation[S0]: Isolate[S0, S0, S0] = new Isolate[S0, S0, S0]:
  type State        = Unit
  type Transform[A] = A

  override def capture[A, S](f: Unit => A < S)(using Frame): A < (S & S0) = f(())

  override def restore[A, S](v: A < S)(using Frame): A < (S & S0) = v

  override def isolate[A, S](state: Unit, v: A < (S & S0) )(using Frame) = v


def eval(v: Unit < (Var[State] & Abort[Nothing])): Unit =
  v.handle(
    Var.run(emptyState),
    Abort.runPartialOrThrow(_),
    _.unit.debugValue.eval
  )

@main def prgNoIsolation(): Unit =
  eval:
    prg(noIsolation)

@main def prgVarIsolation(): Unit =
  eval:
    prg(Var.isolate.update[State].andThen(noIsolation))

@main def prgCustomIsolation(): Unit =
  eval:
    prg(discardVarOnError:
      case Error.bIsNotAllowed => true
      case _                   => false)

@main def prgRewriteVarEffect(): Unit =
  eval:
    prg(noIsolation).handle(logCompact)

def logCompact[A, S](v: A < (S & Var[State])) =
  ArrowEffect.wireTap(Tag[Var[State]], v)(
    mapInput = [C] => input => input,
    mapOutput = [C] =>
      (input, output) =>
        // Abort.panic(new Exception("yolo")) *>
        output.debugValue.map(_.distinctKeepLast.debugValue)
  )

def discardVarOnError(discard: Error => Boolean): Isolate[Context, Any, Context] =
  new Isolate[Context, Any, Context]:
    type State        = rotationIsolation.State
    type Transform[A] = (State, Result[Error, A])

    override def capture[A, S](f: State => A < S)(using Frame): A < (S & Context) =
      Var.use[State](f)

    override def isolate[A, S](state: State, v: A < (S & Context))(using Frame): (State, Result[Error, A]) < S =
      v.handle(
        Abort.run(_),
        Var.runTuple(state)
      )

    override def restore[A, S](v: (State, Result[Error, A]) < S)(using Frame): A < (S & Context) =
      v.map: (state, result) =>
        val discardVar: Boolean = result.fold(_ => false, discard, _ => false)
        Var.set(state).when(!discardVar) *> Abort.get(result)
