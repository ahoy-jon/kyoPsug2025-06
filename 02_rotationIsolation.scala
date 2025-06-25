package rotationIsolation

import kyo.*
import kyo.kernel.ArrowEffect
import kyo.kernel.internal.Safepoint

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

@main def choice(): Unit = {
  val res =
    prgVarChoice.handle(
      _.debugValue,
      //Var.isolate.merge[State](_ ++? _).run,
      Choice.run,
      Var.runTuple(Chunk("0_o")),
      _.debugValue
    ).eval
}

extension [A](seq: Seq[A])
  //la flemme de faire un CRDT
  infix def ++?[B >: A](seq2: Seq[B]): Seq[B] = (seq ++ seq2).distinct

  def distinctKeepLast: Seq[A] = seq.reverse.distinct.reverse


def doSomethingWith(input: String): String < (Var[State] & Abort[Error]) =
  val checkState: Unit < (Var[State] & Abort[Error]) = direct:
    val state = Var.get[State].now
    if (state.size > 4)
      Abort.fail(Error.StateTooBig).now

    if (state.size == 4)
      Var.setDiscard(state.drop(1)).now

  end checkState

  direct:
    addToState(input).now
    if (input == "b")
      Abort.fail(Error.bIsNotAllowed).now

    if (input == "c")
      Abort.fail(Error.Oups).now

    checkState.now

    s"good $input"

end doSomethingWith

type Context = Var[State] & Abort[Error]

def prg(isolate: Isolate.Stateful[Context, Context]): Unit < (Var[State]) =

  def proc(str: String): Unit < (Var[State]) = {
    doSomethingWith(str).handle(
      isolate.run,
      Abort.run,
      _.debugValue,
      _.unit
    )
  }


  direct:
    proc("a").now
    proc("b").now
    proc("b").now
    proc("c").now
    proc("b").now
    Var.get[State].debugValue.unit.now
    proc("a").now

end prg


def noIsolation[S]: Isolate.Stateful[S, S] = new Isolate.Stateful[S, S]:
  type State = Unit
  type Transform[A] = A

  override def capture[A, S1](f: Unit => A < S1)(using Frame): A < (S1 & S) = f(())

  override def restore[A, S1](v: A < S1)(using Frame): A < (S1 & S) = v

  override def isolate[A, S1](state: Unit, v: A < (S1 & S))(using Frame): A < (S1 & S) = v


def eval(v: Unit < Var[State]):Unit = Var.run(emptyState)(v).eval

@main def prgNoIsolation(): Unit = {
  eval:
    prg(noIsolation)
}

@main def prgVarIsolation(): Unit = {
  eval:
    prg(Var.isolate.update[State].andThen(noIsolation))
}

@main def prgCustomIsolation(): Unit = {
  eval:
    prg(discardVarOnError:
      case Error.bIsNotAllowed => true
      case _ => false
    )
}

@main def prgRewriteVarEffect(): Unit = {
  eval:
    prg(noIsolation).handle(logCompact)
}

def logCompact[A, S](v: A < (S & Var[State])): (A < (S & Var[State])) =
  ArrowEffect.tap(Tag[Var[State]], v)(
    mapInput = [C] => input => input,
    mapOutput = [C] => (input, output) => output.debugValue.map(_.distinctKeepLast)
  )



def discardVarOnError(discard: Error => Boolean): Isolate.Stateful[Context, Any] =
  new Isolate.Stateful[Context, Any]:
    type State = rotationIsolation.State
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


extension (arrowEffect: ArrowEffect.type) {
  // --- --- ---
  inline def tap[I[_], O[_], E <: ArrowEffect[I, O], A, S, S2](
                                                                inline effectTag: Tag[E],
                                                                v: A < (E & S)
                                                              )(
                                                                inline mapInput: [C] => I[C] => I[C] < S2,
                                                                inline mapOutput: [C] => (I[C], O[C]) => O[C] < S2
                                                              )(using
                                                                inline _frame: Frame,
                                                                safepoint: Safepoint
                                                              ): A < (S & E & S2) =
    ArrowEffect.handleLoop(effectTag, v):
      [C] =>
        (originalInput, cont) =>
          mapInput[C](originalInput).map: input =>
            ArrowEffect.suspendWith[C](effectTag, input): o =>
              mapOutput[C](originalInput, o).map: o =>
                Loop.continue:
                  cont(o)
  end tap
}