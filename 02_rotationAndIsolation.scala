package rotationAndIsolation

import kyo.*

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

@main def check =
  val abortVar: Result[Error, (State, Unit)] =
    addAndOups.handle(
      Var.runTuple(emptyState),
      Abort.run
    ).eval

  assert(
    abortVar == Result.Failure(Error.Oups)
  )

  val varAbort: (State, Result[Error, Unit]) =
    addAndOups.handle(
      Abort.run(_), // parfois, il faut aider le compilateur, dommage
      Var.runTuple(emptyState)
    ).eval

  assert(varAbort == (Seq("oups"), Result.Failure(Error.Oups)))
end check

def choice() = {}
