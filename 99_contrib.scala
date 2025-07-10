package kyo

import kyo.kernel.ArrowEffect
import kyo.kernel.internal.Safepoint


//Merci Valentin ! https://github.com/vil1/pointless, cela donne envie de faire kyo-pointless
extension [A, S](v: A < S)
  infix def >>>[B, S2](f: A => B < S2)(using Frame): B < (S & S2) = v.map(f)


extension (arrowEffect: ArrowEffect.type)
  // --- --- ---
  inline def wireTap[I[_], O[_], E <: ArrowEffect[I, O], A, S, S2](
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
  end wireTap
end extension