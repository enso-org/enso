import {
  Compartment,
  type Extension,
  type StateEffectType,
  type TransactionSpec,
} from '@codemirror/state'
import { toValue, watch, type WatchSource } from 'vue'

interface HasDispatch {
  dispatch: (...specs: TransactionSpec[]) => void
}

/** Dispatch a state effect to the editor whenever the given watch source changes. */
export function useStateEffect<T>(
  { dispatch }: HasDispatch,
  effectType: StateEffectType<T>,
  value: WatchSource<T>,
) {
  watch(
    value,
    (value) =>
      dispatch({
        effects: effectType.of(value),
      }),
    { immediate: true },
  )
}

/** Returns a reactive {@link Compartment} that includes any extensions provided by the watch source. */
export function useCompartment({ dispatch }: HasDispatch, extension: WatchSource<Extension>) {
  const compartment = new Compartment()
  const compartmentInstance = compartment.of(toValue(extension))
  watch(extension, (extension) =>
    dispatch({
      effects: compartment.reconfigure(extension),
    }),
  )
  return compartmentInstance
}

/** Dispatch a transaction when the provided watch source changes. */
export function useDispatch(
  { dispatch }: HasDispatch,
  transaction: WatchSource<TransactionSpec>,
  onCleanup?: () => void,
) {
  watch(
    transaction,
    (transaction, _old, setCleanupHook) => {
      dispatch(transaction)
      if (onCleanup) setCleanupHook(onCleanup)
    },
    { immediate: true },
  )
}
