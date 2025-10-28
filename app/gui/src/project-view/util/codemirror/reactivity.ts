import {
  Compartment,
  type Extension,
  type StateEffectType,
  type TransactionSpec,
} from '@codemirror/state'
import { markRaw, toRaw, toRef, watch, type WatchSource } from 'vue'

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
export function useCompartment(view: HasDispatch, extension: WatchSource<Extension>) {
  const compartment = markRaw(new Compartment())
  // The `watch` cannot be run immediately because the compartment is not installed yet. Convert
  // the extension to a ref so that it can be watched without an initial execution; evaluate the
  // initial value explicitly, and pass it to the compartment initializer.
  const extensionRef = toRef(extension)
  const compartmentInstance = markRaw(compartment.of(toRaw(extensionRef.value)))
  watch(extensionRef, (extension) =>
    view.dispatch({ effects: compartment.reconfigure(toRaw(extension)) }),
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
