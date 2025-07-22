import {
  type EditorState,
  type Extension,
  StateEffect,
  type StateEffectType,
  StateField,
  type Transaction,
} from '@codemirror/state'

export interface ValueExt<SetT, InitialT = SetT> {
  set: StateEffectType<SetT>
  get: StateField<InitialT>
  extension: Extension
  changed: (update: { state: EditorState; startState: EditorState }) => boolean
}

/**
 * Creates an extension for a state fields that holds a value, and related types for reading and
 * writing the value.
 */
export function valueExt<SetT extends InitialT, InitialT = SetT>(
  initial: InitialT,
): ValueExt<SetT, InitialT> {
  const set = StateEffect.define<SetT>()
  const get = StateField.define<InitialT>({
    create: () => initial,
    update: latestValue(set),
  })
  const extension: Extension = get
  const changed = (update: { state: EditorState; startState: EditorState }) =>
    update.state.field(get) !== update.startState.field(get)
  return { set, get, changed, extension }
}

function latestValue<EffectT extends InitialT, InitialT>(effectType: StateEffectType<EffectT>) {
  return (value: InitialT, transaction: Transaction) =>
    transaction.effects.reduce((a, effect) => (effect.is(effectType) ? effect.value : a), value)
}
