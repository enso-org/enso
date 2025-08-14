export interface PersistableStatePlugin<T> {
  captureState: () => T
  restoreState: (rawState: unknown) => void
}
