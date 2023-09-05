// FIXME: change to use store
export const VISUALIZATION_TYPES = new Map<string, string>()

export function registerVisualization(label: string, inputType: string) {
  VISUALIZATION_TYPES.set(label, inputType)
}
