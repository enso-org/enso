export interface Visualizations {}

export type VisualizationType = keyof Visualizations

export const VISUALIZATION_TYPES = new Map<
  VisualizationType,
  Visualizations[keyof Visualizations]
>()

export function registerVizualization(
  label: keyof Visualizations,
  inputType: Visualizations[keyof Visualizations],
) {
  VISUALIZATION_TYPES.set(label, inputType)
}
