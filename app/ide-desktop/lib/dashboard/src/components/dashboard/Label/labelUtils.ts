/** @file Constants related to labels. */
import type * as backend from '#/services/backend'

// =================
// === Constants ===
// =================

// The default color for labels (Light blue).
export const DEFAULT_LABEL_COLOR: backend.LChColor = {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  lightness: 100,
  chroma: 0,
  hue: 0,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  alpha: 70,
}
