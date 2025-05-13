/**
 * @file Shared types for the asset panel.
 */
import type { Category } from '#/layouts/Drive/CategorySwitcher'
import type Backend from '#/services/Backend'

/**
 * Props for the asset panel.
 */
export interface AssetPanelProps {
  readonly backend: Backend
  readonly category: Category
}
