/**
 * @file shared types for the asset panel
 */
import type Backend from '#/services/Backend'
import type { Category } from '../../Drive/Categories'

/**
 * Props for the asset panel.
 */
export interface AssetPanelProps {
  readonly backend: Backend
  readonly category: Category
}
