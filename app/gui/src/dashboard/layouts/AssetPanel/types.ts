/**
 * @file
 * Types for the {@link import('./AssetPanel').AssetPanel} component.
 */

/** Determines the content of the {@link import('./AssetPanel').AssetPanel}. */
export const ASSET_PANEL_TABS = [
  'settings',
  'versions',
  'sessions',
  'executionsCalendar',
  'docs',
] as const

/** Determines the content of the {@link import('./AssetPanel').AssetPanel}. */
export type AssetPanelTab = (typeof ASSET_PANEL_TABS)[number]
