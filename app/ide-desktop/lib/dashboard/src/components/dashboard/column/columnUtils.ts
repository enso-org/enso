/** @file Types and constants related to `Column`s. */

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import BlankIcon from 'enso-assets/blank.svg'
import DocsIcon from 'enso-assets/docs.svg'
import PeopleIcon from 'enso-assets/people.svg'
import TagIcon from 'enso-assets/tag.svg'
import TimeIcon from 'enso-assets/time.svg'

// =============
// === Types ===
// =============

/** Determines which columns are visible. */
export enum ColumnDisplayMode {
  /** Show only columns which are ready for release. */
  release = 'release',
  /** Show all columns. */
  all = 'all',
  /** Show only name and metadata. */
  compact = 'compact',
  /** Show only columns relevant to documentation editors. */
  docs = 'docs',
  /** Show only name, metadata, and configuration options. */
  settings = 'settings',
}

/** Column type. */
export enum Column {
  name = 'name',
  modified = 'modified',
  sharedWith = 'sharedWith',
  labels = 'labels',
  accessedByProjects = 'accessedByProjects',
  accessedData = 'accessedData',
  docs = 'docs',
}

/** Columns that can be used as a sort column. */
export type SortableColumn = Column.modified | Column.name

// =================
// === Constants ===
// =================

export const DEFAULT_ENABLED_COLUMNS: ReadonlySet<Column> = new Set([
  Column.name,
  Column.modified,
  Column.sharedWith,
  Column.labels,
])

/** The list of all possible columns for the local backend, in order. */
export const LOCAL_COLUMNS = Object.freeze([Column.name, Column.modified] as const)

/** The list of all possible columns for the cloud backend, in order. */
// This MUST be `as const`, to generate the `ExtraColumn` type above.
export const CLOUD_COLUMNS = Object.freeze([
  Column.name,
  Column.modified,
  Column.sharedWith,
  Column.labels,
  Column.accessedByProjects,
  Column.accessedData,
  Column.docs,
] as const)

export const COLUMN_ICONS: Readonly<Record<Column, string>> = {
  /* The file column does not have an icon, however this does not matter as it is not
   * collapsible. */
  [Column.name]: BlankIcon,
  [Column.modified]: TimeIcon,
  [Column.sharedWith]: PeopleIcon,
  [Column.labels]: TagIcon,
  [Column.accessedByProjects]: AccessedByProjectsIcon,
  [Column.accessedData]: AccessedDataIcon,
  [Column.docs]: DocsIcon,
}

/** English names for every column except for the name column. */
export const COLUMN_NAME: Readonly<Record<Column, string>> = {
  [Column.name]: 'Name',
  [Column.modified]: 'Modified',
  [Column.sharedWith]: 'Shared with',
  [Column.labels]: 'Labels',
  [Column.accessedByProjects]: 'Accessed by projects',
  [Column.accessedData]: 'Accessed data',
  [Column.docs]: 'Docs',
}

const COLUMN_CSS_CLASSES =
  'text-left bg-clip-padding border-transparent border-y border-2 last:border-r-0 last:rounded-r-full last:w-full'
const NORMAL_COLUMN_CSS_CLASSES = `px-cell-x py ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. */
export const COLUMN_CSS_CLASS: Readonly<Record<Column, string>> = {
  [Column.name]: `rounded-rows-skip-level min-w-drive-name-column h-full p border-l-0 ${COLUMN_CSS_CLASSES}`,
  [Column.modified]: `min-w-drive-modified-column ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.sharedWith]: `min-w-drive-shared-with-column ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.labels]: `min-w-drive-labels-column ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedByProjects]: `min-w-drive-accessed-by-projects-column ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedData]: `min-w-drive-accessed-data-column ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.docs]: `min-w-drive-docs-column ${NORMAL_COLUMN_CSS_CLASSES}`,
}

// =====================
// === getColumnList ===
// =====================

/** Return the full list of columns given the relevant current state. */
export function getColumnList(isCloud: boolean, enabledColumns: ReadonlySet<Column>) {
  const columns = isCloud ? CLOUD_COLUMNS : LOCAL_COLUMNS
  return columns.filter(column => enabledColumns.has(column))
}
