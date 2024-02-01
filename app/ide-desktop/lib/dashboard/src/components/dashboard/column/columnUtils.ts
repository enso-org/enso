/** @file Types and constants related to `Column`s. */
import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import DocsIcon from 'enso-assets/docs.svg'
import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import SortDescendingIcon from 'enso-assets/sort_descending.svg'
import TagIcon from 'enso-assets/tag.svg'

import * as backend from '#/services/Backend'

import SortDirection from '#/utilities/SortDirection'

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

/** Columns that can be toggled between visible and hidden. */
export type ExtraColumn = (typeof EXTRA_COLUMNS)[number]

/** Columns that can be used as a sort column. */
export type SortableColumn = Column.modified | Column.name

// =================
// === Constants ===
// =================

/** The list of extra columns, in order. */
// This MUST be `as const`, to generate the `ExtraColumn` type above.
// eslint-disable-next-line no-restricted-syntax
export const EXTRA_COLUMNS = [
  Column.labels,
  Column.accessedByProjects,
  Column.accessedData,
  Column.docs,
] as const

export const EXTRA_COLUMN_IMAGES: Record<ExtraColumn, string> = {
  [Column.labels]: TagIcon,
  [Column.accessedByProjects]: AccessedByProjectsIcon,
  [Column.accessedData]: AccessedDataIcon,
  [Column.docs]: DocsIcon,
}

/** English names for every column except for the name column. */
export const COLUMN_NAME: Record<Column, string> = {
  [Column.name]: 'Name',
  [Column.modified]: 'Modified',
  [Column.sharedWith]: 'Shared with',
  [Column.labels]: 'Labels',
  [Column.accessedByProjects]: 'Accessed by projects',
  [Column.accessedData]: 'Accessed data',
  [Column.docs]: 'Docs',
} as const

const COLUMN_CSS_CLASSES =
  'text-left bg-clip-padding border-transparent border-l-2 border-r-2 last:border-r-0'
const NORMAL_COLUMN_CSS_CLASSES = `px-2 last:rounded-r-full last:w-full ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. */
export const COLUMN_CSS_CLASS: Record<Column, string> = {
  [Column.name]: `rounded-rows-skip-level min-w-61.25 p-0 border-l-0 ${COLUMN_CSS_CLASSES}`,
  [Column.modified]: `min-w-33.25 ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.sharedWith]: `min-w-40 ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.labels]: `min-w-80 ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedByProjects]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedData]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.docs]: `min-w-96 ${NORMAL_COLUMN_CSS_CLASSES}`,
} as const

// =====================
// === getColumnList ===
// =====================

/** Return the full list of columns given the relevant current state. */
export function getColumnList(backendType: backend.BackendType, extraColumns: Set<ExtraColumn>) {
  switch (backendType) {
    case backend.BackendType.local: {
      return [Column.name, Column.modified]
    }
    case backend.BackendType.remote: {
      return [
        Column.name,
        Column.modified,
        Column.sharedWith,
        ...EXTRA_COLUMNS.filter(column => extraColumns.has(column)),
      ]
    }
  }
}

// =================
// === Constants ===
// =================

/** The corresponding icon URL for each {@link SortDirection}. */
export const SORT_ICON: Record<SortDirection, string> = {
  [SortDirection.ascending]: SortAscendingIcon,
  [SortDirection.descending]: SortDescendingIcon,
}
