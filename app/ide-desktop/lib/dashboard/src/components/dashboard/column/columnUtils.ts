/** @file Types and constants related to `Column`s. */
import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import DocsIcon from 'enso-assets/docs.svg'
import TagIcon from 'enso-assets/tag.svg'

import * as backend from '#/services/Backend'

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
export const EXTRA_COLUMNS = [
  Column.labels,
  Column.accessedByProjects,
  Column.accessedData,
  Column.docs,
] as const

export const EXTRA_COLUMN_IMAGES: Readonly<Record<ExtraColumn, string>> = {
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
