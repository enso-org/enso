/** @file Types and constants related to `Column`s. */
import type * as text from 'enso-common/src/text'

import AccessedByProjectsIcon from '#/assets/accessed_by_projects.svg'
import AccessedDataIcon from '#/assets/accessed_data.svg'
import BlankIcon from '#/assets/blank.svg'
import DocsIcon from '#/assets/docs.svg'
import PeopleIcon from '#/assets/people.svg'
import TagIcon from '#/assets/tag.svg'
import TimeIcon from '#/assets/time.svg'

import type { Category } from '#/layouts/CategorySwitcher/Category'
import * as backend from '#/services/Backend'

// =============
// === Types ===
// =============

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

export const COLUMN_SHOW_TEXT_ID: Readonly<Record<Column, text.TextId>> = {
  [Column.name]: 'nameColumnShow',
  [Column.modified]: 'modifiedColumnShow',
  [Column.sharedWith]: 'sharedWithColumnShow',
  [Column.labels]: 'labelsColumnShow',
  [Column.accessedByProjects]: 'accessedByProjectsColumnShow',
  [Column.accessedData]: 'accessedDataColumnShow',
  [Column.docs]: 'docsColumnShow',
} satisfies { [C in Column]: `${C}ColumnShow` }

const COLUMN_CSS_CLASSES =
  'text-left bg-clip-padding border-transparent border-y border-2 last:border-r-0 last:rounded-r-full last:w-full'
const NORMAL_COLUMN_CSS_CLASSES = `px-cell-x py ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. */
export const COLUMN_CSS_CLASS: Readonly<Record<Column, string>> = {
  [Column.name]: `rounded-rows-skip-level min-w-drive-name-column h-full p-0 border-l-0 ${COLUMN_CSS_CLASSES}`,
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
export function getColumnList(
  user: backend.User,
  backendType: backend.BackendType,
  category: Category,
) {
  const isCloud = backendType === backend.BackendType.remote
  const isEnterprise = user.plan === backend.Plan.enterprise
  const isTrash = category.type === 'trash'
  const columns = [
    Column.name,
    Column.modified,
    isCloud && (isEnterprise || isTrash) && Column.sharedWith,
    isCloud && Column.labels,
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1525
    // Bring back these columns when they are ready for use again.
    // isCloud && Column.accessedByProjects,
    // isCloud && Column.accessedData,
    isCloud && Column.docs,
  ]
  return columns.flatMap((column) => (column !== false ? [column] : []))
}
