/** @file Types and constants related to `Column`s. */
import type * as text from 'enso-common/src/text'

import type { SvgUseIcon } from '#/components/AriaComponents'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import * as backend from '#/services/Backend'

/** Column type. */
export enum Column {
  name = 'name',
  modified = 'modified',
  sharedWith = 'sharedWith',
  labels = 'labels',
  path = 'path',
  accessedByProjects = 'accessedByProjects',
  accessedData = 'accessedData',
}

/** Columns that can be used as a sort column. */
export type SortableColumn = Column.modified | Column.name

export const DEFAULT_ENABLED_COLUMNS: ReadonlySet<Column> = new Set([
  Column.name,
  Column.modified,
  Column.sharedWith,
  Column.labels,
  Column.path,
])

export const COLUMN_ICONS: Readonly<Record<Column, SvgUseIcon | (string & {})>> = {
  /* The file column does not have an icon, however this does not matter as it is not
   * collapsible. */
  [Column.name]: 'ghost',
  [Column.modified]: 'time',
  [Column.sharedWith]: 'people',
  [Column.labels]: 'tag',
  [Column.accessedByProjects]: 'accessed_by_projects',
  [Column.accessedData]: 'accessed_data',
  [Column.path]: 'folder',
}

export const COLUMN_SHOW_TEXT_ID: Readonly<Record<Column, text.TextId>> = {
  [Column.name]: 'nameColumnShow',
  [Column.modified]: 'modifiedColumnShow',
  [Column.sharedWith]: 'sharedWithColumnShow',
  [Column.labels]: 'labelsColumnShow',
  [Column.accessedByProjects]: 'accessedByProjectsColumnShow',
  [Column.accessedData]: 'accessedDataColumnShow',
  [Column.path]: 'pathColumnShow',
} satisfies { [C in Column]: `${C}ColumnShow` }

const COLUMN_CSS_CLASSES =
  'max-w-96 text-left bg-clip-padding last:border-r-0 last:rounded-r-full last:w-full last:min-w-0 last:max-w-0'
const NORMAL_COLUMN_CSS_CLASSES = `px-cell-x py ${COLUMN_CSS_CLASSES}`

/** CSS classes for every column. */
export const COLUMN_CSS_CLASS: Readonly<Record<Column, string>> = {
  [Column.name]: `z-10 sticky left-1 bg-dashboard rounded-rows-skip-level w-80 max-w-80 h-full p-0 border-l-0 after:absolute after:right-0 after:top-0 after:bottom-0 after:border-r-[1.5px] after:border-primary/5 ${COLUMN_CSS_CLASSES}`,
  [Column.modified]: `min-w-drive-modified-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.sharedWith]: `min-w-drive-shared-with-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.labels]: `min-w-drive-labels-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedByProjects]: `min-w-drive-accessed-by-projects-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.accessedData]: `min-w-drive-accessed-data-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
  [Column.path]: `min-w-drive-path-column rounded-rows-have-level ${NORMAL_COLUMN_CSS_CLASSES}`,
}

/** Return the full list of columns given the relevant current state. */
export function getColumnList(
  user: backend.User,
  backendType: backend.BackendType,
  category: Category,
): readonly Column[] {
  const isCloud = backendType === backend.BackendType.remote
  const isEnterprise = user.plan === backend.Plan.enterprise

  const isTrash = category.type === 'trash'
  const isRecent = category.type === 'recent'
  const isRoot = category.type === 'cloud'

  const sharedWithColumn = () => {
    if (isTrash) return false
    if (isRecent) return false
    if (isRoot) return false
    return isCloud && isEnterprise && Column.sharedWith
  }

  const pathColumn = () => {
    if (isTrash) return Column.path
    if (isRecent) return Column.path

    return false
  }

  const columns = [
    Column.name,
    Column.modified,
    sharedWithColumn(),
    pathColumn(),
    isCloud && Column.labels,
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1525
    // Bring back these columns when they are ready for use again.
    // isCloud && Column.accessedByProjects,
    // isCloud && Column.accessedData,
  ] as const

  return columns.flatMap((column) => (column !== false ? [column] : []))
}
