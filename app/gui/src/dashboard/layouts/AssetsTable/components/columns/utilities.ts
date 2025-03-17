/** @file Utility functions for columns. */
import type { Category } from '#/layouts/Drive/CategorySwitcher'
import * as backend from '#/services/Backend'
import { Column } from './types'

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
    isCloud && Column.docs,
  ] as const

  return columns.flatMap((column) => (column !== false ? [column] : []))
}
