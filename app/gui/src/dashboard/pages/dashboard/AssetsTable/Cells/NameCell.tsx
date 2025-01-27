/**
 * @file Name cell for any asset
 */
import { AssetType } from '#/services/Backend'
import { DatalinkNameCell } from './DatalinNameCell'
import { DirectoryNameCell } from './DirectoryNameCell'
import { FileCell } from './FileCell'
import { ProjectNameCell } from './ProjectNameCell'
import { SecretNameCell } from './SecretNameCell'
import type { AnyCellProps } from './types'

/**
 * Props for {@link NameCell}
 */
export interface NameCellProps extends AnyCellProps {}

/**
 * Generic name cell for assets
 */
export function NameCell(props: NameCellProps) {
  const { state } = props

  const itemType = state.row.original.item.type

  return (
    <>
      {itemType === AssetType.project && <ProjectNameCell state={state} />}
      {itemType === AssetType.directory && <DirectoryNameCell state={state} />}
      {itemType === AssetType.file && <FileCell state={state} />}
      {itemType === AssetType.secret && <SecretNameCell state={state} />}
      {itemType === AssetType.datalink && <DatalinkNameCell state={state} />}
    </>
  )
}
