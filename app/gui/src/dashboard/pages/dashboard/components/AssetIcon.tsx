/** @file Displays a non-interactable icon for an asset based on its type and name. */
import { Icon } from '#/components/Icon'
import * as fileIcon from '#/utilities/fileIcon'
import * as backend from 'enso-common/src/services/Backend'

/** Props for an {@link AssetIcon}. */
export interface AssetIconProps {
  readonly asset: Pick<backend.AnyAsset, 'title' | 'type'>
  readonly className?: string
}

/** Displays a non-interactable icon for an asset based on its type and name. */
export default function AssetIcon(props: AssetIconProps) {
  const { asset, className } = props
  switch (asset.type) {
    case backend.AssetType.directory: {
      return <Icon icon="folder" className={className} />
    }
    case backend.AssetType.project: {
      return <Icon icon="graph_editor" className={className} />
    }
    case backend.AssetType.file: {
      return <Icon icon={fileIcon.fileIcon(asset.title)} className={className} />
    }
    case backend.AssetType.datalink: {
      return <Icon icon="connector" className={className} />
    }
    case backend.AssetType.secret: {
      return <Icon icon="key" className={className} />
    }
  }
}
