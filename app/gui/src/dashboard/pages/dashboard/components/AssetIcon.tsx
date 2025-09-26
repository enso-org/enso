/** @file Displays a non-interactable icon for an asset based on its type and name. */
import { Icon } from '#/components/Icon'
import type { AnyAsset } from '#/services/Backend'
import { fileIcon } from '#/utilities/fileIcon'

/** Props for an {@link AssetIcon}. */
export interface AssetIconProps {
  readonly asset: Pick<AnyAsset, 'title' | 'type'>
  readonly className?: string
}

/** Displays a non-interactable icon for an asset based on its type and name. */
export default function AssetIcon(props: AssetIconProps) {
  const { asset, className } = props
  switch (asset.type) {
    case 'directory': {
      return <Icon icon="folder" className={className} />
    }
    case 'project': {
      return <Icon icon="graph_editor" className={className} />
    }
    case 'file': {
      return <Icon icon={fileIcon(asset.title)} className={className} />
    }
    case 'datalink': {
      return <Icon icon="connector" className={className} />
    }
    case 'secret': {
      return <Icon icon="key" className={className} />
    }
    case 'specialUp': {
      // It should not be possible for these to be displayed, but return something anyway.
      return <Icon icon="ghost" className={className} />
    }
  }
}
