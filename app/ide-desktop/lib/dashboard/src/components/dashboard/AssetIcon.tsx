/** @file Displays a non-interactable icon for an asset based on its type and name. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'
import ConnectorIcon from 'enso-assets/connector.svg'
import FolderIcon from 'enso-assets/folder.svg'
import NetworkIcon from 'enso-assets/network.svg'

import SvgMask from '#/components/SvgMask'

import * as backend from '#/services/Backend'

import * as fileIcon from '#/utilities/fileIcon'

/** Props for an {@link AssetIcon}. */
export interface AssetIconProps {
  asset: backend.AnyAsset
  className?: string
}

/** Displays a few details of an asset. */
export default function AssetIcon(props: AssetIconProps) {
  const { asset, className } = props
  switch (asset.type) {
    case backend.AssetType.directory: {
      return <SvgMask src={FolderIcon} className={className} />
    }
    case backend.AssetType.project: {
      return <SvgMask src={NetworkIcon} className={className} />
    }
    case backend.AssetType.file: {
      return <SvgMask src={fileIcon.fileIcon()} className={className} />
    }
    case backend.AssetType.secret: {
      return <SvgMask src={ConnectorIcon} className={className} />
    }
    case backend.AssetType.specialLoading:
    case backend.AssetType.specialEmpty: {
      // It should not be possible for these to be displayed, but return something anyway.
      return <SvgMask src={BlankIcon} className={className} />
    }
  }
}
