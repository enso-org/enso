/** @file Return the appropriate file icon given the file name. */
import type { SvgUseIcon } from '#/components/types'
import { basenameAndExtension } from 'enso-common/src/utilities/file'

/** Return the appropriate icon given the file name. */
export function fileIcon(fileName: string): SvgUseIcon {
  const { extension } = basenameAndExtension(fileName)
  switch (extension.toLowerCase()) {
    case 'png':
    case 'jpg':
    case 'jpeg':
    case 'tiff':
    case 'bmp':
    case 'webp':
    case 'gif': {
      return 'image'
    }
    case 'enso-project': {
      return 'graph_editor'
    }
    case 'datalink': {
      return 'connector'
    }
    case 'secret': {
      return 'key'
    }
    default: {
      if (fileName.endsWith('/')) {
        return 'folder'
      }
      return 'text'
    }
  }
}
