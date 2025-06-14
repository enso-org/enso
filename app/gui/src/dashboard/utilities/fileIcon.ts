/** @file Return the appropriate file icon given the file name. */
import { basenameAndExtension } from '#/utilities/fileInfo'
import type { SvgUseIcon } from '$/react-components/types'

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
    default: {
      return 'text'
    }
  }
}
