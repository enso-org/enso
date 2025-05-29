/** @file Return the appropriate file icon given the file name. */
import type { SvgUseIcon } from '#/components/types'
import { basenameAndExtension } from '#/utilities/fileInfo'

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
