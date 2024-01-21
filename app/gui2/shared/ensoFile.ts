const META_TAG = '\n\n\n#### METADATA ####'

export interface EnsoFileParts {
  code: string
  idMapJson: string | null
  metadataJson: string | null
}

export function splitFileContents(content: string): EnsoFileParts {
  const splitPoint = content.lastIndexOf(META_TAG)
  if (splitPoint < 0) {
    return {
      code: content,
      idMapJson: null,
      metadataJson: null,
    }
  }
  const code = content.slice(0, splitPoint)
  const metadataString = content.slice(splitPoint + META_TAG.length)
  const metaLines = metadataString.trim().split('\n')
  const idMapJson = metaLines[0] ?? null
  const metadataJson = metaLines[1] ?? null
  return { code, idMapJson, metadataJson }
}

export function combineFileParts(parts: EnsoFileParts): string {
  const hasMeta = parts.idMapJson != null || parts.metadataJson != null
  if (hasMeta) {
    return `${parts.code}${META_TAG}\n${parts.idMapJson ?? ''}\n${parts.metadataJson ?? ''}`
  } else {
    // If code segment contains meta tag, add another one to make sure it is not misinterpreted.
    if (parts.code.includes(META_TAG)) {
      return `${parts.code}${META_TAG}`
    } else {
      return parts.code
    }
  }
}
