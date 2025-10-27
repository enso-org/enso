import type { ToValue } from '@/util/reactivity'
import { urlParse } from '@/util/url'
import { EnsoPath } from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { toValue } from 'vue'

export type ParsedAssetUrl =
  | { kind: 'projectRelative'; relativePath: string; uploading: boolean }
  | { kind: 'ensoPath'; ensoPath: EnsoPath }
  | { kind: 'webUrl'; url: URL } // only allowed web protocols

// A canary "root directory" name, used during parsing to detect if the path escaped the project.
const CANARY_ROOT = `__CANARY__`

/**
 * Transform given raw URL (e.g. from a markdown image tags) into a validated representation
 * that explciitly represents all variations of different paths we want to support.
 */
export function parseResourceUrl(
  urlString: string,
  basePathSegments?: ToValue<string[] | undefined>,
): Result<ParsedAssetUrl> {
  if (!urlString) return Err('Expected non-empty resource URL')
  const asUrl = urlParse(urlString)

  if (asUrl != null) {
    switch (asUrl.protocol) {
      case 'data:':
      case 'http:':
      case 'https:':
        return Ok({ kind: 'webUrl', url: asUrl })
      case 'enso:':
        return Ok({ kind: 'ensoPath', ensoPath: EnsoPath(decodeURI(asUrl.href)) })
    }
    return Err('Unsupported URL protocol: ' + asUrl.protocol)
  }
  // We already know that `urlString` is not a valid URL by itself.
  // Attempt interpreting it as a relative path with a project base.
  if (!/[:\\<>|]/.test(urlString)) {
    // relative URLs starting with '/' are always treated as project-relative.
    // Avoid creating a dependency on `basePathSegments`.
    urlString = urlString.replace(/\/\/+/, '/')
    const isAbsolute = urlString.startsWith('/')
    const segments = isAbsolute ? [] : toValue(basePathSegments)
    if (segments) {
      const rootRelativePath =
        isAbsolute ?
          CANARY_ROOT + urlString
        : [CANARY_ROOT, ...segments.slice(0, -1), urlString].join('/')
      const asProjectUrl = urlParse(rootRelativePath, 'project:///')
      if (asProjectUrl?.protocol === 'project:') {
        const relativePath = decodeURI(asProjectUrl.pathname)
        if (!relativePath.startsWith('/' + CANARY_ROOT + '/'))
          return Err('Resource path outside of project directory')

        return Ok({
          kind: 'projectRelative',
          relativePath: relativePath.substring(CANARY_ROOT.length + 2), // drop canary root and slashes
          uploading: asProjectUrl.searchParams.get('uploading') != null,
        })
      }
    }
  }

  return Err('Unsupported resource URL: ' + urlString)
}
