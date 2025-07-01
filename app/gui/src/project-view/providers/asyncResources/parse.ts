import { EnsoPath } from '#/services/Backend'
import { Err, Ok, Result } from '@/util/data/result'
import { ToValue } from '@/util/reactivity'
import { toValue } from 'vue'

export type ParsedAssetUrl =
  | { kind: 'projectRelative'; relativePath: string }
  | { kind: 'ensoPath'; ensoPath: EnsoPath }
  | { kind: 'webUrl'; url: URL } // only allowed web protocols

/**
 * Transform given raw URL (e.g. from a markdown image tags) into a validated representation
 * that explciitly represents all variations of different paths we want to support.
 */
export function parseResourceUrl(
  unparsedAssetUrl: string,
  basePathSegments: ToValue<string[] | undefined>,
): Result<ParsedAssetUrl> {
  const asUrl = URL.parse(unparsedAssetUrl)
  if (asUrl != null) {
    switch (asUrl.protocol) {
      case 'http:':
      case 'https:':
        return Ok({ kind: 'webUrl', url: asUrl })
      case 'enso:':
        return Ok({ kind: 'ensoPath', ensoPath: EnsoPath(decodeURI(asUrl.href)) })
    }
    return Err('Unsupported URL protocol: ' + asUrl.protocol)
  }
  // We already know that `unparsedAssetUrl` is not a valid URL by itself.
  // Attempt interpreting it as a relative path with a project base.

  // relative URLs starting with '/' are always treated as project-relative.
  // Avoid creating a dependency on `basePathSegments`.
  const segments = unparsedAssetUrl.startsWith('/') ? [] : toValue(basePathSegments)
  if (segments) {
    const asProjectUrl = URL.parse(unparsedAssetUrl, 'project:///' + segments.join('/'))
    if (asProjectUrl?.protocol === 'project:') {
      const relativePath = decodeURI(asProjectUrl.pathname).substring(1) // drop leading '/'
      return Ok({ kind: 'projectRelative', relativePath })
    }
  }

  return Err('Unsupported resource URL: ' + unparsedAssetUrl)
}
