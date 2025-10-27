/** @file APIs for using `enso://` paths to refer to files accessible to a user. */
import type { UserFiles } from '@/components/widgets/FileBrowserWidget/userFiles'
import { findDifferenceIndex } from '@/util/data/array'
import type { ToValue } from '@/util/reactivity'
import type { DirectoryId } from 'enso-common/src/services/Backend'
import {
  andThen,
  Err,
  Ok,
  unwrapOrWithLog,
  type Result,
} from 'enso-common/src/utilities/data/result'
import { computed, toValue } from 'vue'

/** @internal */
export function pathToSegments(path: string) {
  const withProtocol = path.split('/')
  if (withProtocol[0] !== 'enso:') return Err(`"${path}" is not an enso path`)
  const endsWithSlash = withProtocol[withProtocol.length - 1] === ''
  const segments = withProtocol.slice(1).filter((segment) => segment)
  const nonEmpty = segments.length > 0
  const res = segments.concat(endsWithSlash && nonEmpty ? [''] : [])
  return Ok(res)
}

export interface EnsoPath {
  root: DirectoryId
  segments: string[]
}

/** @returns A new path produced by applying the given function to the segments of this path. */
export function mapPath(
  { root, segments }: EnsoPath,
  f: (segments: string[]) => string[],
): EnsoPath {
  return {
    root,
    segments: f(segments),
  }
}

/** @returns Whether two EnsoPath objects are equal. */
export function ensoPathEqual(a: EnsoPath | undefined, b: EnsoPath) {
  return (
    a != null &&
    a.root === b.root &&
    a.segments.length === b.segments.length &&
    a.segments.every((segment, i) => segment === b.segments[i])
  )
}

/**
 * @returns An API for interpreting and creating Enso Paths in the context of a given user's
 * available files.
 */
export function useEnsoPaths(
  userFiles: ToValue<Pick<UserFiles, 'rootDirectoryId' | 'rootPath'> | null>,
) {
  const rootSegments = computed(() =>
    pathToSegments(toValue(toValue(userFiles)?.rootPath) ?? 'enso://'),
  )

  function segmentsExcludingRoot(segments: string[]) {
    const rootSegs = unwrapOrWithLog(rootSegments.value ?? Err('cannot load root directory'), [])
    const afterRootIndex = findDifferenceIndex(segments, rootSegs)
    if (afterRootIndex < rootSegs.length) {
      return []
    } else {
      return segments.slice(afterRootIndex)
    }
  }

  function ensoPath(segments: string[]): Result<EnsoPath, string> {
    const files = toValue(userFiles)
    if (!files) return Err('User file information unavailable')
    return Ok({
      root: toValue(files.rootDirectoryId),
      segments: segmentsExcludingRoot(segments),
    })
  }

  function parseEnsoPath(path: string): Result<EnsoPath, string> {
    const rawSegments = pathToSegments(path)
    return andThen(rawSegments, (rawSegments) => ensoPath(rawSegments))
  }

  function printEnsoPath(path: EnsoPath) {
    const files = toValue(userFiles)
    let rootPath =
      (path.root === toValue(files?.rootDirectoryId) ? toValue(files?.rootPath) : undefined) ??
      'enso://'
    // Sometimes rootPath ends with /, and sometimes not.
    if (!rootPath.endsWith('/') && path.segments.length) rootPath = rootPath + '/'
    return `${rootPath}${path.segments.join('/')}`
  }

  return {
    parseEnsoPath,
    ensoPath,
    printEnsoPath,
  }
}
