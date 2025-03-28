/** @file Functions for manipulating and querying paths. */
import * as detect from 'enso-common/src/detect'

import * as newtype from '#/utilities/newtype'

/** A filesystem path. */
export type Path = newtype.Newtype<string, 'Path'>
/** Create a {@link Path}. */
// eslint-disable-next-line @typescript-eslint/no-redeclare
export const Path = newtype.newtypeConstructor<Path>()

/** Construct a {@link Path} from an existing {@link Path} of the parent directory. */
export function joinPath(directoryPath: Path, fileName: string) {
  return Path(`${directoryPath}/${fileName}`)
}

/** Return the path, with backslashes (on Windows only) normalized to forward slashes. */
export function normalizeSlashes(path: string): Path {
  if (detect.isOnWindows()) {
    return Path(path.replace(/\\/g, '/'))
  } else {
    return Path(path)
  }
}

/** Split a {@link Path} inito the path of its parent directory, and its file name. */
export function getDirectoryAndName(path: Path) {
  const [, directoryPath = '', fileName = ''] = path.match(/^(.+)[/]([^/]+)$/) ?? []
  return { directoryPath: Path(directoryPath), fileName }
}
