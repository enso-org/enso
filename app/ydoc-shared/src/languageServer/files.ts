import type { LanguageServer } from '../languageServer'
import type { FileSystemObject, Path } from '../languageServerTypes'
import { Err, Ok, type Result } from '../util/data/result'

/**
 * Recursively walk through the file system using the Language Server,
 * calling the given callback with each file system entry found
 * (both files and folders).
 */
export async function walkFs(
  ls: LanguageServer,
  path: Path,
  cb: (type: FileSystemObject['type'], path: Path) => void,
): Promise<Result<void>> {
  const files = await ls.listFiles(path)
  if (!files.ok) return files
  for (const file of files.value.paths) {
    const filePath: Path = {
      rootId: file.path.rootId,
      segments: [...file.path.segments, file.name],
    }
    cb(file.type, filePath)
    switch (file.type) {
      case 'Directory':
      case 'DirectoryTruncated': {
        walkFs(ls, filePath, cb)
        break
      }
      case 'File':
      case 'Other':
      case 'SymlinkLoop': {
        // Ignored.
        break
      }
      default: {
        const unexpected: never = file
        return Err('Unexpected object: ' + JSON.stringify(unexpected))
      }
    }
  }
  return Ok()
}
