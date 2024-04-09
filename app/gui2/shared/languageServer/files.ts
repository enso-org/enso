import { type LanguageServer } from 'shared/languageServer'
import type { FileSystemObject, Path } from 'shared/languageServerTypes'

export async function walkFs(
  ls: LanguageServer,
  path: Path,
  cb: (type: FileSystemObject['type'], path: Path) => void,
) {
  for (const file of (await ls.listFiles(path)).paths) {
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
        throw new Error('Unexpected object: ' + JSON.stringify(unexpected))
      }
    }
  }
}
