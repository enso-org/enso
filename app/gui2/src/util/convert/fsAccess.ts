/// <reference types="wicg-file-system-access" />
export interface FileTree {
  [name: string]: FileTree | string | ArrayBuffer
}

function arrayIsSame(a: unknown[], b: unknown) {
  return Array.isArray(b) && a.length === b.length && a.every((item, i) => b[i] === item)
}

export function mockFsFileHandle(
  contents: string | ArrayBuffer,
  name: string,
  path: string[] = [],
): FileSystemFileHandle {
  return {
    kind: 'file',
    isFile: true,
    isDirectory: false,
    // Spreaded to avoid excess property error.
    ...{ _path: path },
    name,
    queryPermission() {
      // Unimplemented.
      throw new Error('Cannot query permission in a read-only mock.')
    },
    requestPermission() {
      // Unimplemented.
      throw new Error('Cannot request permission in a read-only mock.')
    },
    async isSameEntry(other) {
      return this.kind === other.kind && '_path' in other && arrayIsSame(path, other._path)
    },
    async getFile() {
      return new File([contents], name)
    },
    createWritable() {
      throw new Error('Cannot create a writable strean from a read-only mock.')
    },
  }
}

export function mockFsDirectoryHandle(
  tree: FileTree,
  name: string,
  path: string[] = [],
): FileSystemDirectoryHandle {
  return {
    kind: 'directory',
    isFile: false,
    isDirectory: true,
    name,
    // Spreaded to avoid excess property error.
    ...{ _path: path },
    async isSameEntry(other) {
      return this.kind === other.kind && '_path' in other && arrayIsSame(path, other._path)
    },
    async resolve(possibleDescendant) {
      if (!('_path' in possibleDescendant)) return null
      if (!Array.isArray(possibleDescendant._path)) return null
      if (possibleDescendant._path.length < path.length) return null
      if (possibleDescendant._path.slice(0, path.length).some((segment, i) => segment !== path[i]))
        return null
      const descendantPath: string[] = possibleDescendant._path
      return descendantPath.slice(path.length)
    },
    queryPermission() {
      // Unimplemented.
      throw new Error('Cannot query permission in a read-only mock.')
    },
    requestPermission() {
      // Unimplemented.
      throw new Error('Cannot request permission in a read-only mock.')
    },
    async getDirectoryHandle(name) {
      const entry = tree[name]
      if (!entry || typeof entry === 'string' || entry instanceof ArrayBuffer) {
        const error = new DOMException(
          `The directory '${[...path, name].join('/')}' was not found.`,
          'NotFoundError',
        )
        throw error
      }
      return mockFsDirectoryHandle(entry, name, [...path, name])
    },
    async getFileHandle(name) {
      const entry = tree[name]
      if (entry == null || (typeof entry !== 'string' && !(entry instanceof ArrayBuffer))) {
        const error = new DOMException(
          `The file '${[...path, name].join('/')}' could not be found.`,
          'NotFoundError',
        )
        throw error
      }
      return mockFsFileHandle(entry, name, [...path, name])
    },
    getDirectory(name) {
      return this.getDirectoryHandle(name)
    },
    getFile(name) {
      return this.getFileHandle(name)
    },
    async removeEntry() {
      throw new Error('Cannot remove an entry from a read-only mock.')
    },
    async *keys() {
      for (const name in tree) yield name
    },
    async *values() {
      for (const name in tree) {
        const entry = tree[name]!
        if (typeof entry === 'string' || entry instanceof ArrayBuffer) {
          yield mockFsFileHandle(entry, name, [...path, name])
        } else {
          yield mockFsDirectoryHandle(entry, name, [...path, name])
        }
      }
    },
    getEntries() {
      return this.values()
    },
    async *entries() {
      for (const name in tree) {
        const entry = tree[name]!
        if (typeof entry === 'string' || entry instanceof ArrayBuffer) {
          yield [name, mockFsFileHandle(entry, name, [...path, name])]
        } else {
          yield [name, mockFsDirectoryHandle(entry, name, [...path, name])]
        }
      }
    },
    [Symbol.asyncIterator]() {
      return this.entries()
    },
  }
}
