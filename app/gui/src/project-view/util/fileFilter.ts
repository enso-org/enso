/** Describes allowed file types in the file browser. */
export interface FileFilter {
  name: string
  extensions: 'all' | string[]
}
