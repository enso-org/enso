/** Describes allowed file types in the file browser. */
export interface FileFilter {
  readonly name: string
  readonly extensions: 'all' | ReadonlyArray<string>
}
