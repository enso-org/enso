import type { Path } from './utilities/file.js'

export interface DownloadUrlOptions {
  readonly url: string
  readonly path?: Path | null | undefined
  readonly name?: string | null | undefined
  readonly shouldUnpackProject?: boolean
  readonly showFileDialog?: boolean
}

/** Options for `download` function. */
export interface DownloadOptions {
  readonly url: string
  readonly name?: string | null | undefined
  readonly electronOptions?: Omit<DownloadUrlOptions, 'name' | 'url'>
}
