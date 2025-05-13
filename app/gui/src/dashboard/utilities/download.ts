/** @file Functions to initiate a download. */

import type { DownloadUrlOptions, SystemApi } from '../../../env'

/**
 * Options for `download` function.
 */
export interface DownloadOptions {
  readonly url: string
  readonly name?: string | null | undefined
  readonly electronOptions?: Omit<DownloadUrlOptions, 'name' | 'url'>
}

/** Initiate a download for the specified url. */
export async function download(options: DownloadOptions) {
  let { url } = options
  const { name, electronOptions } = options

  url = new URL(url, location.toString()).toString()
  const systemApi = window.systemApi

  if (systemApi != null) {
    await downloadUsingElectron({
      url,
      name,
      downloadURL: systemApi.downloadURL,
      ...electronOptions,
    })
    return
  }

  const link = document.createElement('a')
  link.href = url
  link.download = name ?? url.match(/[^/]+$/)?.[0] ?? ''
  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
}

/** Initiate a download with the specified headers, for the specified url. */
export async function downloadWithHeaders(
  url: string,
  headers: Record<string, string>,
  name?: string,
) {
  url = new URL(url, location.toString()).toString()
  const response = await fetch(url, { headers })
  const body = await response.blob()
  const objectUrl = URL.createObjectURL(body)

  return download({ url: objectUrl, name })
}

/**
 * Options for `downloadUsingElectron`.
 */
export type DownloadUsingElectronOptions = DownloadUrlOptions & {
  readonly downloadURL: SystemApi['downloadURL']
}

/**
 * Initiate a download for the specified url using Electron's download API.
 * @throws invariant if you try to use this function in a non-Electron environment.
 */
export async function downloadUsingElectron(options: DownloadUsingElectronOptions) {
  const { downloadURL, ...rest } = options
  await downloadURL(rest)
}
