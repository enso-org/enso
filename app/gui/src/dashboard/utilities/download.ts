/** @file Functions to initiate a download. */

import type { SystemApi } from '../../../env'
import type { Path } from './path'

/** Initiate a download for the specified url. */
export async function download(url: string, name?: string | null, path?: Path | null) {
  const systemApi = window.systemApi

  if (systemApi != null) {
    return downloadUsingElectron({ url, path, filename: name, downloadURL: systemApi.downloadURL })
  }

  url = new URL(url, location.toString()).toString()
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

  return download(objectUrl, name)
}

/**
 * Options for `downloadUsingElectron`.
 */
export interface DownloadUsingElectronOptions {
  readonly downloadURL: SystemApi['downloadURL']
  /** The URL to download. */
  readonly url: string
  /** The path to save the file to. */
  readonly path?: Path | null | undefined
  /** The name of the file to save. */
  readonly filename?: string | null | undefined
}

/**
 * Initiate a download for the specified url using Electron's download API.
 * @throws invariant if you try to use this function in a non-Electron environment.
 */
export async function downloadUsingElectron(options: DownloadUsingElectronOptions) {
  await options.downloadURL(options.url, options.path, options.filename)
}
