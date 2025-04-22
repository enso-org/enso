/** @file Functions to initiate a download. */

import invariant from 'tiny-invariant'
import type { Path } from './path'

/** Initiate a download for the specified url. */
export async function download(url: string, name?: string | null, path?: Path | null) {
  const systemApi = window.systemApi

  if (systemApi != null) {
    return downloadUsingElectron(url, path, name)
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
 * Initiate a download for the specified url using Electron's download API.
 * @throws invariant if you try to use this function in a non-Electron environment.
 */
export async function downloadUsingElectron(
  url: string,
  path?: Path | null,
  filename?: string | null,
) {
  invariant(window.systemApi != null, 'Electron is not available.')
  await window.systemApi.downloadURL(url, path, filename)
}
