/** @file A function to initiate a download. */

// ================
// === download ===
// ================

/** Initiate a download for the specified url. */
export function download(url: string, name?: string | null) {
  url = new URL(url, location.toString()).toString()
  // Avoid using `window.systemApi` because the name is lost.
  const link = document.createElement('a')
  link.href = url
  link.download = name ?? url.match(/[^/]+$/)?.[0] ?? ''
  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
}

// ===========================
// === downloadWithHeaders ===
// ===========================

/** Initiate a download with the specified headers, for the specified url. */
export async function downloadWithHeaders(
  url: string,
  headers: Record<string, string>,
  name?: string,
) {
  url = new URL(url, location.toString()).toString()
  if ('systemApi' in window) {
    window.systemApi.downloadURL(url, headers)
  } else {
    const response = await fetch(url, { headers })
    const body = await response.blob()
    const objectUrl = URL.createObjectURL(body)
    download(objectUrl, name ?? url.match(/[^/]+$/)?.[0] ?? '')
  }
}
