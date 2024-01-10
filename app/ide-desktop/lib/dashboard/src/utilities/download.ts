/** @file A function to initiate a download. */

/** Initiates a download for the specified url. */
export function download(url: string, name?: string) {
    const link = document.createElement('a')
    link.href = url
    link.download = name ?? url.match(/[^/]+$/)?.[0] ?? ''
    document.body.appendChild(link)
    link.click()
    document.body.removeChild(link)
}
