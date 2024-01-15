/** @file A function to initiate a download. */

/** Initiates a download for the specified url. */
export function download(url: string, name?: string | null) {
    const link = document.createElement('a')
    link.href = url
    link.download = name ?? url.match(/[^/]+$/)?.[0] ?? ''
    document.body.appendChild(link)
    link.click()
    document.body.removeChild(link)
}

/** Convert an AWS S3 URL to a HTTP URL. */
export function s3URLToHTTPURL(s3URL: string) {
    const [, bucket, key] = s3URL.match(/^s3:[/][/](.+?)[/](.+)$/) ?? []
    return `https://${bucket}.s3.amazonaws.com/${key}`
}
