/**
 * @file Utility methods for ydoc server authentication.
 */

export type ConnectionData = {
  lsUrl: string
  doc: string
  user: string
}

const docNameRegex = /^[a-z0-9/-]+$/i

export function docName(pathname: string) {
  const prefix = '/project/'
  if (pathname != null && pathname.startsWith(prefix)) {
    const docName = pathname.slice(prefix.length)
    if (docNameRegex.test(docName)) {
      return docName
    }
  }
  return null
}
