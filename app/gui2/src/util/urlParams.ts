export interface UrlParams {
  [key: string]: string | UrlParams
}

/** Returns the parameters passed in the URL query string. */
export function urlParams(): UrlParams {
  const params: UrlParams = {}
  const urlParams = new URLSearchParams(window.location.search)
  for (const [name, value] of urlParams.entries()) {
    let obj = params
    const path = name.split('.')
    const lastSegment = path.pop()
    if (lastSegment == null) {
      console.error(`Invalid URL parameter name: '${name}'`)
    } else {
      let segment = null
      while ((segment = path.shift()) != null) {
        const nextObj = obj[segment] ?? {}
        if (typeof nextObj === 'string') {
          console.error(`Duplicate URL parameter name: '${name}'`)
        } else {
          obj[segment] = nextObj
          obj = nextObj
        }
      }
      obj[lastSegment] = value
    }
  }
  return params
}
