import type { StringConfig } from './config'

export interface UrlParamsProps {
  ignoreKeysRegExp?: RegExp | undefined
}

/** Returns the parameters passed in the URL query string. */
export function urlParams(props: UrlParamsProps = {}): StringConfig {
  const { ignoreKeysRegExp } = props

  const params: StringConfig = {}
  const urlParams = new URLSearchParams(window.location.search)

  for (const [name, value] of urlParams.entries()) {
    let obj = params
    const path = name.split('.')
    const lastSegment = path.pop()

    if (ignoreKeysRegExp != null && ignoreKeysRegExp.test(name)) {
      continue
    }

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
