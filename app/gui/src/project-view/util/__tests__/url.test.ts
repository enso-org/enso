import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { describe, expect, test } from 'vitest'
import { parseEnsoDeeplink, type DeeplinkUrl } from '../url'

describe('parseEnsoDeeplink', () => {
  const errInvalid = Err('Invalid deeplink URL')

  const path = (pathname: string, search: string = ''): Result<DeeplinkUrl> =>
    Ok({ pathname, searchParams: new URLSearchParams(search), search })

  test.each([
    ['', errInvalid],
    ['asd', errInvalid],
    ['asd?abc=def', errInvalid],
    ['enso:', path('')],
    ['enso:/', path('')],
    ['enso://', path('')],
    ['enso:///', path('')],
    ['enso:////', path('/')],
    [
      'enso://auth?code=some_code&state=some_state',
      path('auth', '?code=some_code&state=some_state'),
    ],
    ['enso:hello/world', path('hello/world')],
    ['enso:/hello/world', path('hello/world')],
    ['enso://hello/world', path('hello/world')],
    ['enso:///hello/world', path('hello/world')],
    ['enso://hello//world', path('hello//world')],
    ['enso:///hello//world?x=y', path('hello//world', '?x=y')],
    ['enso:////hello/world', path('/hello/world')],
    ['enso://example.com/world', path('example.com/world')],
    [
      'enso://password-reset?verification_code=123abcdef',
      path('password-reset', '?verification_code=123abcdef'),
    ],
  ] satisfies [string, Result<DeeplinkUrl>][])('Parse deeplink %s', (url, expected) => {
    expect(parseEnsoDeeplink(url)).toStrictEqual(expected)
  })
})
