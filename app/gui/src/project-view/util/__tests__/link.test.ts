import { expect, test } from 'vitest'
import { LINKABLE_EMAIL_REGEX, LINKABLE_URL_REGEX } from '../link'

const cases = {
  urls: [
    'http://example.com',
    'https://a.b',
    'https://some.local',
    'http://AsDf.GhI',
    'https://xn--ls8h.la/',
    '](http://example.com',
  ],
  emails: [
    'example@gmail.com',
    'EXAMPLE@GMAIL.COM',
    'example..+hello.world@gmail.com',
    'a@b.bla',
    '(a@b.cd)',
  ],
  neither: [
    'www.a.b',
    'https://💩.la/',
    'a.b',
    'http://AsDf',
    'file://hello.world',
    'https://localhost',
    'data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==',
    '',
    'a@b',
    'a@b.c',
  ],
}

test.each(cases.urls)('LINKABLE_URL_REGEX should match: %s', (url) =>
  expect(url).toMatch(LINKABLE_URL_REGEX),
)
test.each([...cases.neither, ...cases.emails])(
  'LINKABLE_URL_REGEX should not match: %s',
  (nonUrl) => expect(nonUrl).not.toMatch(LINKABLE_URL_REGEX),
)
test.each(cases.emails)('LINKABLE_EMAIL_REGEX should match: %s', (email) =>
  expect(email).toMatch(LINKABLE_EMAIL_REGEX),
)
test.each([...cases.neither, ...cases.urls])(
  'LINKABLE_EMAIL_REGEX should not match: %s',
  (nonEmail) => expect(nonEmail).not.toMatch(LINKABLE_EMAIL_REGEX),
)
