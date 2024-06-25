import { expect, test } from 'vitest'
import { __TEST } from '..'

const { URL_REGEX, EMAIL_REGEX } = __TEST

test('Auto linking URL_REGEX', () => {
  expect('www.a.b').toMatch(URL_REGEX)
  expect('http://example.com').toMatch(URL_REGEX)
  expect('https://a.b').toMatch(URL_REGEX)
  expect('https://some.local').toMatch(URL_REGEX)
  expect('http://AsDf.GhI').toMatch(URL_REGEX)
  expect('https://xn--ls8h.la/').toMatch(URL_REGEX)
  expect('https://💩.la/').not.toMatch(URL_REGEX)
  expect('a.b').not.toMatch(URL_REGEX)
  expect('a@b').not.toMatch(URL_REGEX)
  expect('http://AsDf').not.toMatch(URL_REGEX)
  expect('file://hello.world').not.toMatch(URL_REGEX)
  expect('https://localhost').not.toMatch(URL_REGEX)
  expect('data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==').not.toMatch(URL_REGEX)
  expect('](http://example.com').not.toMatch(URL_REGEX)
})

test('Auto linking EMAIL_REGEX', () => {
  expect('example@gmail.com').toMatch(EMAIL_REGEX)
  expect('EXAMPLE@GMAIL.COM').toMatch(EMAIL_REGEX)
  expect('example..+hello.world@gmail.com').toMatch(EMAIL_REGEX)
  expect('a@b.bla').toMatch(EMAIL_REGEX)
  expect('(a@b.cd)').toMatch(EMAIL_REGEX)
  expect('http://example.com').not.toMatch(EMAIL_REGEX)
  expect('').not.toMatch(EMAIL_REGEX)
  expect('a@b').not.toMatch(EMAIL_REGEX)
  expect('a@b.c').not.toMatch(EMAIL_REGEX)
})
