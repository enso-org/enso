import * as fs from 'node:fs'
import * as path from 'node:path'

import * as v from 'vitest'

import * as validateDataLink from '#/utilities/validateDataLink'

v.test('correctly rejects invalid values as not matching the schema', () => {
  v.expect(validateDataLink.validateDataLink({})).toBe(false)
  v.expect(validateDataLink.validateDataLink('foobar')).toBe(false)
  v.expect(validateDataLink.validateDataLink({ foo: 'BAR' })).toBe(false)
})

function loadDataLinkFile(path: string): object {
  const text: string = fs.readFileSync(path, { encoding: 'utf-8' })
  return JSON.parse(text)
}

// We need to go up from `app/ide-desktop/lib/dashboard/` to the root of the repo
const repoRoot = '../../../../'

v.test('correctly validates example HTTP .datalink files with the schema', () => {
  const baseDatalinksRoot = path.resolve(repoRoot, 'test/Base_Tests/data/datalinks/')
  const schemas = [
    'example-http.datalink',
    'example-http-format-explicit-default.datalink',
    'example-http-format-delimited.datalink',
    'example-http-format-json.datalink',
  ]

  for (const schema of schemas) {
    const json = loadDataLinkFile(path.resolve(baseDatalinksRoot, schema))
    v.expect(validateDataLink.validateDataLink(json)).toBe(true)
  }
})

v.test('correctly validates example S3 .datalink files with the schema', () => {
  const s3datalinksRoot = path.resolve(repoRoot, 'test/AWS_Tests/data/')
  const schemas = ['simple.datalink', 'credentials-with-secrets.datalink', 'formatted.datalink']

  for (const schema of schemas) {
    const json = loadDataLinkFile(path.resolve(s3datalinksRoot, schema))
    v.expect(validateDataLink.validateDataLink(json)).toBe(true)
  }
})
