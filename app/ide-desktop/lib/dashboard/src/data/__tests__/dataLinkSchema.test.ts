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

function testSchema(json: object, fileName: string): void {
  const validate = validateDataLink.validateDataLink
  if (!validate(json)) {
    v.assert.fail(`Failed to validate ${fileName}:\n${JSON.stringify(validate.errors, null, 2)}`)
  }
}

// We need to go up from `app/ide-desktop/lib/dashboard/` to the root of the repo
const repoRoot = '../../../../'
const baseDatalinksRoot = path.resolve(repoRoot, 'test/Base_Tests/data/datalinks/')
const s3datalinksRoot = path.resolve(repoRoot, 'test/AWS_Tests/data/')

v.test('correctly validates example HTTP .datalink files with the schema', () => {
  const schemas = [
    'example-http.datalink',
    'example-http-format-explicit-default.datalink',
    'example-http-format-delimited.datalink',
    'example-http-format-json.datalink',
  ]
  for (const schema of schemas) {
    const json = loadDataLinkFile(path.resolve(baseDatalinksRoot, schema))
    testSchema(json, schema)
  }
})

v.test('rejects invalid schemas (Base)', () => {
  const invalidSchemas = ['example-http-format-invalid.datalink']
  for (const schema of invalidSchemas) {
    const json = loadDataLinkFile(path.resolve(baseDatalinksRoot, schema))
    v.expect(validateDataLink.validateDataLink(json)).toBe(false)
  }
})

v.test('correctly validates example S3 .datalink files with the schema', () => {
  const schemas = ['simple.datalink', 'credentials-with-secrets.datalink', 'formatted.datalink']
  for (const schema of schemas) {
    const json = loadDataLinkFile(path.resolve(s3datalinksRoot, schema))
    testSchema(json, schema)
  }
})
