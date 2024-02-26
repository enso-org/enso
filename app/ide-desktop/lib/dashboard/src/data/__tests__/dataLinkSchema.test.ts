import * as fs from 'node:fs'
import * as path from 'node:path'

import * as v from 'vitest'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import * as jsonSchema from '#/utilities/jsonSchema'

const DEFS = SCHEMA.$defs
const DATALINK_SCHEMA = DEFS.DataLink

v.test('correctly rejects invalid values as not matching the schema', () => {
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, {})).toBe(false)
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, 'foobar')).toBe(false)
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, { foo: 'BAR' })).toBe(false)
})

function loadDataLinkFile(path: string): object {
  const text: string = fs.readFileSync(path, { encoding: 'utf-8' })
  return JSON.parse(text)
}

// We need to go up from `app/ide-desktop/lib/dashboard/` to the root of the repo
const repoRoot = '../../../../'

v.test('correctly validates example HTTP .datalink files with the schema', () => {
  const baseDatalinksRoot = path.resolve(repoRoot, 'test/Base_Tests/data/')

  const example = loadDataLinkFile(path.resolve(baseDatalinksRoot, 'example-http.datalink'))
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, example)).toBe(true)
})

v.test('correctly validates example S3 .datalink files with the schema', () => {
  const s3datalinksRoot = path.resolve(repoRoot, 'test/AWS_Tests/data/')

  const simple = loadDataLinkFile(path.resolve(s3datalinksRoot, 'simple.datalink'))
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, simple)).toBe(true)

  const credentialsWithSecrets = loadDataLinkFile(
    path.resolve(s3datalinksRoot, 'credentials-with-secrets.datalink')
  )
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, credentialsWithSecrets)).toBe(true)

  const formatted = loadDataLinkFile(path.resolve(s3datalinksRoot, 'formatted.datalink'))
  v.expect(jsonSchema.isMatch(DEFS, DATALINK_SCHEMA, formatted)).toBe(true)
})
