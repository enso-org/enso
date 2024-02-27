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
  const baseDatalinksRoot = path.resolve(repoRoot, 'test/Base_Tests/data/')

  const example = loadDataLinkFile(path.resolve(baseDatalinksRoot, 'example-http.datalink'))
  v.expect(validateDataLink.validateDataLink(example)).toBe(true)
})

v.test('correctly validates example S3 .datalink files with the schema', () => {
  const s3datalinksRoot = path.resolve(repoRoot, 'test/AWS_Tests/data/')

  const simple = loadDataLinkFile(path.resolve(s3datalinksRoot, 'simple.datalink'))
  v.expect(validateDataLink.validateDataLink(simple)).toBe(true)

  const credentialsWithSecrets = loadDataLinkFile(
    path.resolve(s3datalinksRoot, 'credentials-with-secrets.datalink')
  )
  v.expect(validateDataLink.validateDataLink(credentialsWithSecrets)).toBe(true)

  const formatted = loadDataLinkFile(path.resolve(s3datalinksRoot, 'formatted.datalink'))
  v.expect(validateDataLink.validateDataLink(formatted)).toBe(true)
})
