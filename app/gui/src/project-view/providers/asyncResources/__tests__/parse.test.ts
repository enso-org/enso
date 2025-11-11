import { urlParse } from '@/util/url'
import { EnsoPath } from 'enso-common/src/services/Backend'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { describe, expect, test } from 'vitest'
import { parseResourceUrl } from '../parse'

describe('parseResourceUrl', () => {
  const asEnsoPath = (ensoPath: string) => Ok({ kind: 'ensoPath', ensoPath: EnsoPath(ensoPath) })
  const asProjectPath = (relativePath: string, uploading = false) =>
    Ok({ kind: 'projectRelative', relativePath, uploading })
  const asWebUrl = (url: string) => Ok({ kind: 'webUrl', url: urlParse(url)! })

  const errEmpty = Err('Expected non-empty resource URL')
  const errOutsideProject = Err('Resource path outside of project directory')
  const errUnsupported = (url: string) => Err('Unsupported resource URL: ' + url)
  const errUnsupportedProtocol = (protocol: string) => Err('Unsupported URL protocol: ' + protocol)

  test.each([
    ['', errEmpty],
    ['', errEmpty, ['src', 'Main.enso']],
    ['enso://some/path', asEnsoPath('enso://some/path')],
    ['enso://some/path with spaces', asEnsoPath('enso://some/path with spaces')],
    ['enso://some/:path', asEnsoPath('enso://some/:path')],
    ['/enso://some/path', errUnsupported('/enso://some/path')],
    ['http://example.com', asWebUrl('http://example.com')],
    ['/a/b/c?xyz', asProjectPath('a/b/c')],
    ['/a/b/c', asProjectPath('a/b/c'), ['src', 'Main.enso']],
    ['/a/b/c?uploading', asProjectPath('a/b/c', true), ['src', 'Main.enso']],
    ['//a/b/c', asProjectPath('a/b/c')],
    ['a/b//c', asProjectPath('src/a/b/c'), ['src', 'Main.enso']],
    ['a/b/c', errUnsupported('a/b/c')],
    ['a/b/c/', asProjectPath('src/a/b/c/'), ['src', 'Main.enso']],
    ['/a/b/>c', errUnsupported('/a/b/>c'), ['src', 'Main.enso']],
    ['something', errUnsupported('something')],
    ['hello0', asProjectPath('src/subdir/hello0'), ['src', 'subdir', 'Mod.enso']],
    ['../hello1', asProjectPath('hello1'), ['src', 'Mod.enso']],
    ['../hello2', asProjectPath('a/hello2'), ['a', 'b', 'Mod.enso']],
    ['../../hello3', asProjectPath('hello3'), ['a', 'b', 'Mod.enso']],
    ['../../../hello4', errOutsideProject, ['a', 'b', 'Mod.enso']],
    ['/../foo', errOutsideProject, ['a', 'b', 'c', 'd', 'Mod.enso']],
    ['//../bar', errOutsideProject, ['a', 'b', 'c', 'd', 'Mod.enso']],
    ['../../../../foo2?uploading', asProjectPath('foo2', true), ['a', 'b', 'c', 'd', 'Mod.enso']],
    ['../../../../foo3', errOutsideProject, ['a', 'b', 'c', 'Mod.enso']],
    ['../x/./a/b/.././../y/z', asProjectPath('m/x/y/z'), ['m', 'n', 'Mod.enso']],
    ['../x/./a/b/.././../y/z', asProjectPath('x/y/z'), ['m', 'Mod.enso']],
    ['../x/./a/b/.././../y/z', errOutsideProject, ['Mod.enso']],
    ['../../hello/bla/../x', asProjectPath('hello/x'), ['src', 'subdir', 'Mod.enso']],
    ['/', asProjectPath(''), ['src', 'Main.enso']],
    ['./', asProjectPath('src/'), ['src', 'Main.enso']],
    ['./x', asProjectPath('src/x'), ['src', 'Main.enso']],
    ['project://example.com', errUnsupportedProtocol('project:')],
    ['ws://example.com', errUnsupportedProtocol('ws:')],
    ['example.com', errUnsupported('example.com')],
    ['example.com', asProjectPath('example.com'), []],
    ['is-it-email@gmail.com', errUnsupported('is-it-email@gmail.com')],
    ['is-it-email@gmail.com', asProjectPath('a/b/is-it-email@gmail.com'), ['a', 'b', 'Mod.enso']],
  ] satisfies [string, ReturnType<typeof parseResourceUrl>, string[]?][])(
    'Resource URL %s parses correctly',
    (input, expected, segments?) => {
      expect(parseResourceUrl(input, segments)).toStrictEqual(expected)
    },
  )
})
