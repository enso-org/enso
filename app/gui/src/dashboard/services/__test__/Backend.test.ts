import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { describe, expect, it, test } from 'vitest'
import {
  compareAssets,
  doesTitleContainInvalidCharacters,
  type AnyComparableAsset,
} from '../Backend'

describe('Backend', () => {
  it('sorts assets by modified date descending', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-02'), title: 'b' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-03'), title: 'c' },
    ]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { modifiedAt: '2024-01-03' },
      { modifiedAt: '2024-01-02' },
      { modifiedAt: '2024-01-01' },
    ])
  })

  it('sorts assets by type first', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
      { type: 'directory', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'b' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'c' },
      { type: 'directory', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'd' },
      { type: 'project', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'e' },
      { type: 'datalink', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'f' },
    ]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { type: 'directory', modifiedAt: '2024-01-01' },
      { type: 'directory', modifiedAt: '2024-01-01' },
      { type: 'project', modifiedAt: '2024-01-01' },
      { type: 'file', modifiedAt: '2024-01-01' },
      { type: 'file', modifiedAt: '2024-01-01' },
      { type: 'datalink', modifiedAt: '2024-01-01' },
    ])
  })

  it('sorts titles case-insensitively', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Apple' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'banana' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'CARROT' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'date' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'Apple' },
      { title: 'banana' },
      { title: 'CARROT' },
      { title: 'date' },
    ])
  })

  it('sorts titles with numbers correctly', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file10' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file2' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file1' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file20' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file1' },
      { title: 'file2' },
      { title: 'file10' },
      { title: 'file20' },
    ])
  })

  it('sorts titles with special characters', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '@special' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '#hashtag' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '$money' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '_underscore' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '_underscore' },
      { title: '@special' },
      { title: '#hashtag' },
      { title: '$money' },
    ])
  })

  it('sorts titles with emojis', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍎 apple' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍌 banana' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🥕 carrot' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🌴 palm' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '🌴 palm' },
      { title: '🍌 banana' },
      { title: '🍎 apple' },
      { title: '🥕 carrot' },
    ])
  })

  it('sorts titles with spaces correctly', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'no space' },
      {
        type: 'file',
        modifiedAt: Rfc3339DateTime('2024-01-01'),
        title: 'multiple   spaces',
      },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: ' leading space' },
      {
        type: 'file',
        modifiedAt: Rfc3339DateTime('2024-01-01'),
        title: 'trailing space ',
      },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: ' leading space' },
      { title: 'multiple   spaces' },
      { title: 'no space' },
      { title: 'trailing space ' },
    ])
  })

  it('sorts titles with accented characters', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'étoile' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'über' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'naïve' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'café' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'café' },
      { title: 'étoile' },
      { title: 'naïve' },
      { title: 'über' },
    ])
  })

  it('sorts titles with mixed alphanumeric and special characters', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file-1.txt' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file_2.txt' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file(3).txt' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file[4].txt' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file_2.txt' },
      { title: 'file-1.txt' },
      { title: 'file(3).txt' },
      { title: 'file[4].txt' },
    ])
  })

  it('sorts titles with unicode symbols', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♠️ spades' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♥️ hearts' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♦️ diamonds' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♣️ clubs' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '♠️ spades' },
      { title: '♣️ clubs' },
      { title: '♥️ hearts' },
      { title: '♦️ diamonds' },
    ])
  })

  it('sorts titles with mixed case and numbers', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'File123' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file123' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'FILE123' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'FiLe123' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file123' },
      { title: 'File123' },
      { title: 'FiLe123' },
      { title: 'FILE123' },
    ])
  })

  it('sorts titles with parentheses and brackets', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file (copy)' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file [backup]' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file {draft}' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file <old>' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file (copy)' },
      { title: 'file [backup]' },
      { title: 'file {draft}' },
      { title: 'file <old>' },
    ])
  })

  it('sorts titles with same letters but different capitalization', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PrOjEcT' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'project' },
      { title: 'Project' },
      { title: 'PrOjEcT' },
      { title: 'PROJECT' },
    ])
  })

  it('sorts mixed case titles with spaces', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'NEW PROJECT' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'new project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New project' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'new project' },
      { title: 'New project' },
      { title: 'New Project' },
      { title: 'NEW PROJECT' },
    ])
  })

  it('sorts titles with leading capitals', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Alpha' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Beta' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'alpha' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'beta' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'alpha' },
      { title: 'Alpha' },
      { title: 'beta' },
      { title: 'Beta' },
    ])
  })

  it('sorts titles with mixed capitalization and special characters', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project_A' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project-a' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT_A' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project-A' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'Project_A' },
      { title: 'PROJECT_A' },
      { title: 'project-A' },
      { title: 'Project-a' },
    ])
  })

  it('sorts titles with numbers in different positions', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project1' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Pro2ject' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '10Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project10' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Pro10ject' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '1Project' },
      { title: '10Project' },
      { title: 'Pro2ject' },
      { title: 'Pro10ject' },
      { title: 'Project1' },
      { title: 'Project10' },
    ])
  })

  it('sorts titles with mixed numbers and special characters', () => {
    const assets: AnyComparableAsset[] = [
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1-Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project-1' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1_Project' },
      { type: 'file', modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project_1' },
    ]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '1_Project' },
      { title: '1-Project' },
      { title: 'Project_1' },
      { title: 'Project-1' },
    ])
  })
})

test.each([
  { name: 'foo', valid: true },
  { name: 'foo/', valid: false },
  { name: 'foo\\', valid: false },
  { name: 'foo/bar', valid: false },
  { name: 'foo\\bar', valid: false },
  { name: '/bar', valid: false },
  { name: '\\bar', valid: false },
  { name: '\\', valid: false },
  { name: '/', valid: false },
  { name: '......', valid: false },
  { name: '..', valid: false },
  { name: '.', valid: false },
  { name: '~', valid: false },
  { name: '~a', valid: true },
  { name: 'a~', valid: true },
  { name: 'a.a.a.a.a.a.a.a.', valid: true },
  { name: 'a.a.a.a.a.a.a.a.a', valid: true },
  { name: '.a.a.a.a.a.a.a.a', valid: true },
  { name: 'a.a.a.a.a.a.a.a..', valid: false },
  { name: './', valid: false },
  { name: '//', valid: false },
  { name: '/\\', valid: false },
  { name: '\\/', valid: false },
])('directory name validation', (args) => {
  const { name, valid } = args

  expect(!doesTitleContainInvalidCharacters(name), `'${name}' is a valid directory name`).toBe(
    valid,
  )
})
