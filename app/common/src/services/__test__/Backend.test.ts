import { describe, expect, it, test } from 'vitest'
import { Rfc3339DateTime } from '../../utilities/data/dateTime'
import {
  AssetType,
  compareAssets,
  doesTitleContainInvalidCharacters,
  type AnyAsset,
} from '../Backend'

describe('Backend', () => {
  it('sorts assets by modified date descending', () => {
    const assets = [
      {
        type: AssetType.file,
        modifiedAt: Rfc3339DateTime('2024-01-01'),
        title: 'a',
      },
      {
        type: AssetType.file,
        modifiedAt: Rfc3339DateTime('2024-01-02'),
        title: 'b',
      },
      {
        type: AssetType.file,
        modifiedAt: Rfc3339DateTime('2024-01-03'),
        title: 'c',
      },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { modifiedAt: '2024-01-03' },
      { modifiedAt: '2024-01-02' },
      { modifiedAt: '2024-01-01' },
    ])
  })

  it('sorts assets by type first', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'b' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'c' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'd' },
      { type: AssetType.project, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'e' },
      { type: AssetType.datalink, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'f' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { type: AssetType.directory, modifiedAt: '2024-01-01' },
      { type: AssetType.directory, modifiedAt: '2024-01-01' },
      { type: AssetType.project, modifiedAt: '2024-01-01' },
      { type: AssetType.file, modifiedAt: '2024-01-01' },
      { type: AssetType.file, modifiedAt: '2024-01-01' },
      { type: AssetType.datalink, modifiedAt: '2024-01-01' },
    ])
  })

  it('sorts titles case-insensitively', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Apple' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'banana' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'CARROT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'date' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'Apple' },
      { title: 'banana' },
      { title: 'CARROT' },
      { title: 'date' },
    ])
  })

  it('sorts titles with numbers correctly', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file10' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file2' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file1' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file20' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file1' },
      { title: 'file2' },
      { title: 'file10' },
      { title: 'file20' },
    ])
  })

  it('sorts titles with special characters', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '@special' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '#hashtag' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '$money' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '_underscore' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '_underscore' },
      { title: '@special' },
      { title: '#hashtag' },
      { title: '$money' },
    ])
  })

  it('sorts titles with emojis', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍎 apple' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍌 banana' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🥕 carrot' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🌴 palm' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '🌴 palm' },
      { title: '🍌 banana' },
      { title: '🍎 apple' },
      { title: '🥕 carrot' },
    ])
  })

  it('sorts titles with spaces correctly', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'no space' },
      {
        type: AssetType.file,
        modifiedAt: Rfc3339DateTime('2024-01-01'),
        title: 'multiple   spaces',
      },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: ' leading space' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'trailing space ' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: ' leading space' },
      { title: 'multiple   spaces' },
      { title: 'no space' },
      { title: 'trailing space ' },
    ])
  })

  it('sorts titles with accented characters', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'étoile' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'über' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'naïve' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'café' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'café' },
      { title: 'étoile' },
      { title: 'naïve' },
      { title: 'über' },
    ])
  })

  it('sorts titles with mixed alphanumeric and special characters', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file-1.txt' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file_2.txt' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file(3).txt' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file[4].txt' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file_2.txt' },
      { title: 'file-1.txt' },
      { title: 'file(3).txt' },
      { title: 'file[4].txt' },
    ])
  })

  it('sorts titles with unicode symbols', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♠️ spades' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♥️ hearts' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♦️ diamonds' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♣️ clubs' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: '♠️ spades' },
      { title: '♣️ clubs' },
      { title: '♥️ hearts' },
      { title: '♦️ diamonds' },
    ])
  })

  it('sorts titles with mixed case and numbers', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'File123' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file123' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'FILE123' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'FiLe123' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file123' },
      { title: 'File123' },
      { title: 'FiLe123' },
      { title: 'FILE123' },
    ])
  })

  it('sorts titles with parentheses and brackets', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file (copy)' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file [backup]' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file {draft}' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file <old>' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'file (copy)' },
      { title: 'file [backup]' },
      { title: 'file {draft}' },
      { title: 'file <old>' },
    ])
  })

  it('sorts titles with same letters but different capitalization', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PrOjEcT' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'project' },
      { title: 'Project' },
      { title: 'PrOjEcT' },
      { title: 'PROJECT' },
    ])
  })

  it('sorts mixed case titles with spaces', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'NEW PROJECT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'new project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New project' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'new project' },
      { title: 'New project' },
      { title: 'New Project' },
      { title: 'NEW PROJECT' },
    ])
  })

  it('sorts titles with leading capitals', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Alpha' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Beta' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'alpha' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'beta' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'alpha' },
      { title: 'Alpha' },
      { title: 'beta' },
      { title: 'Beta' },
    ])
  })

  it('sorts titles with mixed capitalization and special characters', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project_A' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project-a' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT_A' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project-A' },
    ] as AnyAsset[]

    const sorted = assets.sort((a, b) => compareAssets(a, b, 'title', 'ascending'))
    expect(sorted).toMatchObject([
      { title: 'Project_A' },
      { title: 'PROJECT_A' },
      { title: 'project-A' },
      { title: 'Project-a' },
    ])
  })

  it('sorts titles with numbers in different positions', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project1' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Pro2ject' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '10Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project10' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Pro10ject' },
    ] as AnyAsset[]

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
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1-Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project-1' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '1_Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project_1' },
    ] as AnyAsset[]

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
