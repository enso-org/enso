import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { describe, expect, it } from 'vitest'
import { AssetType, compareAssets, type AnyAsset } from '../Backend'

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

  it('sorts assets by title if modified dates are equal', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'g' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'b' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'c' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'd' },
      { type: AssetType.project, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'e' },
      { type: AssetType.datalink, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'f' },
      { type: AssetType.datalink, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { type: AssetType.directory, modifiedAt: '2024-01-01', title: 'b' },
      { type: AssetType.directory, modifiedAt: '2024-01-01', title: 'd' },
      { type: AssetType.project, modifiedAt: '2024-01-01', title: 'e' },
      { type: AssetType.file, modifiedAt: '2024-01-01', title: 'a' },
      { type: AssetType.file, modifiedAt: '2024-01-01', title: 'c' },
      { type: AssetType.file, modifiedAt: '2024-01-01', title: 'g' },
      { type: AssetType.datalink, modifiedAt: '2024-01-01', title: 'a' },
      { type: AssetType.datalink, modifiedAt: '2024-01-01', title: 'f' },
    ])
  })

  it('sorts by type, then by modified date, then by title', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'd' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2021-01-01'), title: 'b' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2023-01-01'), title: 'c' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2020-01-01'), title: 'd' },
      { type: AssetType.directory, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'e' },
      { type: AssetType.project, modifiedAt: Rfc3339DateTime('2021-01-01'), title: 'f' },
      { type: AssetType.datalink, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'g' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'a' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { type: AssetType.directory, modifiedAt: '2024-01-01', title: 'e' },
      { type: AssetType.directory, modifiedAt: '2020-01-01', title: 'd' },
      { type: AssetType.project, modifiedAt: '2021-01-01', title: 'f' },
      { type: AssetType.file, modifiedAt: '2024-01-01', title: 'a' },
      { type: AssetType.file, modifiedAt: '2024-01-01', title: 'd' },
      { type: AssetType.file, modifiedAt: '2023-01-01', title: 'c' },
      { type: AssetType.file, modifiedAt: '2021-01-01', title: 'b' },
      { type: AssetType.datalink, modifiedAt: '2024-01-01', title: 'g' },
    ])
  })
  it('sorts titles case-insensitively', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Apple' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'banana' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'CARROT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'date' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'file1' },
      { title: 'file10' },
      { title: 'file2' },
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: '#hashtag' },
      { title: '$money' },
      { title: '@special' },
      { title: '_underscore' },
    ])
  })

  it('sorts titles with emojis', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍎 apple' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🍌 banana' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🥕 carrot' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '🌴 palm' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
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

    const sorted = assets.sort(compareAssets)
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'café' },
      { title: 'naïve' },
      { title: 'étoile' },
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'file(3).txt' },
      { title: 'file-1.txt' },
      { title: 'file[4].txt' },
      { title: 'file_2.txt' },
    ])
  })

  it('sorts titles with unicode symbols', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♠️ spades' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♥️ hearts' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♦️ diamonds' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: '♣️ clubs' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'File123' },
      { title: 'file123' },
      { title: 'FILE123' },
      { title: 'FiLe123' },
    ])
  })

  it('sorts titles with parentheses and brackets', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file (copy)' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file [backup]' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file {draft}' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'file <old>' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'file (copy)' },
      { title: 'file <old>' },
      { title: 'file [backup]' },
      { title: 'file {draft}' },
    ])
  })

  it('sorts titles with same letters but different capitalization', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PrOjEcT' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'Project' },
      { title: 'PROJECT' },
      { title: 'project' },
      { title: 'PrOjEcT' },
    ])
  })

  it('sorts mixed case titles with spaces', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New Project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'NEW PROJECT' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'new project' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'New project' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'New Project' },
      { title: 'NEW PROJECT' },
      { title: 'new project' },
      { title: 'New project' },
    ])
  })

  it('sorts titles with leading capitals', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Alpha' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Beta' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'alpha' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'beta' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'Alpha' },
      { title: 'alpha' },
      { title: 'Beta' },
      { title: 'beta' },
    ])
  })

  it('sorts titles with mixed capitalization and special characters', () => {
    const assets = [
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project_A' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'Project-a' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'PROJECT_A' },
      { type: AssetType.file, modifiedAt: Rfc3339DateTime('2024-01-01'), title: 'project-A' },
    ] as AnyAsset[]

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: 'Project-a' },
      { title: 'project-A' },
      { title: 'Project_A' },
      { title: 'PROJECT_A' },
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: '10Project' },
      { title: '1Project' },
      { title: 'Pro10ject' },
      { title: 'Pro2ject' },
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

    const sorted = assets.sort(compareAssets)
    expect(sorted).toMatchObject([
      { title: '1-Project' },
      { title: '1_Project' },
      { title: 'Project-1' },
      { title: 'Project_1' },
    ])
  })
})
