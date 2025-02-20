/**
 * @file Tests for the Breadcrumbs component.
 */

import { describe } from 'vitest'
import { getItemsWithCollapsedItem } from './utilities'

describe('getItemsWithCollapsedItem', (it) => {
  it('returns the items when there is enough space', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Current Project'])
    expect(items).toEqual(['Home', 'Projects', 'Current Project'])
  })

  it('returns the items with a collapsed item when there is not enough space', ({ expect }) => {
    const items = getItemsWithCollapsedItem([
      'Home',
      'Projects',
      'Team',
      'Documents',
      'Reports',
      'Current Report',
    ])

    expect(items).toMatchObject([
      'Home',
      { items: ['Projects', 'Team', 'Documents'] },
      'Reports',
      'Current Report',
    ])
  })

  it('handles empty input array', ({ expect }) => {
    const items = getItemsWithCollapsedItem([])
    expect(items).toEqual([])
  })

  it('handles single item array', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home'])
    expect(items).toEqual(['Home'])
  })

  it('handles custom startVisibleItemsCount', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Team', 'Documents', 'Reports'], {
      startVisibleItemsCount: 2,
    })

    expect(items).toMatchObject(['Home', 'Projects', { items: ['Team'] }, 'Documents', 'Reports'])
  })

  it('handles custom endVisibleItemsCount', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Team', 'Documents', 'Reports'], {
      endVisibleItemsCount: 3,
    })

    expect(items).toMatchObject(['Home', { items: ['Projects'] }, 'Team', 'Documents', 'Reports'])
  })

  it('handles both custom start and end counts', ({ expect }) => {
    const items = getItemsWithCollapsedItem(
      ['Home', 'Projects', 'Team', 'Documents', 'Reports', 'Final'],
      { startVisibleItemsCount: 2, endVisibleItemsCount: 1 },
    )

    expect(items).toMatchObject([
      'Home',
      'Projects',
      { items: ['Team', 'Documents', 'Reports'] },
      'Final',
    ])
  })

  it('handles when custom counts exceed array length', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Final'], {
      startVisibleItemsCount: 2,
      endVisibleItemsCount: 2,
    })

    expect(items).toEqual(['Home', 'Projects', 'Final'])
  })

  it('handles zero startVisibleItemsCount', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Team', 'Documents', 'Reports'], {
      startVisibleItemsCount: 0,
    })

    expect(items).toMatchObject([{ items: ['Home', 'Projects', 'Team'] }, 'Documents', 'Reports'])
  })

  it('handles zero endVisibleItemsCount', ({ expect }) => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Team', 'Documents', 'Reports'], {
      endVisibleItemsCount: 0,
    })

    expect(items).toMatchObject(['Home', { items: ['Projects', 'Team', 'Documents', 'Reports'] }])
  })

  it('handles non-string items', ({ expect }) => {
    const items = getItemsWithCollapsedItem([
      { id: 1, name: 'Home' },
      { id: 2, name: 'Projects' },
      { id: 3, name: 'Team' },
      { id: 4, name: 'Final' },
    ])

    expect(items).toMatchObject([
      { id: 1, name: 'Home' },
      { items: [{ id: 2, name: 'Projects' }] },
      { id: 3, name: 'Team' },
      { id: 4, name: 'Final' },
    ])
  })

  it('handles iterables other than arrays', ({ expect }) => {
    const set = new Set(['Home', 'Projects', 'Team', 'Documents'])
    const items = getItemsWithCollapsedItem(set)

    expect(items).toMatchObject(['Home', { items: ['Projects'] }, 'Team', 'Documents'])
  })
})
