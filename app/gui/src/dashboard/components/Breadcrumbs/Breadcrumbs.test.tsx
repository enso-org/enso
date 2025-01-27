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

    expect(items).toEqual(['Home', { items: ['Team', 'Documents', 'Reports'] }, 'Current Report'])
  })
})
