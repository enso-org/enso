/**
 * @file Tests for the Breadcrumbs component.
 */

import '@testing-library/jest-dom'
import { describe, expect, it } from 'vitest'
import { getItemsWithCollapsedItem } from './utilities'

describe('getItemsWithCollapsedItem', () => {
  it('returns the items when there is enough space', () => {
    const items = getItemsWithCollapsedItem(['Home', 'Projects', 'Current Project'])
    expect(items).toEqual(['Home', 'Projects', 'Current Project'])
  })

  it('returns the items with a collapsed item when there is not enough space', () => {
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
