import { test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { expect } from './customExpect'
import * as locate from './locate'

test('Navigating with arrows', async ({ page }) => {
  await actions.goToGraph(page)
  // Make sure nothing else is focused right now.
  await locate.graphEditor(page).click({ position: { x: 400, y: 400 } })
  const allNodes = await locate.graphNode(page).all()
  const receiveBBoxes = () =>
    Promise.all(
      Array.from(allNodes, (node) =>
        node.boundingBox().then((bbox) => {
          assert(bbox != null)
          return bbox
        }),
      ),
    )
  const initialBBoxes = await receiveBBoxes()
  await page.keyboard.press('ArrowLeft', { delay: 500 })
  const newBBoxes = await receiveBBoxes()
  expect(newBBoxes).toEqual(
    Array.from(initialBBoxes, (bbox) =>
      expect.objectContaining({
        x: expect.not.closeTo(bbox.x),
        y: expect.closeTo(bbox.y),
      }),
    ),
  )
})
