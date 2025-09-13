import { backgroundPositionForProgress, useProgressBackground } from '@/composables/progressBar'
import { assertDefined } from '@/util/assert'
import { withSetup } from '@/util/testing'
import { expect, test } from 'vitest'
import { nextTick, ref, type WatchSource } from 'vue'

function setupProgressBackground(
  progress: WatchSource<number>,
  progressId: WatchSource<number> | undefined,
) {
  const progressBackground = withSetup(() =>
    useProgressBackground(progress, {
      progressId,
    }),
  )
  assertDefined(progressBackground)
  return progressBackground
}

test('No progress ID', async () => {
  const progress = ref(10)
  const { progressStyles } = setupProgressBackground(progress, undefined)

  // Render 0: Transitional frome: No CSS transition, starting background position
  expect(progressStyles.value).not.toHaveProperty('transition')
  expect(progressStyles.value).toHaveProperty(
    'background-position-x',
    backgroundPositionForProgress(0),
  )

  // Render 1: Transitioning to provided value
  await nextTick()
  expect(progressStyles.value).toHaveProperty('transition')
  expect(progressStyles.value).toHaveProperty('background-position-x')
  expect(progressStyles.value['background-position-x']).toBe(
    backgroundPositionForProgress(progress.value),
  )
  const render1 = {
    progressStyles: progressStyles.value,
  }

  // Render 2: No changes
  await nextTick()
  expect(progressStyles.value).toBe(render1.progressStyles)

  // Render 3: Change to new value: applied immediately, CSS transition active
  await nextTick()
  progress.value = 30
  expect(progressStyles.value).toHaveProperty('transition')
  expect(progressStyles.value['background-position-x']).toBe(
    backgroundPositionForProgress(progress.value),
  )
})

test('With progress ID', async () => {
  const progress = ref(10)
  const progressId = ref(0)
  const { progressStyles } = setupProgressBackground(progress, progressId)

  // Render 0: Transitional frome: No CSS transition, starting background position
  expect(progressStyles.value).not.toHaveProperty('transition')
  expect(progressStyles.value).toHaveProperty(
    'background-position-x',
    backgroundPositionForProgress(0),
  )

  // Render 1: Transitioning to provided value
  await nextTick()
  expect(progressStyles.value).toHaveProperty('transition')
  expect(progressStyles.value).toHaveProperty('background-position-x')
  expect(progressStyles.value['background-position-x']).toBe(
    backgroundPositionForProgress(progress.value),
  )
  const render1 = {
    progressStyles: progressStyles.value,
  }

  // Render 2: No changes
  await nextTick()
  expect(progressStyles.value).toBe(render1.progressStyles)

  // Render 3: Change to new value: no transitional frame; CSS transition active.
  await nextTick()
  progress.value = 30
  expect(progressStyles.value).toHaveProperty('transition')
  expect(progressStyles.value['background-position-x']).toBe(
    backgroundPositionForProgress(progress.value),
  )

  // Render 4: Change value and progressId: Transitional frame, renders initial state.
  await nextTick()
  progress.value = 60
  progressId.value += 1
  expect(progressStyles.value).not.toHaveProperty('transition')
  expect(progressStyles.value).toHaveProperty(
    'background-position-x',
    backgroundPositionForProgress(0),
  )

  // Render 5: After transitional frame, animates to new value.
  await nextTick()
  expect(progressStyles.value).toHaveProperty('transition')
  expect(progressStyles.value['background-position-x']).toBe(
    backgroundPositionForProgress(progress.value),
  )
})
