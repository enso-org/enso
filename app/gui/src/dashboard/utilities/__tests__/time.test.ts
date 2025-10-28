import { DAY_MS, HOUR_MS, HOURS_PER_DAY, rfc3339DurationProgress } from '#/utilities/time'
import { fc, test as fcTest } from '@fast-check/vitest'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { expect } from 'vitest'

const START_DATE = new Date(1.7e12)
const END_DATE = new Date(Number(START_DATE) + 30 * DAY_MS)
const MAX_HOURS = Number(START_DATE) / HOUR_MS

fcTest.prop({
  hoursLeft: fc.double({
    min: 0,
    max: MAX_HOURS,
    noNaN: true,
  }),
})('test `durationProgress`', ({ hoursLeft }) => {
  const daysLeft = Math.floor(hoursLeft / HOURS_PER_DAY)
  const flooredHoursLeft = Math.floor(hoursLeft)
  const now = new Date(Number(END_DATE) - hoursLeft * HOUR_MS)
  const fraction = (Number(now) - Number(START_DATE)) / (Number(END_DATE) - Number(START_DATE))
  const progress = rfc3339DurationProgress(toRfc3339(START_DATE), toRfc3339(END_DATE), now)
  expect(progress.daysLeft, '`durationProgress.daysLeft` should be correct').toBe(daysLeft)
  expect(progress.hoursLeft, '`durationProgress.hoursLeft` should be correct').toBe(
    flooredHoursLeft,
  )
  expect(progress.fraction, '`durationProgress.fraction` should be correct').toBeCloseTo(fraction)
})

fcTest.prop({
  fraction: fc.double({
    min: 0,
    max: 1,
    noNaN: true,
  }),
})('test `durationProgress.fraction`', ({ fraction }) => {
  const deltaMs = Number(END_DATE) - Number(START_DATE)
  const now = new Date(Number(START_DATE) + fraction * deltaMs)
  expect(
    rfc3339DurationProgress(toRfc3339(START_DATE), toRfc3339(END_DATE), now).fraction,
    '`durationProgress.fraction` should be correct',
  ).toBeCloseTo(fraction)
})
