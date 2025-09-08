/** @file Global setup for dashboard tests. */
import '$/config'
import * as jestDomMatchers from '@testing-library/jest-dom/matchers'
import { cleanup } from '@testing-library/react'
import { afterEach, expect } from 'vitest'

expect.extend(jestDomMatchers)

afterEach(() => {
  cleanup()
})
