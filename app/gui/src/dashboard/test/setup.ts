/**
 * @file Global setup for dashboard tests.
 */

import * as matchers from '@testing-library/jest-dom/matchers'
import { cleanup } from '@testing-library/react'
import { MotionGlobalConfig } from 'framer-motion'

import { afterEach, expect } from 'vitest'

MotionGlobalConfig.skipAnimations = true

expect.extend(matchers)

afterEach(() => {
  cleanup()
})
