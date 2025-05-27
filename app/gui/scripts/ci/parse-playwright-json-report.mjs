// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck

import fs from 'fs'
import { stdout } from 'process'

/**
 * Function to format duration in milliseconds to human-readable format
 * @param {number} ms - The duration in milliseconds
 * @returns {string} The formatted duration
 */
function formatDuration(ms) {
  // Convert milliseconds to seconds
  const totalSeconds = Math.floor(ms / 1000)

  // Extract minutes and remaining seconds
  const minutes = Math.floor(totalSeconds / 60)
  const seconds = totalSeconds % 60

  // Format as "Xm Ys"
  return `${minutes}m ${seconds}s`
}

/**
 * Reads the Playwright JSON report and formats the stats into environment variables.
 *
 */
export function format({ path }) {
  const rawData = fs.readFileSync(path, 'utf8')
  const results = JSON.parse(rawData)

  // Extract the stats
  const { duration, expected, unexpected, flaky } = results.stats

  // Format the duration as human-readable
  const readableDuration = formatDuration(duration)

  const envVars = {
    duration: readableDuration,
    success: expected,
    errors: unexpected,
    flaky,
  }

  // Output the environment variables in a format that can be used by CI systems
  // For GitHub Actions, this uses the special ::set-env syntax
  Object.entries(envVars).forEach(([key, value]) => {
    stdout.write(`${key}=${value} >> $GITHUB_OUTPUT`)

    // For general shell use (can be sourced)
    console.log(`export ${key}=${value}`)
  })

  // Also create a summary for easy viewing
  console.log('\nTest Summary:')
  console.log(`⏳ Duration: ${readableDuration}`)
  console.log(`✅ Success: ${expected}`)
  console.log(`❌ Errors: ${unexpected}`)
  console.log(`⚠️ Flaky: ${flaky}`)

  return {
    total: expected + unexpected + flaky,
    duration: readableDuration,
    success: expected,
    errors: unexpected,
    flaky,
  }
}
