/** @file Playwright non-browser testing configuration. While Playwright is not designed for
 * non-browser testing, it avoids the fragmentation of installing a different testing framework
 * for other tests. */
import * as test from '@playwright/test'

export default test.defineConfig({
    testDir: './test',
})
