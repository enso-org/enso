/** @file Configuration for Tailwind. */
import * as path from 'node:path'
import * as url from 'node:url'

// =================
// === Constants ===
// =================

const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =====================
// === Configuration ===
// =====================

// This is a third-party API that we do not control.
/* eslint-disable no-restricted-syntax */
export const content = [THIS_PATH + '/src/**/*.tsx']
