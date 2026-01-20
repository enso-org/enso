#!/usr/bin/env node
import { rmSync } from 'fs'
import { dirname, join } from 'path'
import process from 'process'
import { fileURLToPath } from 'url'
import { downloadEnsoEngine } from '../dist/projectService/ensoRunner.js'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const projectRoot = join(__dirname, '../../..')
const builtDistributionPath = join(projectRoot, 'built-distribution')

// Delete the built-distribution directory
rmSync(builtDistributionPath, { recursive: true, force: true })

// Download the engine
downloadEnsoEngine(projectRoot)
  .then(() => {
    process.exit(0)
  })
  .catch((error) => {
    console.error('Failed to download engine:', error)
    process.exit(1)
  })
