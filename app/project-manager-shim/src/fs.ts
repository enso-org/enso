/**
 * @file Filesystem utilities for watching directories with debouncing and synchronous callback execution.
 */

import chokidar from 'chokidar'
import type { Stats } from 'node:fs'

export type WatcherState = 'pending' | 'executed'

export interface WatchOptions {
  /** Directory to watch recursively */
  directory: string
  /** Debounce delay in milliseconds before executing the callback */
  delay: number
  /** Maximum time in milliseconds to wait before forcing callback execution, regardless of new events */
  timeout: number
  /** Async callback to execute when changes are detected */
  callback: () => Promise<void>
}

export interface Watcher {
  /**
   * Stops watching the directory and cleans up resources.
   * @returns true if the directory is dirty (callback was scheduled but not executed), false otherwise
   */
  close: () => Promise<boolean>
  /**
   * Gets the current state of the watched directory.
   * @returns 'pending' if callback is scheduled to be executed, 'executed' otherwise
   */
  getState: () => WatcherState
}

interface FileState {
  exists: boolean
  mtimeMs?: number
  size?: number
}

function toState(exists: boolean, stats?: Stats): FileState {
  return { exists, ...(stats ? { mtimeMs: stats.mtimeMs, size: stats.size } : {}) }
}

function isDuplicateState(previous: FileState | undefined, next: FileState): boolean {
  if (!previous) {
    return false
  }
  // If the file state hasn't materially changed, suppress duplicate events.
  return (
    previous.exists === next.exists &&
    previous.mtimeMs === next.mtimeMs &&
    previous.size === next.size
  )
}

/**
 * Watches a directory for filesystem changes with debouncing and synchronous callback execution.
 * @param options - Configuration options for the watcher
 * @returns A watcher instance with a close method to stop watching
 */
export function watch(options: WatchOptions): Watcher {
  const { directory, delay, timeout, callback } = options
  // Track last observed metadata to dedupe duplicate OS events for the same state.
  const lastKnownStates = new Map<string, FileState>()
  let isReady = false

  let debounceTimer: NodeJS.Timeout | null = null
  let timeoutTimer: NodeJS.Timeout | null = null
  let isExecuting = false
  let pendingExecution = false

  async function executeCallback() {
    // If already executing, mark that we need to execute again and return
    if (isExecuting) {
      pendingExecution = true
      return
    }

    isExecuting = true
    pendingExecution = false

    try {
      await callback()
    } catch (error) {
      console.error('Error executing watch callback:', error)
    } finally {
      isExecuting = false

      // If a new execution was requested while we were running, execute again
      if (pendingExecution) {
        executeCallback()
      }
    }
  }

  function scheduleCallback() {
    // Cancel any previously scheduled debounce callback
    if (debounceTimer) {
      clearTimeout(debounceTimer)
    }

    // If this is the first event, start the timeout timer
    if (!timeoutTimer) {
      timeoutTimer = setTimeout(() => {
        timeoutTimer = null
        if (debounceTimer) {
          clearTimeout(debounceTimer)
          debounceTimer = null
        }
        executeCallback()
      }, timeout)
    }

    // Schedule the debounced callback after the delay
    debounceTimer = setTimeout(() => {
      debounceTimer = null
      // Clear the timeout timer since we're executing now
      if (timeoutTimer) {
        clearTimeout(timeoutTimer)
        timeoutTimer = null
      }
      executeCallback()
    }, delay)
  }

  const watcher = chokidar.watch(directory, {
    persistent: true,
    ignoreInitial: false,
    ignorePermissionErrors: true,
    alwaysStat: true,
  })

  // Derive a normalized file state per event to suppress duplicates and only
  // trigger callbacks for meaningful state changes after the initial scan.
  //
  // This is required on Mac to prevent duplicated events emitted for some file operations.
  const handleEvent = (event: string) => {
    return function handleEventEntry(filePath: string, stats?: Stats) {
      const exists = event !== 'unlink' && event !== 'unlinkDir'
      const nextState = toState(exists, stats)
      const previousState = lastKnownStates.get(filePath)
      const isDuplicate = isDuplicateState(previousState, nextState)

      lastKnownStates.set(filePath, nextState)

      // Ignore initial scan events while still seeding baseline state.
      if (!isReady && (event === 'add' || event === 'addDir')) {
        return
      }

      if (isDuplicate) {
        return
      }

      scheduleCallback()
    }
  }

  // Listen to all change events
  watcher
    .on('add', handleEvent('add'))
    .on('change', handleEvent('change'))
    .on('unlink', handleEvent('unlink'))
    .on('addDir', handleEvent('addDir'))
    .on('unlinkDir', handleEvent('unlinkDir'))
    .on('ready', () => {
      isReady = true
    })
    .on('error', (error) => {
      console.error('Watcher error:', error)
    })

  return {
    async close() {
      // Check if there's a scheduled callback that hasn't executed yet
      const isDirty = debounceTimer !== null || timeoutTimer !== null

      // Cancel any pending scheduled callbacks
      if (debounceTimer) {
        clearTimeout(debounceTimer)
        debounceTimer = null
      }
      if (timeoutTimer) {
        clearTimeout(timeoutTimer)
        timeoutTimer = null
      }

      // Close the watcher
      await watcher.close()

      return isDirty
    },
    getState() {
      return debounceTimer !== null || timeoutTimer !== null ? 'pending' : 'executed'
    },
  }
}
