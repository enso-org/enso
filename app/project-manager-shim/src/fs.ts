/**
 * @file Filesystem utilities for watching directories with debouncing and synchronous callback execution.
 */

import chokidar from 'chokidar'

export interface WatchOptions {
  /** Directory to watch recursively */
  directory: string
  /** Debounce delay in milliseconds before executing the callback */
  delay: number
  /** Async callback to execute when changes are detected */
  callback: () => Promise<void>
}

export interface Watcher {
  /**
   * Stops watching the directory and cleans up resources.
   * @returns true if the directory is dirty (callback was scheduled but not executed), false otherwise
   */
  close: () => Promise<boolean>
}

/**
 * Watches a directory for filesystem changes with debouncing and synchronous callback execution.
 * @param options - Configuration options for the watcher
 * @returns A watcher instance with a close method to stop watching
 */
export function watch(options: WatchOptions): Watcher {
  const { directory, delay, callback } = options

  let debounceTimer: NodeJS.Timeout | null = null
  let isExecuting = false
  let pendingExecution = false

  const executeCallback = async () => {
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

  const scheduleCallback = () => {
    // Cancel any previously scheduled callback
    if (debounceTimer) {
      clearTimeout(debounceTimer)
    }

    // Schedule the callback after the delay
    debounceTimer = setTimeout(() => {
      debounceTimer = null
      executeCallback()
    }, delay)
  }

  // Create the watcher
  const watcher = chokidar.watch(directory, {
    persistent: true,
    ignoreInitial: true, // Don't fire events for files that already exist
    ignorePermissionErrors: true,
  })

  // Listen to all change events
  watcher
    .on('add', scheduleCallback)
    .on('change', scheduleCallback)
    .on('unlink', scheduleCallback)
    .on('addDir', scheduleCallback)
    .on('unlinkDir', scheduleCallback)
    .on('error', (error) => {
      console.error('Watcher error:', error)
    })

  return {
    close: async () => {
      // Check if there's a scheduled callback that hasn't executed yet
      const isDirty = debounceTimer !== null

      // Cancel any pending scheduled callback
      if (debounceTimer) {
        clearTimeout(debounceTimer)
        debounceTimer = null
      }

      // Close the watcher
      await watcher.close()

      return isDirty
    },
  }
}
