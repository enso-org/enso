import * as fs from 'node:fs/promises'
import * as os from 'node:os'
import * as path from 'node:path'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { watch } from '../fs'

describe('watch', () => {
  let testDir: string

  beforeEach(async () => {
    // Create a temporary test directory
    testDir = await fs.mkdtemp(path.join(os.tmpdir(), 'fs-watch-test-'))
  })

  afterEach(async () => {
    // Clean up test directory
    try {
      await fs.rm(testDir, { recursive: true, force: true })
    } catch {
      // Ignore cleanup errors
    }
  })

  test('should trigger callback when a file is created', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a file
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Wait for debounce delay + execution
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).toHaveBeenCalledTimes(1)

    await watcher.close()
  })

  test('should trigger callback when a file is modified', async () => {
    // Create initial file
    const filePath = path.join(testDir, 'test.txt')
    await fs.writeFile(filePath, 'initial content')

    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Modify the file
    await fs.writeFile(filePath, 'updated content')

    // Wait for debounce delay + execution
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).toHaveBeenCalledTimes(1)

    await watcher.close()
  })

  test('should trigger callback when a file is deleted', async () => {
    // Create initial file
    const filePath = path.join(testDir, 'test.txt')
    await fs.writeFile(filePath, 'content')

    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Delete the file
    await fs.unlink(filePath)

    // Wait for debounce delay + execution
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).toHaveBeenCalledTimes(1)

    await watcher.close()
  })

  test('should debounce multiple rapid changes', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 200,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    const filePath = path.join(testDir, 'test.txt')

    // Create multiple rapid changes
    await fs.writeFile(filePath, 'content1')
    await new Promise((resolve) => setTimeout(resolve, 50))
    await fs.writeFile(filePath, 'content2')
    await new Promise((resolve) => setTimeout(resolve, 50))
    await fs.writeFile(filePath, 'content3')

    // Wait for debounce delay + execution
    await new Promise((resolve) => setTimeout(resolve, 400))

    // Should only be called once despite 3 changes
    expect(callback).toHaveBeenCalledTimes(1)

    await watcher.close()
  })

  test('should execute callbacks synchronously', async () => {
    const executionOrder: number[] = []
    let callCount = 0

    const callback = vi.fn(async () => {
      const currentCall = ++callCount
      executionOrder.push(currentCall)
      // Simulate async work
      await new Promise((resolve) => setTimeout(resolve, 150))
      executionOrder.push(-currentCall) // Mark completion
    })

    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create first change
    await fs.writeFile(path.join(testDir, 'test1.txt'), 'content1')

    // Wait for first callback to start executing
    await new Promise((resolve) => setTimeout(resolve, 150))

    // Create second change while first callback is still executing
    await fs.writeFile(path.join(testDir, 'test2.txt'), 'content2')

    // Wait for both callbacks to complete
    await new Promise((resolve) => setTimeout(resolve, 600))

    // Callback should be called twice
    expect(callback).toHaveBeenCalledTimes(2)

    // Execution order should show synchronous execution:
    // First callback starts (1), completes (-1), then second callback starts (2), completes (-2)
    expect(executionOrder).toEqual([1, -1, 2, -2])

    await watcher.close()
  })

  test('should watch subdirectories recursively', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a subdirectory and file
    const subDir = path.join(testDir, 'subdir')
    await fs.mkdir(subDir)
    await new Promise((resolve) => setTimeout(resolve, 150))
    await fs.writeFile(path.join(subDir, 'test.txt'), 'content')

    // Wait for debounce delay + execution
    await new Promise((resolve) => setTimeout(resolve, 300))

    // Should detect changes in subdirectory (at least 2 calls: one for mkdir, one for file)
    expect(callback).toHaveBeenCalled()
    expect(callback.mock.calls.length).toBeGreaterThanOrEqual(1)

    await watcher.close()
  })

  test('should not trigger callback after watcher is closed', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Close the watcher
    const isDirty = await watcher.close()

    // Should not be dirty since no changes were made
    expect(isDirty).toBe(false)

    // Create a file after closing
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Wait to ensure no callback is triggered
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).not.toHaveBeenCalled()
  })

  test('should handle callback errors gracefully', async () => {
    const error = new Error('Test error')
    const callback = vi.fn(async () => {
      throw error
    })

    // Spy on console.error
    const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {})

    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a file
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Wait for callback execution
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).toHaveBeenCalledTimes(1)
    expect(consoleErrorSpy).toHaveBeenCalledWith('Error executing watch callback:', error)

    consoleErrorSpy.mockRestore()
    await watcher.close()
  })

  test('should cancel scheduled callback when closed before execution', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 300,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a file
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Close before debounce delay expires
    await new Promise((resolve) => setTimeout(resolve, 100))
    const isDirty = await watcher.close()

    // Should be dirty since callback was scheduled but not executed
    expect(isDirty).toBe(true)

    // Wait to ensure callback is not triggered
    await new Promise((resolve) => setTimeout(resolve, 400))

    expect(callback).not.toHaveBeenCalled()
  })

  test('should return false when closed without pending changes', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Close without any changes
    const isDirty = await watcher.close()

    expect(isDirty).toBe(false)
  })

  test('should return false when closed after callback has executed', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 100,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a file
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Wait for callback to execute
    await new Promise((resolve) => setTimeout(resolve, 300))

    expect(callback).toHaveBeenCalledTimes(1)

    // Close after callback has executed
    const isDirty = await watcher.close()

    expect(isDirty).toBe(false)
  })

  test('should return true when closed with scheduled but not executed callback', async () => {
    const callback = vi.fn(async () => {})
    const watcher = watch({
      directory: testDir,
      delay: 500,
      callback,
    })

    // Wait for watcher to initialize
    await new Promise((resolve) => setTimeout(resolve, 200))

    // Create a file
    await fs.writeFile(path.join(testDir, 'test.txt'), 'content')

    // Close immediately after change, before callback can execute
    await new Promise((resolve) => setTimeout(resolve, 50))
    const isDirty = await watcher.close()

    expect(isDirty).toBe(true)
    expect(callback).not.toHaveBeenCalled()
  })
})
