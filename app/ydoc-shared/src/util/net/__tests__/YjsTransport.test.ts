import type { IJSONRPCData } from '@open-rpc/client-js/build/Request.js'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import * as Y from 'yjs'
import { YjsTransport } from '../YjsTransport'

// Helper function to create JSON-RPC notification data (no id = no response expected)
function createNotification(method: string, params?: any): IJSONRPCData {
  return {
    internalID: Math.random(),
    request: {
      jsonrpc: '2.0',
      method,
      ...(params === undefined ? { params: null } : { params }),
    },
  }
}

// Polyfill DOM event types for Node.js environment
if (typeof globalThis.CloseEvent === 'undefined') {
  ;(globalThis as any).CloseEvent = class CloseEvent extends Event {
    constructor(type: string, options?: any) {
      super(type, options)
    }
  }
}

if (typeof globalThis.MessageEvent === 'undefined') {
  ;(globalThis as any).MessageEvent = class MessageEvent extends Event {
    data: any
    constructor(type: string, options?: any) {
      super(type, options)
      this.data = options?.data
    }
  }
}

if (typeof globalThis.ErrorEvent === 'undefined') {
  ;(globalThis as any).ErrorEvent = class ErrorEvent extends Event {
    error: any
    message: string
    constructor(type: string, options?: any) {
      super(type, options)
      this.error = options?.error
      this.message = options?.message || ''
    }
  }
}

describe('YjsTransport', () => {
  let doc: Y.Doc
  let transport1: YjsTransport
  let transport2: YjsTransport

  beforeEach(async () => {
    doc = new Y.Doc()
    transport1 = new YjsTransport(doc, 'test-channel')
    transport2 = new YjsTransport(doc, 'test-channel')
    await transport1.connect()
    await transport2.connect()
  })

  afterEach(() => {
    transport1.close()
    transport2.close()
  })

  test('sends message from one transport and receives on another', async () => {
    const messageListener = vi.fn()
    transport2.on('message', messageListener)

    // Send a notification through sendData
    const notification = createNotification('test.method', { test: 'data', value: 123 })
    await transport1.sendData(notification)

    // Wait a bit for the message to propagate
    await new Promise((resolve) => setTimeout(resolve, 10))

    expect(messageListener).toHaveBeenCalledTimes(1)
    expect(messageListener).toHaveBeenCalledWith(
      expect.objectContaining({
        type: 'message',
      }),
    )
    // Verify the message contains the notification data
    const receivedData = JSON.parse(messageListener.mock.calls[0]?.[0].data)
    expect(receivedData).toMatchObject({
      jsonrpc: '2.0',
      method: 'test.method',
      params: { test: 'data', value: 123 },
    })
  })

  test('multiple transports can exchange messages bidirectionally', async () => {
    const listener1 = vi.fn()
    const listener2 = vi.fn()

    transport1.on('message', listener1)
    transport2.on('message', listener2)

    // Send from transport1 to transport2
    const notification1 = createNotification('message', { text: 'from transport1' })
    await transport1.sendData(notification1)

    await new Promise((resolve) => setTimeout(resolve, 10))

    expect(listener2).toHaveBeenCalledTimes(1)
    expect(listener2).toHaveBeenCalledWith(
      expect.objectContaining({
        type: 'message',
      }),
    )
    expect(listener1).not.toHaveBeenCalled()

    // Send from transport2 to transport1
    const notification2 = createNotification('message', { text: 'from transport2' })
    await transport2.sendData(notification2)

    await new Promise((resolve) => setTimeout(resolve, 10))

    expect(listener1).toHaveBeenCalledTimes(1)
    expect(listener1).toHaveBeenCalledWith(
      expect.objectContaining({
        type: 'message',
      }),
    )
    expect(listener2).toHaveBeenCalledTimes(1) // Still only 1 call
  })

  test('subscribes and unsubscribes to message events', async () => {
    const listener = vi.fn()

    transport2.on('message', listener)

    // Send message - should be received
    await transport1.sendData(createNotification('test', { id: 1 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1)

    // Unsubscribe
    transport2.off('message', listener)

    // Send another message - should NOT be received
    await transport1.sendData(createNotification('test', { id: 2 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1) // Still only 1 call
  })

  test('supports multiple listeners for same event', async () => {
    const listener1 = vi.fn()
    const listener2 = vi.fn()
    const listener3 = vi.fn()

    transport2.on('message', listener1)
    transport2.on('message', listener2)
    transport2.on('message', listener3)

    await transport1.sendData(createNotification('test'))
    await new Promise((resolve) => setTimeout(resolve, 10))

    expect(listener1).toHaveBeenCalledTimes(1)
    expect(listener2).toHaveBeenCalledTimes(1)
    expect(listener3).toHaveBeenCalledTimes(1)
  })

  test('supports once option for one-time listeners', async () => {
    const listener = vi.fn()

    transport2.on('message', listener, { once: true })

    await transport1.sendData(createNotification('test', { id: 1 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1)

    await transport1.sendData(createNotification('test', { id: 2 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1) // Still only 1
  })

  test('supports abort signal for cancellable listeners', async () => {
    const abortController = new AbortController()
    const listener = vi.fn()

    transport2.on('message', listener, { signal: abortController.signal })

    await transport1.sendData(createNotification('test', { id: 1 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1)

    // Abort the listener
    abortController.abort()

    await transport1.sendData(createNotification('test', { id: 2 }))
    await new Promise((resolve) => setTimeout(resolve, 10))
    expect(listener).toHaveBeenCalledTimes(1) // Still only 1
  })

  test('emits open event on connect', async () => {
    const transport3 = new YjsTransport(doc, 'test-channel-2')
    const openListener = vi.fn()

    transport3.on('open', openListener)
    await transport3.connect()

    expect(openListener).toHaveBeenCalledTimes(1)
    expect(openListener).toHaveBeenCalledWith(expect.objectContaining({ type: 'open' }))

    transport3.close()
  })

  test('emits close event on close', async () => {
    const transport3 = new YjsTransport(doc, 'test-channel-3')
    const closeListener = vi.fn()

    transport3.on('close', closeListener)
    await transport3.connect()
    transport3.close()

    expect(closeListener).toHaveBeenCalledTimes(1)
    expect(closeListener).toHaveBeenCalledWith(expect.objectContaining({ type: 'close' }))
  })

  test('can close without connecting', () => {
    const transport3 = new YjsTransport(doc, 'test-channel-6')
    expect(() => transport3.close()).not.toThrow()
  })

  test('does not receive own messages', async () => {
    const listener = vi.fn()
    transport1.on('message', listener)

    await transport1.sendData(createNotification('self.message'))
    await new Promise((resolve) => setTimeout(resolve, 10))

    expect(listener).not.toHaveBeenCalled()
  })
})
