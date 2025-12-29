import { describe, expect, it } from 'vitest'
import * as Y from 'yjs'
import { YjsChannel } from './YjsChannel.js'

// Mock CloseEvent for Node.js environment
if (typeof globalThis.CloseEvent === 'undefined') {
  class CloseEvent extends Event {
    constructor(type: string) {
      super(type)
    }
  }
  ;(globalThis as any).CloseEvent = CloseEvent
}

describe('YjsChannel', () => {
  it('should send and receive messages between two channels', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<string>(doc, 'test-channel')
    const channel2 = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []

    // Subscribe channel2 to receive messages
    channel2.subscribe((message) => {
      receivedMessages.push(message)
    })

    // Send message from channel1
    channel1.send('Hello from channel1')

    // Channel2 should receive the message
    expect(receivedMessages).toEqual(['Hello from channel1'])
  })

  it('should not receive its own messages', () => {
    const doc = new Y.Doc()
    const channel = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []

    // Subscribe to own channel
    channel.subscribe((message) => {
      receivedMessages.push(message)
    })

    // Send message from the same channel
    channel.send('Hello from myself')

    // Should not receive own message
    expect(receivedMessages).toEqual([])
  })

  it('should allow multiple subscribers', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<string>(doc, 'test-channel')
    const channel2 = new YjsChannel<string>(doc, 'test-channel')

    const received1: string[] = []
    const received2: string[] = []

    // Multiple subscribers on channel2
    channel2.subscribe((message) => received1.push(message))
    channel2.subscribe((message) => received2.push(message))

    // Send message from channel1
    channel1.send('Broadcast message')

    // Both subscribers should receive the message
    expect(received1).toEqual(['Broadcast message'])
    expect(received2).toEqual(['Broadcast message'])

    // Send message from channel1
    channel1.send('Broadcast message 1')

    // Both subscribers should receive the message
    expect(received1).toEqual(['Broadcast message', 'Broadcast message 1'])
    expect(received2).toEqual(['Broadcast message', 'Broadcast message 1'])
  })

  it('should support unsubscribing', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<string>(doc, 'test-channel')
    const channel2 = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []

    // Subscribe and then unsubscribe
    const unsubscribe = channel2.subscribe((message) => {
      receivedMessages.push(message)
    })

    channel1.send('First message')
    unsubscribe()
    channel1.send('Second message')

    // Should only receive the first message
    expect(receivedMessages).toEqual(['First message'])
  })

  it('should handle complex message types', () => {
    interface ComplexMessage {
      id: number
      data: string
      nested: { value: boolean }
    }

    const doc = new Y.Doc()
    const channel1 = new YjsChannel<ComplexMessage>(doc, 'test-channel')
    const channel2 = new YjsChannel<ComplexMessage>(doc, 'test-channel')

    let receivedMessage: ComplexMessage | undefined

    channel2.subscribe((message) => {
      receivedMessage = message
    })

    const testMessage: ComplexMessage = {
      id: 42,
      data: 'test data',
      nested: { value: true },
    }

    channel1.send(testMessage)

    expect(receivedMessage).toEqual(testMessage)
  })

  it('should clean up properly when disposed', () => {
    const doc = new Y.Doc()
    const channel = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []
    channel.subscribe((message) => {
      receivedMessages.push(message)
    })

    channel.dispose()

    // After dispose, the channel should no longer receive messages
    const channel2 = new YjsChannel<string>(doc, 'test-channel')
    channel2.send('Message after dispose')

    expect(receivedMessages).toEqual([])
  })

  it('should cleanup internal storage after receiving', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<string>(doc, 'test-channel')
    const channel2 = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []

    // Subscribe channel2 to receive messages
    channel2.subscribe((message) => {
      receivedMessages.push(message)
    })

    // Send message from channel1
    channel1.send('Hello from channel1')

    // Channel2 should receive the message
    expect(receivedMessages).toEqual(['Hello from channel1'])

    expect(doc.getArray('test-channel').length).toEqual(0)
  })

  describe('WebSocket-compatible API', () => {
    it('should support addEventListener for message events', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const receivedMessages: string[] = []

      // Use addEventListener to listen for messages
      channel2.addEventListener('message', (event) => {
        receivedMessages.push(event.data)
      })

      // Send message from channel1
      channel1.send('Hello via addEventListener')

      // Channel2 should receive the message with MessageEvent structure
      expect(receivedMessages).toEqual(['Hello via addEventListener'])
    })

    it('should support removeEventListener', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const receivedMessages: string[] = []

      const listener = (event: MessageEvent) => {
        receivedMessages.push(event.data)
      }

      // Add and then remove event listener
      channel2.addEventListener('message', listener)
      channel1.send('First message')

      channel2.removeEventListener('message', listener)
      channel1.send('Second message')

      // Should only receive the first message
      expect(receivedMessages).toEqual(['First message'])
    })

    it('should support on/off methods', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const receivedMessages: string[] = []

      const listener = (event: MessageEvent) => {
        receivedMessages.push(event.data)
      }

      // Use on to add listener
      channel2.on('message', listener)
      channel1.send('First message')

      // Use off to remove listener
      channel2.off('message', listener)
      channel1.send('Second message')

      // Should only receive the first message
      expect(receivedMessages).toEqual(['First message'])
    })

    it('should support addEventListener with once option', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const receivedMessages: string[] = []

      // Add listener with once option
      channel2.addEventListener(
        'message',
        (event) => {
          receivedMessages.push(event.data)
        },
        { once: true },
      )

      // Send multiple messages
      channel1.send('First message')
      channel1.send('Second message')
      channel1.send('Third message')

      // Should only receive the first message due to once option
      expect(receivedMessages).toEqual(['First message'])
    })

    it('should emit close event when disposed', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'test-channel')

      let closeEventFired = false

      channel.addEventListener('close', () => {
        closeEventFired = true
      })

      channel.dispose()

      expect(closeEventFired).toBe(true)
    })

    it('should support emitOpen event', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'test-channel')

      let openEventFired = false

      channel.addEventListener('open', () => {
        openEventFired = true
      })

      expect(openEventFired).toBe(true)
    })

    it('should handle multiple addEventListener calls for the same event', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const received1: string[] = []
      const received2: string[] = []

      // Add multiple listeners
      channel2.addEventListener('message', (event) => received1.push(event.data))
      channel2.addEventListener('message', (event) => received2.push(event.data))

      // Send message
      channel1.send('Broadcast message')

      // Both listeners should receive the message
      expect(received1).toEqual(['Broadcast message'])
      expect(received2).toEqual(['Broadcast message'])
    })
  })
})
