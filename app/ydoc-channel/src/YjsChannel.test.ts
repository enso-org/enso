import { describe, expect, it } from 'vitest'
import * as Y from 'yjs'
import { YjsChannel, type ChannelCodec, type TapDirection } from './YjsChannel.js'

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

  it('should send and receive ArrayBuffer messages', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<ArrayBuffer>(doc, 'test-channel')
    const channel2 = new YjsChannel<ArrayBuffer>(doc, 'test-channel')

    let receivedBuffer: ArrayBuffer | undefined

    channel2.subscribe((message) => {
      receivedBuffer = message
    })

    // Create a test ArrayBuffer with some data
    const view = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8])
    const testBuffer = view.buffer

    channel1.send(testBuffer)

    expect(receivedBuffer).toBeDefined()
    expect(receivedBuffer?.byteLength).toBe(8)

    // Verify the contents
    const receivedView = new Uint8Array(receivedBuffer!)
    expect(Array.from(receivedView)).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
  })

  it('should send and receive Uint8Array messages', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<Uint8Array>(doc, 'test-channel')
    const channel2 = new YjsChannel<Uint8Array>(doc, 'test-channel')

    let receivedArray: Uint8Array | undefined
    channel2.subscribe((message) => {
      receivedArray = message
    })

    // Create a test Uint8Array with some data
    const view = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8])

    channel1.send(view)

    expect(receivedArray).toBeDefined()
    expect(receivedArray?.byteLength).toBe(8)

    // Verify the contents
    expect(Array.from(receivedArray!)).toEqual([1, 2, 3, 4, 5, 6, 7, 8])
  })

  it('should clean up properly when closed', () => {
    const doc = new Y.Doc()
    const channel = new YjsChannel<string>(doc, 'test-channel')

    const receivedMessages: string[] = []
    channel.subscribe((message) => {
      receivedMessages.push(message)
    })

    channel.close()

    // After dispose, the channel should no longer receive messages
    const channel2 = new YjsChannel<string>(doc, 'test-channel')
    channel2.send('Message after dispose')

    expect(receivedMessages).toEqual([])
  })

  it('should deliver pre-existing messages when subscribing', () => {
    const doc = new Y.Doc()
    const channel1 = new YjsChannel<string>(doc, 'test-channel')
    const channel2 = new YjsChannel<string>(doc, 'test-channel')

    // Send messages before channel2 has any subscriber
    channel1.send('Early message 1')
    channel1.send('Early message 2')

    // Messages should be sitting in the array, unprocessed
    expect(doc.getArray('test-channel').length).toBeGreaterThan(0)

    const receivedMessages: string[] = []

    // Now subscribe should receive the pre-existing messages immediately
    channel2.subscribe((message) => {
      receivedMessages.push(message)
    })

    expect(receivedMessages).toEqual(['Early message 1', 'Early message 2'])
    // Array should be cleaned up after processing
    expect(doc.getArray('test-channel').length).toEqual(0)
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

  describe('message deletion race condition', () => {
    // These tests use separate Y.Doc instances synced via Y.applyUpdate to simulate
    // real network conditions where each party has its own document replica.

    it('should not delete local pending message when processing a remote message', () => {
      const docA = new Y.Doc()
      const docB = new Y.Doc()
      const channelA = new YjsChannel<string>(docA, 'chan')
      const channelB = new YjsChannel<string>(docB, 'chan')

      const receivedByA: string[] = []
      channelA.subscribe((m) => receivedByA.push(m))
      // B intentionally has no subscriber yet, so it won't consume/delete

      // A sends a message — it's in docA's array
      channelA.send('A1')

      // Sync A → B so docB also has 'A1'
      Y.applyUpdate(docB, Y.encodeStateAsUpdate(docA, Y.encodeStateVector(docB)), 'sync')

      // B sends a message — appended after 'A1' in docB
      channelB.send('B1')

      // Sync B → A — docA now has both 'A1' and 'B1'.
      // A's observer fires for the remote insert of 'B1'.
      // BUG: the observer does this.array.delete(0), which deletes 'A1' instead of 'B1'.
      Y.applyUpdate(docA, Y.encodeStateAsUpdate(docB, Y.encodeStateVector(docA)), 'sync')

      // A should have received B's message
      expect(receivedByA).toEqual(['B1'])

      // A's own message 'A1' should still be in the array — B hasn't consumed it yet.
      // With the bug, 'A1' gets deleted and only 'B1' (or nothing) remains.
      const remaining = docA.getArray<string>('chan').toArray()
      expect(remaining).toContain('A1')
    })

    it('should preserve message ordering across interleaved sends', () => {
      const docA = new Y.Doc()
      const docB = new Y.Doc()
      const channelA = new YjsChannel<string>(docA, 'chan')
      const channelB = new YjsChannel<string>(docB, 'chan')

      const receivedByA: string[] = []
      const receivedByB: string[] = []
      channelA.subscribe((m) => receivedByA.push(m))
      channelB.subscribe((m) => receivedByB.push(m))

      // A sends two messages before any sync
      channelA.send('A1')
      channelA.send('A2')

      // Sync A → B
      Y.applyUpdate(docB, Y.encodeStateAsUpdate(docA, Y.encodeStateVector(docB)), 'sync')

      // B should receive both messages in order
      expect(receivedByB).toEqual(['A1', 'A2'])

      // B sends a reply
      channelB.send('B1')

      // Sync B → A
      Y.applyUpdate(docA, Y.encodeStateAsUpdate(docB, Y.encodeStateVector(docA)), 'sync')

      // A should receive B's reply
      expect(receivedByA).toEqual(['B1'])

      // The array should be empty — all messages consumed by their respective receivers
      expect(docA.getArray<string>('chan').toArray()).toEqual([])
    })

    it('should handle concurrent sends from both parties without message loss', () => {
      const docA = new Y.Doc()
      const docB = new Y.Doc()
      const channelA = new YjsChannel<string>(docA, 'chan')
      const channelB = new YjsChannel<string>(docB, 'chan')

      const receivedByA: string[] = []
      const receivedByB: string[] = []
      channelA.subscribe((m) => receivedByA.push(m))
      channelB.subscribe((m) => receivedByB.push(m))

      // Both parties send before any sync (true concurrent scenario)
      channelA.send('A1')
      channelB.send('B1')

      // Sync both ways — each side receives the other's message and deletes it locally
      const updateFromA = Y.encodeStateAsUpdate(docA, Y.encodeStateVector(docB))
      const updateFromB = Y.encodeStateAsUpdate(docB, Y.encodeStateVector(docA))
      Y.applyUpdate(docB, updateFromA, 'sync')
      Y.applyUpdate(docA, updateFromB, 'sync')

      // Each party should receive exactly the other's message, not their own
      expect(receivedByA).toEqual(['B1'])
      expect(receivedByB).toEqual(['A1'])

      // After first sync, each doc still has its own locally-sent item because
      // the remote's deletion hasn't been synced back yet.
      // A second sync round propagates the deletions.
      const deletionsFromA = Y.encodeStateAsUpdate(docA, Y.encodeStateVector(docB))
      const deletionsFromB = Y.encodeStateAsUpdate(docB, Y.encodeStateVector(docA))
      Y.applyUpdate(docB, deletionsFromA, 'sync')
      Y.applyUpdate(docA, deletionsFromB, 'sync')

      // Now all messages should be consumed — arrays empty on both sides
      expect(docA.getArray<string>('chan').toArray()).toEqual([])
      expect(docB.getArray<string>('chan').toArray()).toEqual([])
    })
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

    it('should not silently discard messages after all message listeners are removed', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<string>(doc, 'test-channel')
      const channel2 = new YjsChannel<string>(doc, 'test-channel')

      const received: string[] = []
      const listener = (event: MessageEvent) => {
        received.push(event.data)
      }

      // Add then remove a message listener.
      // This sets hasMessageListeners = true, which is never reset to false.
      channel2.addEventListener('message', listener)
      channel1.send('First')
      expect(received).toEqual(['First'])

      channel2.removeEventListener('message', listener)

      // Send a message while no listeners are attached.
      // BUG: because hasMessageListeners is stuck true, the observer enters the
      // processing branch, deletes the item from the array, and calls notifyHandlers
      // which emits to zero listeners — the message is silently lost.
      channel1.send('Second')

      // The message should still be in the array, waiting for a future subscriber.
      // With the bug, it has already been deleted.
      expect(doc.getArray<string>('test-channel').toArray()).toContain('Second')

      // A new listener added later should receive the preserved message.
      const lateReceived: string[] = []
      channel2.addEventListener('message', (event) => {
        lateReceived.push(event.data)
      })
      expect(lateReceived).toEqual(['Second'])
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

    it('should emit close event when closed', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'test-channel')

      let closeEventFired = false

      channel.addEventListener('close', () => {
        closeEventFired = true
      })

      channel.close()

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

  describe('tap', () => {
    it('should receive sent messages with direction send', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'tap-test')

      const tapped: { msg: string; dir: TapDirection }[] = []
      channel.tap((msg, dir) => tapped.push({ msg, dir }))

      channel.send('hello')

      expect(tapped).toEqual([{ msg: 'hello', dir: 'send' }])
    })

    it('should receive incoming messages with direction receive', () => {
      const doc = new Y.Doc()
      const sender = new YjsChannel<string>(doc, 'tap-test')
      const receiver = new YjsChannel<string>(doc, 'tap-test')

      const tapped: { msg: string; dir: TapDirection }[] = []
      receiver.tap((msg, dir) => tapped.push({ msg, dir }))

      // Receiver needs a subscriber so the observer processes messages
      receiver.subscribe(() => {})

      sender.send('from sender')

      expect(tapped).toEqual([{ msg: 'from sender', dir: 'receive' }])
    })

    it('should not consume messages (non-destructive)', () => {
      const doc = new Y.Doc()
      const sender = new YjsChannel<string>(doc, 'tap-test')
      const receiver = new YjsChannel<string>(doc, 'tap-test')

      const tapped: string[] = []
      const received: string[] = []

      receiver.tap((msg) => tapped.push(msg))
      receiver.subscribe((msg) => received.push(msg))

      sender.send('msg1')

      // Both tap and subscribe should see the message
      expect(tapped).toEqual(['msg1'])
      expect(received).toEqual(['msg1'])
    })

    it('should support unsubscribing', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'tap-test')

      const tapped: string[] = []
      const untap = channel.tap((msg) => tapped.push(msg))

      channel.send('before')
      untap()
      channel.send('after')

      expect(tapped).toEqual(['before'])
    })

    it('should receive incoming messages even when no handlers are subscribed', () => {
      const doc = new Y.Doc()
      const sender = new YjsChannel<string>(doc, 'tap-test')
      const receiver = new YjsChannel<string>(doc, 'tap-test')

      const tapped: { msg: string; dir: TapDirection }[] = []
      receiver.tap((msg, dir) => tapped.push({ msg, dir }))

      // No subscribe() or addEventListener — only a tap is registered.
      // This mirrors the inspect scenario: the tap is set up before
      // the Java side subscribes to the channel.
      sender.send('early message')

      // Tap should still be notified
      expect(tapped).toEqual([{ msg: 'early message', dir: 'receive' }])

      // Message must remain in the array for a future handler
      expect(doc.getArray<string>('tap-test').toArray()).toContain('early message')

      // A later subscriber should still receive the message
      const received: string[] = []
      receiver.subscribe((msg) => received.push(msg))
      expect(received).toEqual(['early message'])
    })

    it('should be cleared on close', () => {
      const doc = new Y.Doc()
      const sender = new YjsChannel<string>(doc, 'tap-test')
      const receiver = new YjsChannel<string>(doc, 'tap-test')

      const tapped: string[] = []
      receiver.tap((msg) => tapped.push(msg))
      receiver.subscribe(() => {})

      sender.send('before close')
      receiver.close()
      sender.send('after close')

      expect(tapped).toEqual(['before close'])
    })
  })

  describe('notifyHandlers', () => {
    it('should deliver message to subscribers without touching the Y.Array', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'notify-test')

      const received: string[] = []
      channel.subscribe((msg) => received.push(msg))

      channel.notifyHandlers('injected message')

      expect(received).toEqual(['injected message'])
      // Message should NOT be in the array
      expect(doc.getArray<string>('notify-test').length).toBe(0)
    })

    it('should deliver message to addEventListener listeners', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'notify-test')

      const received: string[] = []
      channel.addEventListener('message', (event) => {
        received.push(event.data)
      })

      channel.notifyHandlers('injected message')

      expect(received).toEqual(['injected message'])
    })
  })

  describe('channelName', () => {
    it('should expose the channel name', () => {
      const doc = new Y.Doc()
      const channel = new YjsChannel<string>(doc, 'my-channel')
      expect(channel.channelName).toBe('my-channel')
    })
  })

  describe('ChannelCodec', () => {
    // A simple codec that doubles numbers on encode and halves on decode
    const doubleCodec: ChannelCodec<number, number> = {
      encode: (n) => n * 2,
      decode: (n) => n / 2,
    }

    it('should send and receive messages through codec', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)
      const channel2 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)

      const received: number[] = []
      channel2.subscribe((msg) => received.push(msg))

      channel1.send(5)

      // channel1 encodes 5 → 10 in array, channel2 decodes 10 → 5
      expect(received).toEqual([5])
    })

    it('should decode pre-existing messages on subscribe', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)
      const channel2 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)

      // Send before subscribing
      channel1.send(3)
      channel1.send(7)

      // Raw stored values should be encoded (doubled)
      const raw = doc.getArray<number>('codec-chan').toArray()
      expect(raw).toEqual([6, 14])

      const received: number[] = []
      channel2.subscribe((msg) => received.push(msg))

      // Drain should decode back to original values
      expect(received).toEqual([3, 7])
    })

    it('should decode pre-existing messages on addEventListener', () => {
      const doc = new Y.Doc()
      const channel1 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)
      const channel2 = new YjsChannel<number, number>(doc, 'codec-chan', doubleCodec)

      channel1.send(4)

      const received: number[] = []
      channel2.addEventListener('message', (event) => {
        received.push(event.data)
      })

      expect(received).toEqual([4])
    })
  })
})
