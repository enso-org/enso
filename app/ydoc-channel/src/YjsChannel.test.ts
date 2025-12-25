import { describe, expect, it } from 'vitest'
import * as Y from 'yjs'
import { YjsChannel } from './YjsChannel.js'

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
})
