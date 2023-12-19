import { Err, Ok, ResultError, rejectionToResult, type Result } from '@/util/data/result'
import { WebSocketTransport } from '@open-rpc/client-js'
import type {
  IJSONRPCNotificationResponse,
  JSONRPCRequestData,
} from '@open-rpc/client-js/build/Request'
import { Transport } from '@open-rpc/client-js/build/transports/Transport'
import type { ArgumentsType } from '@vueuse/core'
import { wait } from 'lib0/promise'
import { LsRpcError } from 'shared/languageServer'
import type { Notifications } from 'shared/languageServerTypes'
import { WebsocketClient } from 'shared/websocket'

export interface BackoffOptions<E> {
  maxRetries?: number
  retryDelay?: number
  retryDelayMultiplier?: number
  retryDelayMax?: number
  /**
   * Called when the promise return an error result, and the next retry is about to be attempted.
   * When this function returns `false`, the backoff is immediately aborted. When this function is
   * not provided, the backoff will always continue until the maximum number of retries is reached.
   */
  onBeforeRetry?: (error: ResultError<E>, retryCount: number, delay: number) => boolean | void
}

const defaultBackoffOptions: Required<BackoffOptions<any>> = {
  maxRetries: 3,
  retryDelay: 1000,
  retryDelayMultiplier: 2,
  retryDelayMax: 10000,
  onBeforeRetry: () => {},
}

/**
 * Retry a failing promise function with exponential backoff.
 */
export async function exponentialBackoff<T, E>(
  f: () => Promise<Result<T, E>>,
  backoffOptions?: BackoffOptions<E>,
): Promise<Result<T, E>> {
  const options = { ...defaultBackoffOptions, ...backoffOptions }
  for (
    let retries = 0, delay = options.retryDelay;
    ;
    retries += 1, delay = Math.min(options.retryDelayMax, delay * options.retryDelayMultiplier)
  ) {
    const result = await f()
    if (
      result.ok ||
      retries >= options.maxRetries ||
      options.onBeforeRetry(result.error, retries, delay) === false
    ) {
      return result
    }
    await wait(delay)
  }
}

export const lsRequestResult = rejectionToResult(LsRpcError)

/**
 * Retry a failing Language Server RPC call with exponential backoff. The provided async function is
 * called on each retry.
 */
export async function rpcWithRetries<T>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<LsRpcError>,
): Promise<T> {
  const result = await exponentialBackoff(() => lsRequestResult(f()), backoffOptions)
  if (result.ok) return result.value
  else {
    console.error('Too many failed retries.')
    throw result.error
  }
}

type QueueTask<State> = (state: State) => Promise<State>

export function createRpcTransport(url: string): Transport {
  if (url.startsWith('mock://')) {
    const mockName = url.slice('mock://'.length)
    return new MockTransport(mockName)
  } else {
    return new WebSocketTransport(url)
  }
}

export function createWebsocketClient(
  url: string,
  options?: { binaryType?: 'arraybuffer' | 'blob' | null; sendPings?: boolean },
): WebsocketClient {
  if (url.startsWith('mock://')) {
    const mockWs = new MockWebSocketClient(url)
    if (options?.binaryType) mockWs.binaryType = options.binaryType
    return mockWs
  } else {
    const client = new WebsocketClient(url, options)
    client.connect()
    return client
  }
}

export interface MockTransportData<Methods extends string = string> {
  (method: Methods, params: any, transport: MockTransport): Promise<any>
}

export class MockTransport extends Transport {
  static mocks: Map<string, MockTransportData> = new Map()
  constructor(public name: string) {
    super()
  }

  static addMock<Methods extends string>(name: string, data: MockTransportData<Methods>) {
    MockTransport.mocks.set(name, data as any)
  }
  connect(): Promise<any> {
    return Promise.resolve()
  }
  close(): void {}
  sendData(data: JSONRPCRequestData, timeout?: number | null): Promise<any> {
    if (Array.isArray(data)) return Promise.all(data.map((d) => this.sendData(d.request, timeout)))
    return (
      MockTransport.mocks.get(this.name)?.(data.request.method, data.request.params, this) ??
      Promise.reject()
    )
  }
  emit<N extends keyof Notifications>(method: N, params: ArgumentsType<Notifications[N]>[0]): void {
    this.transportRequestManager.transportEventChannel.emit('notification', {
      jsonrpc: '2.0',
      method,
      params,
    } as IJSONRPCNotificationResponse)
  }
}

export interface WebSocketHandler {
  (
    data: string | ArrayBufferLike | Blob | ArrayBufferView,
    send: (data: string | ArrayBufferLike | Blob | ArrayBufferView) => void,
  ): void
}

export class MockWebSocket extends EventTarget implements WebSocket {
  static mocks: Map<string, WebSocketHandler> = new Map()
  readonly CONNECTING = WebSocket.CONNECTING
  readonly OPEN = WebSocket.OPEN
  readonly CLOSING = WebSocket.CLOSING
  readonly CLOSED = WebSocket.CLOSED
  readyState: number = WebSocket.OPEN
  binaryType: BinaryType = 'blob'
  readonly bufferedAmount = 0
  readonly extensions = ''
  readonly protocol = ''
  onopen: ((this: WebSocket, ev: Event) => any) | null = null
  onclose: ((this: WebSocket, ev: CloseEvent) => any) | null = null
  onmessage: ((this: WebSocket, ev: MessageEvent<any>) => any) | null = null
  onerror: ((this: WebSocket, ev: Event) => any) | null = null

  static addMock(name: string, data: WebSocketHandler) {
    MockWebSocket.mocks.set(name, data)
  }

  constructor(
    public url: string,
    public name: string,
  ) {
    super()
    this.addEventListener('open', (ev) => this.onopen?.(ev))
    this.addEventListener('close', (ev) => this.onclose?.(ev as CloseEvent))
    // deepcode ignore InsufficientPostmessageValidation: This is not a `postMessage`.
    this.addEventListener('message', (ev) => this.onmessage?.(ev as MessageEvent<any>))
    this.addEventListener('error', (ev) => this.onerror?.(ev))
    setTimeout(() => this.dispatchEvent(new Event('open')), 0)
  }

  send(data: string | ArrayBufferLike | Blob | ArrayBufferView): void {
    MockWebSocket.mocks.get(this.name)?.(data, (data) =>
      this.dispatchEvent(new MessageEvent('message', { data })),
    )
  }
  close() {
    this.readyState = WebSocket.CLOSED
  }
}

export class MockWebSocketClient extends WebsocketClient {
  constructor(url: string) {
    super(url)
    super.connect(new MockWebSocket(url, url.slice('mock://'.length)))
  }
}

/**
 * A serializing queue of asynchronous tasks transforming a state. Each task is a function that
 * takes the current state and produces a promise to the transformed state. Each task waits for the
 * previous task to finish before starting.
 */
export class AsyncQueue<State> {
  lastTask: Promise<State>
  taskRunning = false
  queuedTasks: QueueTask<State>[] = []

  constructor(initTask: Promise<State>) {
    this.lastTask = initTask
  }

  private run() {
    if (this.taskRunning) return
    const task = this.queuedTasks.shift()
    if (task == null) return
    this.taskRunning = true
    this.lastTask = this.lastTask
      .then(
        (state) => task(state),
        (error) => {
          console.error(
            "AsyncQueue failed to run task '" + task.toString() + "' with error:",
            error,
          )
          throw error
        },
      )
      .finally(() => {
        this.taskRunning = false
        this.run()
      })
  }

  pushTask(f: QueueTask<State>) {
    this.queuedTasks.push(f)
    this.run()
  }

  clear() {
    this.queuedTasks.length = 0
  }

  async waitForCompletion(): Promise<State> {
    let lastState: State
    do {
      lastState = await this.lastTask
    } while (this.taskRunning)
    return lastState
  }
}

if (import.meta.vitest) {
  const { describe, test, expect, beforeEach, afterEach, vi } = import.meta.vitest

  beforeEach(() => {
    vi.useFakeTimers()
  })
  afterEach(() => {
    vi.useRealTimers()
  })

  describe('AsyncQueue', () => {
    test('sets initial state', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      expect(await queue.waitForCompletion()).toBe(1)
    })

    test('runs tasks in sequence', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      queue.pushTask(async (state) => {
        expect(state).toBe(1)
        await wait(100)
        return 2
      })
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 3
      })
      vi.runAllTimersAsync()
      expect(await queue.waitForCompletion()).toBe(3)
    })

    test('clear removes all not yet started tasks', async () => {
      const queue = new AsyncQueue(Promise.resolve(1))
      queue.pushTask(async (state) => {
        expect(state).toBe(1)
        await wait(100)
        return 2
      })
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 3
      })
      queue.clear()
      queue.pushTask(async (state) => {
        expect(state).toBe(2)
        return 5
      })
      vi.runAllTimersAsync()
      expect(await queue.waitForCompletion()).toBe(5)
    })
  })

  describe('exponentialBackoff', () => {
    test('runs successful task once', async () => {
      const task = vi.fn(async () => Ok(1))
      const result = await exponentialBackoff(task)
      expect(result).toEqual({ ok: true, value: 1 })
      expect(task).toHaveBeenCalledTimes(1)
    })

    test('retry failing task up to a limit', async () => {
      const task = vi.fn(async () => Err(1))
      const promise = exponentialBackoff(task, { maxRetries: 4 })
      vi.runAllTimersAsync()
      const result = await promise
      expect(result).toEqual({ ok: false, error: new ResultError(1) })
      expect(task).toHaveBeenCalledTimes(5)
    })

    test('wait before retrying', async () => {
      const task = vi.fn(async () => Err(null))
      exponentialBackoff(task, {
        maxRetries: 10,
        retryDelay: 100,
        retryDelayMultiplier: 3,
        retryDelayMax: 1000,
      })
      expect(task).toHaveBeenCalledTimes(1)
      await vi.advanceTimersByTimeAsync(100)
      expect(task).toHaveBeenCalledTimes(2)
      await vi.advanceTimersByTimeAsync(300)
      expect(task).toHaveBeenCalledTimes(3)
      await vi.advanceTimersByTimeAsync(900)
      expect(task).toHaveBeenCalledTimes(4)
      await vi.advanceTimersByTimeAsync(5000)
      expect(task).toHaveBeenCalledTimes(9)
    })

    test('retry task until success', async () => {
      const task = vi.fn()
      task.mockReturnValueOnce(Promise.resolve(Err(3)))
      task.mockReturnValueOnce(Promise.resolve(Err(2)))
      task.mockReturnValueOnce(Promise.resolve(Ok(1)))
      const promise = exponentialBackoff(task)
      vi.runAllTimersAsync()
      const result = await promise
      expect(result).toEqual({ ok: true, value: 1 })
      expect(task).toHaveBeenCalledTimes(3)
    })

    test('call retry callback', async () => {
      const task = vi.fn()
      task.mockReturnValueOnce(Promise.resolve(Err(3)))
      task.mockReturnValueOnce(Promise.resolve(Err(2)))
      task.mockReturnValueOnce(Promise.resolve(Ok(1)))
      const onBeforeRetry = vi.fn()

      const promise = exponentialBackoff(task, { onBeforeRetry })
      vi.runAllTimersAsync()
      await promise
      expect(onBeforeRetry).toHaveBeenCalledTimes(2)
      expect(onBeforeRetry).toHaveBeenNthCalledWith(1, new ResultError(3), 0, 1000)
      expect(onBeforeRetry).toHaveBeenNthCalledWith(2, new ResultError(2), 1, 2000)
    })
  })
}
