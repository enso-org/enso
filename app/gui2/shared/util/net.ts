import type { ObservableV2 } from 'lib0/observable'

interface Disposable {
  dispose(): void
}

export class AbortScope {
  private ctrl: AbortController = new AbortController()
  get signal() {
    return this.ctrl.signal
  }

  dispose(reason?: string) {
    this.ctrl.abort(reason)
  }

  handleDispose(disposable: Disposable) {
    this.signal.throwIfAborted()
    this.onAbort(disposable.dispose.bind(disposable))
  }

  onAbort(listener: () => void) {
    if (this.signal.aborted) {
      setTimeout(listener, 0)
    } else {
      this.signal.addEventListener('abort', listener, { once: true })
    }
  }

  handleObserve<
    EVENTS extends { [key in keyof EVENTS]: (...arg0: any[]) => void },
    NAME extends keyof EVENTS & string,
  >(observable: ObservableV2<EVENTS>, name: NAME, f: EVENTS[NAME]) {
    if (this.signal.aborted) return
    observable.on(name, f)
    this.onAbort(() => observable.off(name, f))
    return f
  }
}
