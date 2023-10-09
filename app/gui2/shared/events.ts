import { ObservableV2 } from 'lib0/observable'

type EventHandler = (...arg0: any[]) => void

type EventMap = {
  [key: string]: EventHandler
}

export class EventEmitter extends ObservableV2<EventMap> {
  emit(name: string, ...args: unknown[]): void {
    super.emit(name, args)
  }

  addListener(name: string, f: EventHandler) {
    this.on(name, f)
  }

  removeListener(name: string, f: EventHandler) {
    this.off(name, f)
  }

  removeAllListeners() {
    this.destroy()
  }
}
