import * as array from 'lib0/array'
import * as map from 'lib0/map'
import * as set from 'lib0/set'

type EventMap = { [name: PropertyKey]: any[] }
export type EventHandler<Args extends any[] = any[]> = (...args: Args) => void

export class Emitter<Events extends EventMap = EventMap> {
  private _observers: Map<PropertyKey, Set<EventHandler<any[]>>>
  constructor() {
    this._observers = map.create()
  }

  on<N extends keyof Events>(name: N, f: EventHandler<Events[N]>) {
    map.setIfUndefined(this._observers, name as PropertyKey, set.create).add(f)
  }

  once<N extends keyof Events>(name: N, f: EventHandler<Events[N]>) {
    const _f = (...args: Events[N]) => {
      this.off(name, _f)
      f(...args)
    }
    this.on(name, _f)
  }

  off<N extends keyof Events>(name: N, f: EventHandler<Events[N]>) {
    const observers = this._observers.get(name)
    if (observers !== undefined) {
      observers.delete(f as EventHandler<any[]>)
      if (observers.size === 0) {
        this._observers.delete(name)
      }
    }
  }

  emit<N extends keyof Events>(name: N, args: Events[N]) {
    // copy all listeners to an array first to make sure that no event is emitted to listeners that
    // are subscribed while the event handler is called.
    return array
      .from((this._observers.get(name) || map.create()).values())
      .forEach((f) => f(...args))
  }

  destroy() {
    this._observers = map.create()
  }
}

// Partial compatibility with node 'events' module, for the purposes of @open-rpc/client-js.
// See vite.config.ts for details.
export class EventEmitter extends Emitter<EventMap> {
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
