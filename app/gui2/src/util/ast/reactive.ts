import type { AstId } from 'ydoc-shared/ast'
import { type FixedMap, MutableModule } from 'ydoc-shared/ast'
import { customRef, markRaw } from 'vue'
import * as Y from 'yjs'

const WrappedValue = Symbol('wrapped')

export function reactiveModule(doc: Y.Doc): {
  module: MutableModule
  triggerAst: (id: AstId) => void
} {
  const triggers = new WeakMap<object, () => void>()
  const tracks = new WeakMap<object, () => void>()
  const wrap = <Fields>(wrapped: FixedMap<Fields>) => {
    if (Object.hasOwnProperty.bind(wrapped)(WrappedValue)) return wrapped
    const track = () => {
      const track = tracks.get(wrapped)
      if (track) {
        track()
      } else {
        const { track, trigger } = customRef((track, trigger) => ({
          get: () => ({ track, trigger }),
          set: () => {},
        })).value
        triggers.set(wrapped, trigger)
        tracks.set(wrapped, track)
        track()
      }
    }
    return Object.defineProperty(
      {
        get: <Key extends string & keyof Fields>(key: Key) => {
          track()
          return wrapped.get(key)
        },
        entries: () => {
          track()
          return wrapped.entries()
        },
        clone: () => {
          track()
          return wrapped.clone()
        },
        has: (key: string) => {
          track()
          return wrapped.has(key)
        },
        toJSON: () => {
          track()
          return wrapped.toJSON()
        },
        set: <Key extends string & keyof Fields>(key: Key, value: Fields[Key]) =>
          wrapped.set(key, value),
      },
      WrappedValue,
      Object.assign(Object.create(null), { value: wrapped }),
    ) satisfies FixedMap<Fields>
  }
  const module = markRaw(new MutableModule(doc, wrap))
  const triggerAst = (id: AstId) => {
    const fields = module.get(id)?.fields
    if (!fields) return
    triggers.get((fields as any)[WrappedValue])?.()
  }
  return { module, triggerAst }
}
