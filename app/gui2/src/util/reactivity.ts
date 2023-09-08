import { computed, isRef, type Ref, type WatchSource } from 'vue'

/** Cast watch source to an observable ref. */
export function watchSourceToRef<T>(src: WatchSource<T>): Ref<T> {
  return isRef(src) ? src : computed(src)
}

/** Get the value behind a watch source at the current time. */
export function evalWatchSource<T>(src: WatchSource<T>): T {
  return isRef(src) ? src.value : src()
}
