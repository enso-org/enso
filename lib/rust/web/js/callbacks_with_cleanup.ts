const registeredCleanups: Set<() => void> = new Set()

interface CleanupHandle {
    cleanup: () => void
}

export function registerCleanup(doCleanup: () => void): CleanupHandle {
    function registeredCleanup() {
        registeredCleanups.delete(registeredCleanup)
        doCleanup()
    }
    registeredCleanups.add(registeredCleanup)
    return { cleanup: doCleanup }
}

export function registerEventListener(
    target: EventTarget,
    event: string,
    callback: EventListener,
    options: AddEventListenerOptions
): CleanupHandle {
    target.addEventListener(event, callback, options)
    // Do not prevent garbage collection of the event target. If it is collected, the event listener
    // will be removed automatically anyway.
    let weakTarget = new WeakRef(target)
    let weakCallback = new WeakRef(callback)
    return registerCleanup(() => {
        let target = weakTarget.deref()
        let callback = weakCallback.deref()
        if (target != null && callback != null) {
            target.removeEventListener(event, callback, options)
        }
    })
}

export function registerTimeout(callback: Function, delay: number): CleanupHandle {
    const timeoutId = setTimeout(callback, delay)
    return registerCleanup(() => clearTimeout(timeoutId))
}

export function registerInterval(callback: Function, delay: number): CleanupHandle {
    const intervalId = setInterval(callback, delay)
    return registerCleanup(() => clearInterval(intervalId))
}

export function registerAnimationFrame(callback: FrameRequestCallback): CleanupHandle {
    const frameId = requestAnimationFrame(callback)
    return registerCleanup(() => cancelAnimationFrame(frameId))
}

export function registerQueueMicrotask(callback: Function): CleanupHandle {
    let canceled = false
    queueMicrotask(() => {
        if (!canceled) {
            callback()
        }
    })
    return registerCleanup(() => {
        canceled = true
    })
}

export interface ResizeCallback {
    (width: number, height: number): void
}

export function registerResizeObserver(target: Element, callback: ResizeCallback) {
    let observer = new ResizeObserver(entries => {
        let rect = entries[entries.length - 1].contentRect
        callback(rect.width, rect.height)
    })
    observer.observe(target, { box: 'content-box' })
    return registerCleanup(() => {
        observer.disconnect()
    })
}

/**
 * Call all registered cleanup functions. This is useful to ensure that all event listeners are
 * removed and all timers are cleared.
 */
export function cleanupAllHandlers(): void {
    for (let cleanup of registeredCleanups) {
        cleanup()
    }
    registeredCleanups.clear()
}
