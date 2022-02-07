// ==============
// === IxPool ===
// ==============

class IxPool {
    constructor() {
        this.next = 0
        this.free = []
    }

    reserve() {
        let ix
        if (this.free.length == 0) {
            ix = this.next
            this.next += 1
        } else {
            ix = this.free.shift()
        }
        return ix
    }

    drop(ix) {
        this.free.unshift(ix)
    }
}

// ============
// === Pool ===
// ============

class Pool {
    constructor(cons) {
        this.cons = cons
        this.ixs = new IxPool()
    }

    reserve(...args) {
        let ix = this.ixs.reserve()
        this[ix] = this.cons(...args)
        return ix
    }

    drop(ix) {
        this.ixs.drop(ix)
        this[ix] = null
    }
}

// ======================
// === ResizeObserver ===
// ======================

let resizeObserverPool = new Pool((...args) => new ResizeObserver(...args))

export function resize_observe(target, f) {
    let id = resizeObserverPool.reserve(resize_observer_update(f))
    resizeObserverPool[id].observe(target)
    return id
}

export function resize_unobserve(id) {
    resizeObserverPool[id].disconnect()
    resizeObserverPool.drop(id)
}

function resize_observer_update(f) {
    return entries => {
        let rect = entries[0].contentRect
        f(rect.width, rect.height)
    }
}
