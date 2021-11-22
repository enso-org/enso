// The IntersectionObserver interface of the Intersection Observer API provides
// a way to asynchronously observe changes in the intersection of a target
// element with an ancestor element or with a top-level document's viewport.
// The ancestor element or viewport is referred to as the root.
//
// See also
// https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver

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

// ============================
// === IntersectionObserver ===
// ============================

let intersectionObserverPool = new Pool((...args) => new IntersectionObserver(...args))

export function intersection_observe(target, f) {
    let id = intersectionObserverPool.reserve(intersection_observer_update(f))
    intersectionObserverPool[id].observe(target)
    return id
}

export function intersection_unobserve(id) {
    intersectionObserverPool[id].disconnect()
    intersectionObserverPool.drop(id)
}

function intersection_observer_update(f) {
    return entries => {
        let rect = entries[0].boundingClientRect
        f(rect.x, rect.y, rect.width, rect.height)
    }
}
