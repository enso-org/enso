class RustPanic extends Error {
    constructor(message) {
        super(message)
        this.name = 'RustPanic'
    }
}

export function new_panic_error(message) {
    return new RustPanic(message)
}
