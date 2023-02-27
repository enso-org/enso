// FIXME [NP]: docs

export class UnreachableCaseError extends Error {
    constructor(value: never) {
        // FIXME [NP]: doc
        // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
        super(`Unreachable case: ${value}`)
    }
}
