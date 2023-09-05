export function assertNever(x: never): never {
  throw new Error('Unexpected object: ' + x)
}

export function assert(condition: boolean): asserts condition {
  if (!condition) throw new Error('Assertion failed')
}

export function assertUnreachable(): never {
  throw new Error('Unreachable code')
}
