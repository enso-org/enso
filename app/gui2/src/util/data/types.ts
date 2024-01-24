export function asNot<Not>(value: any): Excluding<Not, typeof value> {
  return value as any
}

type Excluding<Not, Value> = Value extends Not ? never : Value
