export type Schema = {
  types: Types
  serialization: Serialization
}
export type TypeId = string
export type Types = {
  [id: TypeId]: Type
}
export type Type = {
  name: string
  fields: Fields
  parent?: string
}
export type Fields = {
  [name: string]: TypeRef
}
export type TypeRef = Class | Primitive | Sequence | Option | Result
export type Class = { class: 'type'; id: TypeId }
export type Primitive = { class: 'primitive'; type: PrimitiveType }
export type Sequence = { class: 'sequence'; type: TypeRef }
export type Option = { class: 'option'; type: TypeRef }
export type Result = { class: 'result'; type0: TypeRef; type1: TypeRef }
export type PrimitiveType = 'bool' | 'u32' | 'u64' | 'i32' | 'i64' | 'char' | 'string'

export type Serialization = {
  [id: TypeId]: Layout
}
export type Layout = {
  discriminants?: DiscriminantMap
  fields: [name: string, offset: number][]
  size: number
}
export type DiscriminantMap = {
  [discriminant: number]: TypeId
}
