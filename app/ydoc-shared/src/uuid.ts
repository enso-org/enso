import type { Uuid } from './yjsModel'

/** Return the textual representation of a UUID from its 64 high and 64 low bits. */
export function uuidFromBits(leastSigBits: bigint, mostSigBits: bigint): Uuid {
  const bits = (mostSigBits << 64n) | leastSigBits
  const string = bits.toString(16).padStart(32, '0')
  return string.replace(/(........)(....)(....)(....)(............)/, '$1-$2-$3-$4-$5') as Uuid
}

/** Return the 64 high and 64 low bits of a UUID from its textual representation. */
export function uuidToBits(uuid: string): [leastSigBits: bigint, mostSigBits: bigint] {
  const bits = BigInt('0x' + uuid.replace(/-/g, ''))
  return [bits & 0xffffffffffffffffn, bits >> 64n]
}
