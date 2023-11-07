import type { Uuid } from './yjsModel'

export function uuidFromBits(leastSigBits: bigint, mostSigBits: bigint): Uuid {
  const bits = (mostSigBits << 64n) | leastSigBits
  const string = bits.toString(16).padStart(32, '0')
  return string.replace(/(........)(....)(....)(....)(............)/, '$1-$2-$3-$4-$5') as Uuid
}

export function uuidToBits(uuid: string): [leastSigBits: bigint, mostSigBits: bigint] {
  const bits = BigInt('0x' + uuid.replace(/-/g, ''))
  return [bits & 0xffffffffffffffffn, bits >> 64n]
}
