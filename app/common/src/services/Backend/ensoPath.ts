import { newtypeConstructor, type Newtype } from '../../utilities/data/newtype.js'

/** The path of this asset, including the root directory. */
export type EnsoPath = Newtype<string, 'EnsoPath'>
export const EnsoPath = newtypeConstructor<EnsoPath>()

/** The path string of this asset, including the root directory. */
export type EnsoPathValue = Newtype<string, 'EnsoPathValue'>
export const EnsoPathValue = newtypeConstructor<EnsoPathValue>()

/** Checks if paths are equal, ignoring subsequent `/`. */
export function ensoPathEq(a: EnsoPath, b: EnsoPath) {
  const normalize = (x: EnsoPath) => x.replaceAll(/\/+/g, '/')
  return normalize(a) === normalize(b)
}
