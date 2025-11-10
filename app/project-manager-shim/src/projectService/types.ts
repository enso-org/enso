import { newtypeConstructor, type Newtype } from 'enso-common/src/utilities/data/newtype'

/** A filesystem path. */
export type Path = Newtype<string, 'Path'>
export const Path = newtypeConstructor<Path>()

/** A project UUID. Only present on the local backend. */
export type UUID = Newtype<string, 'UUID'>
export const UUID = newtypeConstructor<UUID>()
