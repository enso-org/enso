/** @file App-wide local storage keys. */
import { defineLocalStorageKey } from '#/providers/LocalStorageProvider'

export const { use: useInputBindings, useState: useInputBindingsState } = defineLocalStorageKey(
  'inputBindings',
  {
    schema: (z) =>
      z
        .record(z.string().array().readonly())
        .transform((value): { readonly [k: string]: readonly string[] } =>
          Object.fromEntries(
            Object.entries<unknown>({ ...value }).flatMap((kv) => {
              const [k, v] = kv
              return (
                  Array.isArray(v) && v.every((item): item is string => typeof item === 'string')
                ) ?
                  [[k, v]]
                : []
            }),
          ),
        ),
  },
)

export const { use: useLocalRootDirectory, useState: useLocalRootDirectoryState } =
  defineLocalStorageKey('localRootDirectory', { schema: (z) => z.string() })

export const { use: usePreferredTimeZone, useState: usePreferredTimeZoneState } =
  defineLocalStorageKey('preferredTimeZone', { schema: (z) => z.string() })

export const {
  use: useAcceptedTermsOfServiceVersion,
  useState: useAcceptedTermsOfServiceVersionState,
} = defineLocalStorageKey('termsOfService', {
  schema: (z) => z.object({ versionHash: z.string() }),
})

export const {
  use: useAcceptedPrivacyPolicyVersion,
  useState: useAcceptedPrivacyPolicyVersionState,
} = defineLocalStorageKey('privacyPolicy', {
  schema: (z) => z.object({ versionHash: z.string() }),
})
