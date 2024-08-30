/** @file AJV instance configured for datalinks. */
import type * as ajv from 'ajv/dist/2020'
import Ajv from 'ajv/dist/2020'

import SCHEMA from '#/data/datalinkSchema.json' with { type: 'json' }

import * as error from '#/utilities/error'

export const AJV = new Ajv({
  formats: {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    'enso-secret': (value) => typeof value === 'string' && value !== '',
    // eslint-disable-next-line @typescript-eslint/naming-convention
    'enso-file': true,
  },
})
AJV.addSchema(SCHEMA)

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const validateDatalink = error.assert<ajv.ValidateFunction>(() =>
  AJV.getSchema('#/$defs/DataLink'),
)
