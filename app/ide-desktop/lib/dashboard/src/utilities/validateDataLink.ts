/** @file Validation functions related to Data Links. */
import type * as ajv from 'ajv/dist/2020'

import ConfiguredDataLinkValidator from '#/data/ConfiguredDataLinkValidator'

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const validateDataLink = error.assert<ajv.ValidateFunction>(() =>
  ConfiguredDataLinkValidator.getSchema('#/$defs/DataLink')
)
