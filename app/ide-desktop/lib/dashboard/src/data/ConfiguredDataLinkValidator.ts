/** @file AJV instance configured for data links. */
import Ajv from 'ajv/dist/2020'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

// eslint-disable-next-line @typescript-eslint/naming-convention
const AJV = new Ajv({ formats: { 'enso-secret': true, 'enso-file': true } })
AJV.addSchema(SCHEMA)

export default AJV
