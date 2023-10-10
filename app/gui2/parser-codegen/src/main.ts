import fs from 'fs'
import * as codegen from './codegen.js'
import * as Schema from './schema.js'

const schema: Schema.Schema = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
const code = codegen.implement(schema)
fs.writeFileSync(process.argv[3], code)
