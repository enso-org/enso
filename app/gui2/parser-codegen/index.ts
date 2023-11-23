import * as fs from 'node:fs'
import * as process from 'node:process'
import * as codegen from './codegen.js'
import * as Schema from './schema.js'

const schemaPath = process.argv[2]
const outputPath = process.argv[3]

if (!schemaPath || !outputPath) {
  console.error('Usage: parser-codegen <schemaPath> <outputPath>')
  process.exit(1)
}

console.log(`Generating ${outputPath} from ${schemaPath}.`)
const schema: Schema.Schema = JSON.parse(fs.readFileSync(schemaPath, 'utf8'))
const code = codegen.implement(schema)
fs.writeFileSync(outputPath, code)
