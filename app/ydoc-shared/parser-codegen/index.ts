import * as fs from 'node:fs/promises'
import * as process from 'node:process'
import * as codegen from './codegen.js'
import type * as Schema from './schema.js'

const schemaPath = process.argv[2]
const outputPath = process.argv[3]

if (!schemaPath) usage()

function usage(): never {
  console.error('Usage: parser-codegen <schemaPath> [<outputPath>]')
  process.exit(1)
}

console.error(`Generating ${outputPath} from ${schemaPath}.`)

const schemaContents = await fs.readFile(schemaPath, 'utf8')
const schema: Schema.Schema = JSON.parse(schemaContents)
const code = codegen.implement(schema)
if (outputPath != null) {
  await fs.writeFile(outputPath, code)
} else {
  console.log(code)
}
