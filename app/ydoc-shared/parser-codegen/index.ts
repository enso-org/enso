import * as crypto from 'node:crypto'
import * as fs from 'node:fs/promises'
import * as process from 'node:process'
import type * as Schema from './schema.js'

const positionalArgs = []
let doChangeDetection = false

for (let i = 2; i < process.argv.length; i++) {
  const nextArg = process.argv[i]!
  if (nextArg === '--if-changed') doChangeDetection = true
  else if (nextArg.startsWith('--')) usage()
  else if (positionalArgs.length > 2) usage()
  else positionalArgs.push(nextArg)
}

const schemaPath = positionalArgs[0]
const outputPath = positionalArgs[1]

if (!schemaPath || !outputPath) usage()

function usage(): never {
  console.error(
    'Usage: parser-codegen [flags] <schemaPath> <outputPath>\n  flags:\n  --if-changed: only generate if schema is outdated',
  )
  process.exit(1)
}

const schemaContents = await fs.readFile(schemaPath, 'utf8')
const hash = crypto.createHash('sha1')
hash.update(schemaContents)
const schemaHash = hash.digest()

const runCodegen = async () => {
  console.log(`Generating ${outputPath} from ${schemaPath}.`)
  // load codegen lazily only when necessary, since it has a large loading time cost
  const codegen = await import('./codegen.js')
  const schema: Schema.Schema = JSON.parse(schemaContents)
  const code = codegen.implement(schema)
  await fs.writeFile(outputPath, code)
  await fs.writeFile(`${outputPath}.schema-digest`, schemaHash)
}

const checkSchemaChanged = async () => {
  try {
    const lastDigest = await fs.readFile(`${outputPath}.schema-digest`)
    return !lastDigest.equals(schemaHash)
  } catch {
    return true
  }
}

if (!doChangeDetection || (await checkSchemaChanged())) {
  await runCodegen()
} else {
  console.log('No schema changes detected since last code generation.')
}
