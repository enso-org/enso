import fs from "fs";
import * as Schema from '@/schema'
import * as codegen from "@/codegen";

const schema: Schema.Schema = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))
const code = codegen.implement(schema)
fs.writeFileSync(process.argv[3], code)
