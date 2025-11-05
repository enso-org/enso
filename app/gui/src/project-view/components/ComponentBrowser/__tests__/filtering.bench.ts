import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import { mockProjectNameStore } from '$/providers/openedProjects/projectNames'
import { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import { SuggestionUpdateProcessor } from '$/providers/openedProjects/suggestionDatabase/lsUpdate'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { stdPath } from '@/util/projectPath'
import fs from 'fs'
import { bench, describe, expect } from 'vitest'

describe('Component browser filtering benchmarks', () => {
  function setup() {
    const suggestionDbList = []
    const dbPath = import.meta.dirname + '/suggestiondb'
    const dbFiles = fs.readdirSync(dbPath)
    for (const dbFile of dbFiles) {
      if (dbFile.endsWith('.json')) {
        const db = JSON.parse(fs.readFileSync(dbPath + '/' + dbFile, 'utf8'))
        const suggestionDb = new SuggestionDb()
        const projectNames = mockProjectNameStore()
        const suggestionUpdateProcessor = new SuggestionUpdateProcessor([], projectNames)
        let id = 0
        for (const entry of db) {
          const suggestionEntry = suggestionUpdateProcessor.entryFromLs(entry)
          if (!suggestionEntry.ok)
            throw new Error(`Failed to parse suggestion entry: ${suggestionEntry.error}`)
          suggestionDb.set(id++, suggestionEntry.value)
        }
        expect(suggestionDb.size).toBe(db.length)
        suggestionDbList.push([dbFile, suggestionDb] as [string, SuggestionDb])
      }
    }
    return { suggestionDbList }
  }

  const { suggestionDbList } = setup()
  let _match
  let filtering: Filtering = new Filtering({})

  function runFiltering(db: SuggestionDb) {
    for (const [_, entry] of db.entries()) {
      if (!entry) continue
      _match = filtering.filter(entry, db)
    }
  }

  for (const [fileName, db] of suggestionDbList) {
    bench(`[${fileName}] Basic filtering`, () => runFiltering(db), {
      setup: () => {
        filtering = new Filtering({})
      },
    })

    bench(`[${fileName}] Filtering for JS_Object`, () => runFiltering(db), {
      setup: () => {
        filtering = new Filtering({
          selfArg: {
            type: 'known',
            typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Json.JS_Object')], [])!,
            ancestors: [],
          },
        })
      },
    })

    bench(`[${fileName}] Filtering for Table`, () => runFiltering(db), {
      setup: () => {
        filtering = new Filtering({
          selfArg: {
            type: 'known',
            typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Table.Table')], [])!,
            ancestors: [],
          },
        })
      },
    })

    bench(`[${fileName}] Filtering for Table with pattern`, () => runFiltering(db), {
      setup: () => {
        filtering = new Filtering({
          selfArg: {
            type: 'known',
            typeInfo: TypeInfo.fromParsedTypes([stdPath('Standard.Base.Table.Table')], [])!,
            ancestors: [],
          },
          pattern: 'jo',
        })
      },
    })
  }
})
