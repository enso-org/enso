import { parseEntry } from '$/components/ProjectLog/projectLogData'
import { expect, test } from 'vitest'

test('parseEntry', () => {
  const LOG =
    '[INFO] [2026-04-14T09:28:02.655] [org.enso.languageserver.boot.LanguageServerComponent] Starting Language Server...'
  const parsed = parseEntry(LOG)
  expect(parsed).toEqual({
    level: 2,
    timestamp: new Date('2026-04-14T09:28:02.655'),
    module: 'org.enso.languageserver.boot.LanguageServerComponent',
    event: 'Starting Language Server...',
  })
})
