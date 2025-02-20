import { htmlToMarkdown } from '@/components/MarkdownEditor/htmlToMarkdown'
import * as fs from 'node:fs'
import { expect, test } from 'vitest'

test.each(['google_docs1.html.bin', 'preppin_data_2024w1.html.bin'])(
  'htmlToMarkdown: %s',
  async (file) => {
    const html = fs.readFileSync(`${__dirname}/fixtures/${file}`).toString()
    expect(await htmlToMarkdown(html)).toMatchSnapshot()
  },
)
