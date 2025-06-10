import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import { expect, test } from 'vitest'

test.each([
  {
    clipboard: '',
  },
  {
    clipboard: 'Text without links',
  },
  {
    clipboard: 'example.com',
  },
  {
    clipboard: 'http://example.com',
    inserted: '<http://example.com>',
  },
  {
    clipboard: 'Complete URL: http://example.com',
    inserted: 'Complete URL: <http://example.com>',
  },
  {
    clipboard: 'example.com/Address containing spaces and a < character',
  },
  {
    clipboard: 'example.com/Address resembling *bold syntax*',
  },
  {
    clipboard: 'Url: www.a.example.com, another: www.b.example.com',
  },
  {
    clipboard: 'gopher:///no/autolinking/unusual/protocols',
  },
  {
    clipboard: '/',
  },
  {
    clipboard: '//',
  },
  {
    clipboard: 'nodomain',
  },
  {
    clipboard: '/relative',
  },
  {
    clipboard: 'Sentence.',
  },
  {
    clipboard: 'example.com with trailing text',
  },
  {
    clipboard: 'Standard.Base.Math',
  },
])('Auto-linking pasted text: $clipboard', ({ clipboard, inserted }) => {
  expect(transformPastedText(clipboard)).toBe(inserted ?? clipboard)
})
