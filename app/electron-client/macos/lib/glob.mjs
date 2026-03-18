import path from 'node:path'

import glob from 'fast-glob'

export async function globAbsoluteIn(base, pattern, options) {
  const patterns = Array.isArray(pattern) ? pattern : [pattern]
  return await glob(
    patterns.map((onePattern) => path.join(base, onePattern)),
    { absolute: true, ...options },
  )
}
