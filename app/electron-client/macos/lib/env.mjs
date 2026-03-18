export function getRequiredEnv(keys) {
  const result = {}
  for (const key of keys) {
    const value = process.env[key]
    if (value == null || value.trim() === '') {
      throw new Error(`Missing required environment variable: ${key}`)
    }
    result[key] = value
  }
  return result
}
