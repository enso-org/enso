export function sanitizeText(text, redactValues = []) {
  if (typeof text !== 'string' || redactValues.length === 0) {
    return text
  }

  let sanitized = text
  for (const value of redactValues) {
    if (typeof value === 'string' && value.length > 0) {
      sanitized = sanitized.split(value).join('[REDACTED]')
    }
  }
  return sanitized
}

export function redactArgs(args, redactValues = []) {
  if (!Array.isArray(args) || redactValues.length === 0) {
    return args
  }
  return args.map((arg) => (redactValues.includes(arg) ? '[REDACTED]' : arg))
}

export function sanitizeError(error, redactValues = []) {
  if (!error || redactValues.length === 0) {
    return error
  }

  if (typeof error.message === 'string') {
    error.message = sanitizeText(error.message, redactValues)
  }
  if (typeof error.stdout === 'string') {
    error.stdout = sanitizeText(error.stdout, redactValues)
  }
  if (typeof error.stderr === 'string') {
    error.stderr = sanitizeText(error.stderr, redactValues)
  }
  if (Array.isArray(error.output)) {
    error.output = error.output.map((value) => sanitizeText(value, redactValues))
  }
  return error
}
