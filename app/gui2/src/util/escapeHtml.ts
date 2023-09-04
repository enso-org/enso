const REPLACEMENTS: Record<string, string> = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;',
  '"': '&quot;',
  "'": '&#039;',
}

export function escapeHtml(unsafe: string) {
  return unsafe.replace(/[&<>"']/g, (match) => REPLACEMENTS[match])
}
