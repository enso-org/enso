export default function escapeStringRegexp(s: string) {
  return s.replace(/[|\\{}()[\]^$+*?.]/g, '\\$&')
}
