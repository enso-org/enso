export default {
  version: process.env.ENSO_VERSION ?? '0.0.0-dev',
  commit: process.env.GITHUB_SHA ?? '<snapshot>',
}
