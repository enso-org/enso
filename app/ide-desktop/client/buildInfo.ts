export default {
  version: process.env.ENSO_IDE_VERSION ?? '0.0.0-dev',
  commit: process.env.ENSO_IDE_COMMIT_HASH ?? '<snapshot>',
}
