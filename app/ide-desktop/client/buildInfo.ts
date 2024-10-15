export default {
  version: process.env.BUILD_INFO_VERSION ?? 'snapshot',
  commit: process.env.BUILD_INFO_COMMIT_HASH ?? '<snapshot>',
}
