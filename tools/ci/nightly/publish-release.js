const github = require('./github')

const releaseId = process.argv[2]

async function main() {
    console.log('Making release ' + releaseId + ' public.')
    await github.publishRelease(releaseId)
    console.log('Done.')
}

main().catch(err => {
    console.error(err)
    process.exit(1)
})
