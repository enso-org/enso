const github = require('./github')

const repo = process.argv[2]
const workflow_id = process.argv[3]
const ref = process.argv[4]

async function main() {
    console.log('Triggering workflow ' + workflow_id + ' in ' + repo + ' on ' + ref)
    await github.triggerWorkflow(repo, workflow_id, ref)
    console.log('Done.')
}

main().catch(err => {
    console.error(err)
    process.exit(1)
})
