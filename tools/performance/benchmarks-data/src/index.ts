import { Octokit, App } from 'octokit'
import { program } from 'commander'
import { readFileSync, existsSync } from 'fs'
import { join } from 'path'
import { findGHToken } from "./utils.js"
import JSZip from 'jszip'

const stdLibBenchWorkflowId = 66661001
const engineBenchWorkflowId = 67075764

/**
 * Download and unzip the artifact. It is expected that the content of the zip file
 * is a single text file, either XML or JSON.
 */
async function downloadArtifact(octokit: Octokit, artifactId: number): Promise<string> {
    const artifact = await octokit.rest.actions.downloadArtifact({
        owner: 'enso-org',
        repo: 'enso',
        artifact_id: artifactId,
        archive_format: 'zip'
    });
    const data = artifact.data as ArrayBuffer;
    // unzip
    // read file
    // unzip an array buffer
    // const zip = new JSZip();
    const zip = new JSZip();
    let unzipped = await zip.loadAsync(data)
    const benchReportXml = unzipped.file("bench-report.xml")
    if (benchReportXml !== null) {
        const content = await benchReportXml.async("string")
        console.log("bench-report.xml content", content)
        return Promise.resolve(content);
    } else {
        // Try bench-report.json and validate schema
    }
    return Promise.reject();
}

async function main() {
    program
        .name('bench-data')
        .description('Utility to fetch benchmarking data from GH Actions')
        .option('-v', '--verbose', 'Enable verbose logging')

    program.parse()

    const options = program.opts()
    console.log('options', options)

    const ghToken = findGHToken()
    console.log('ghToken', ghToken)
    const octoKit = new Octokit({
        auth: ghToken,
    })
    const stdLibBenchWorkflow = await octoKit.rest.actions.getWorkflow({
        owner: 'enso-org',
        repo: 'enso',
        workflow_id: stdLibBenchWorkflowId,
    })
    console.assert(
        stdLibBenchWorkflow.data.id === stdLibBenchWorkflowId,
        'stdLibBenchWorkflowId mismatch'
    )

    const engineBenchWorkflow = await octoKit.rest.actions.getWorkflow({
        owner: 'enso-org',
        repo: 'enso',
        workflow_id: engineBenchWorkflowId,
    })
    console.assert(
        engineBenchWorkflow.data.id === engineBenchWorkflowId,
        'engineBenchWorkflowId mismatch'
    )

    const stdLibRuns = await octoKit.rest.actions.listWorkflowRuns({
        owner: 'enso-org',
        repo: 'enso',
        workflow_id: stdLibBenchWorkflowId,
        status: 'success',
    })
    const firstRun = stdLibRuns.data.workflow_runs[0]
    if (firstRun !== undefined) {
        console.log('Taking first stdlib run', firstRun.id)
        const artifacts = await octoKit.rest.actions.listWorkflowRunArtifacts({
            owner: 'enso-org',
            repo: 'enso',
            run_id: firstRun.id,
        })
        console.log('Got', artifacts.data.total_count, 'artifacts')
        const artifact = artifacts.data.artifacts[0]
        console.log('First artifact:', artifact)
        if (artifact !== undefined) {
            downloadArtifact(octoKit, artifact.id);
        }
    }
}

await main()
