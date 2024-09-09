import { Octokit, App } from 'octokit'
import { program } from 'commander'
import { readFileSync, existsSync, mkdirSync} from "fs"
import { writeFile } from "fs/promises"
import { join } from 'path'
import { findGHToken, parseLinesFromJobOutput } from "./utils.js"
import JSZip from 'jszip'
import {format, sub} from "date-fns"
import { type RestEndpointMethodTypes } from "@octokit/plugin-rest-endpoint-methods"

const stdLibBenchWorkflowId = 66661001
const engineBenchWorkflowId = 67075764

type WorkflowRunResponse = RestEndpointMethodTypes["actions"]["listWorkflowRuns"]["response"]["data"]["workflow_runs"]
type WorkflowRun = WorkflowRunResponse[0]


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

async function parseLogFromJob(octoKit: Octokit, job_id: number): Promise<Map<string, number>> {
    const jobLogs = await octoKit.rest.actions.downloadJobLogsForWorkflowRun({
        owner: "enso-org",
        repo: "enso",
        job_id: job_id,
        mediaType: {
            format: "text/plain;charset=UTF-8",
        }
    })
    const lines = (jobLogs.data as string).split("\n")
    return parseLinesFromJobOutput(lines)
}

async function getWorkflowRuns(octoKit: Octokit, workflowId: number, since: Date, until: Date = new Date()): Promise<WorkflowRunResponse> {
    const perPage = 50
    const created = format(since, 'yyyy-MM-dd') + ".." + format(until, 'yyyy-MM-dd')
    const workflowRuns = await octoKit.paginate(octoKit.rest.actions.listWorkflowRuns, {
        owner: 'enso-org',
        repo: 'enso',
        workflow_id: workflowId,
        status: 'success',
        created: created,
        per_page: perPage
    })
    return workflowRuns
}

/**
 * From the given workflowId, finds an enterprise benchmark job, from it, it extracts
 * the output and parses the results from the output.
 * @param octoKit 
 * @param workflowId 
 * @returns 
 */
async function getEnterpriseResultsFromWorkflow(
    octoKit: Octokit,
    workflowId: number
): Promise<Map<string, number>> {
    const allJobs = await octoKit.rest.actions.listJobsForWorkflowRun({
        owner: 'enso-org',
        repo: 'enso',
        run_id: workflowId,
        filter: "all"
    })
    const eeJob = allJobs.data.jobs.find((job) => job.name.includes("Oracle GraalVM"))
    if (!eeJob) {
        throw new Error("Enterprise job not found")
    }
    return parseLogFromJob(octoKit, eeJob.id)
}

async function writeResultsToFile(
    results: Map<string, number>,
    benchRun: WorkflowRun,
    directory: string
) {
    if (!existsSync(directory)) {
        console.log("Creating directory", directory)
        mkdirSync(directory)
    }
    const filePath = join(directory, benchRun.id + ".json")
    const obj = {
        bench_run: {
            id: benchRun.id,
            html_url: benchRun.html_url,
            run_attempt: benchRun.run_attempt,
            event: benchRun.event,
            display_title: benchRun.display_title,
            head_commit: {
                id: benchRun.head_commit?.id,
                message: benchRun.head_commit?.message,
                timestamp: benchRun.head_commit?.timestamp,
                author: {
                    name: benchRun.head_commit?.author?.name,
                }
            }
        },
        label_score_dict: Object.fromEntries(results)
    }
    writeFile(filePath, JSON.stringify(obj, null, 2))
    console.log("Results written to", filePath)
}

async function hasEnterpriseJob(
    octoKit: Octokit,
    workflowId: number
): Promise<boolean> {
    const allJobs = await octoKit.rest.actions.listJobsForWorkflowRun({
        owner: 'enso-org',
        repo: 'enso',
        run_id: workflowId,
        filter: "all"
    })
    return allJobs.data.jobs.some((job) => job.name.includes("Oracle GraalVM"))

}


async function main() {
    program
        .name('bench-data')
        .description('Utility to fetch benchmarking data from GH Actions')
        .option('-v', '--verbose', 'Enable verbose logging')

    program.parse()

    const options = program.opts()
    console.log('cmdline options', options)

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

    const today = new Date()
    const beforeThreeMonths = sub(today, {months: 3})

    const engineBenchRunsProm = getWorkflowRuns(octoKit, engineBenchWorkflowId, beforeThreeMonths, today)
    const stdlibBenchRunsProm = getWorkflowRuns(octoKit, stdLibBenchWorkflowId, beforeThreeMonths, today);
    const [engineBenchRuns, stdlibBenchRuns] = await Promise.all([engineBenchRunsProm, stdlibBenchRunsProm])
    console.log("number of engine bench runs", engineBenchRuns.length)
    console.log("number of stdlib bench runs", stdlibBenchRuns.length)

    const engineRunsPromises = engineBenchRuns.map(async (benchRun) => {
        if (await hasEnterpriseJob(octoKit, benchRun.id)) {
            const results = await getEnterpriseResultsFromWorkflow(octoKit, benchRun.id);
            writeResultsToFile(results, benchRun, "results-ee-engine");
        } else {
            console.log("Skipping", benchRun.id, "as it does not have enterprise job")
        }
    });
    const stdlibRunsPromises = stdlibBenchRuns.map(async (benchRun) => {
        if (await hasEnterpriseJob(octoKit, benchRun.id)) {
            const results = await getEnterpriseResultsFromWorkflow(octoKit, benchRun.id);
            writeResultsToFile(results, benchRun, "results-ee-stdlib");
        } else {
            console.log("Skipping", benchRun.id, "as it does not have enterprise job")
        }
    });
    const promises = engineRunsPromises.concat(stdlibRunsPromises)
    await Promise.all(promises)
}

await main()
