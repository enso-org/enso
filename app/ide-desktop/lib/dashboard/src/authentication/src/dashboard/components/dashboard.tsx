/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/** @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components. */

import * as React from "react";
import reactDom from "react-dom";
import * as projectManager from "enso-studio-content/src/project_manager";

import * as auth from "../../authentication/providers/auth";
import withRouter from "../../navigation";
import * as backend from "../service";
import Templates from "./templates";
import ProjectActionButton from "./projectActionButton";
import Table from "./table";
import PermissionDisplay, * as permissionDisplay from "./permissionDisplay";
import Label, * as label from "./label";
import * as loggerProvider from "../../providers/logger";

// =============
// === Types ===
// =============

/** Column type. */
export enum Column {
    projects = "projects",
    lastModified = "last-modified",
    sharedWith = "shared-with",
    docs = "docs",
    labels = "labels",
    dataAccess = "data-access",
    usagePlan = "usage-plan",
    engine = "engine",
    ide = "ide",
}

// =================
// === Constants ===
// =================

/** The full list of columns. */
const ALL_COLUMNS: Column[] = [
    Column.projects,
    Column.lastModified,
    Column.sharedWith,
    Column.docs,
    Column.labels,
    Column.dataAccess,
    Column.usagePlan,
    Column.engine,
    Column.ide,
];

/** English names for every column. */
const COLUMN_NAME: Record<Column, string> = {
    [Column.projects]: "Projects",
    [Column.lastModified]: "Last modified",
    [Column.sharedWith]: "Shared with",
    [Column.docs]: "Docs",
    [Column.labels]: "Labels",
    [Column.dataAccess]: "Data access",
    [Column.usagePlan]: "Usage plan",
    [Column.engine]: "Engine",
    [Column.ide]: "IDE",
};

// =================
// === Component ===
// =================

interface Props {
    runningOnDesktop: boolean;
    projectManager: projectManager.ProjectManager | undefined;
}

/** Main dashboard container responsible for listing user's projects as well as other
 * interactive components. */
const Dashboard = (props: Props) => {
    const { signOut } = auth.useAuth();
    const { accessToken, organization } = auth.useFullUserSession();
    const logger = loggerProvider.useLogger();
    const backendService = backend.createBackend(accessToken, logger);
    const { runningOnDesktop, projectManager } = props;
    const [projectsList, setProjectsList] = React.useState<
        backend.ListedProject[]
    >([]);

    const renderers: Record<
        Column,
        (project: backend.ListedProject, index: number) => JSX.Element
    > = {
        [Column.projects]: (item, index) => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                <ProjectActionButton
                    project={item}
                    onOpen={() =>
                        setProjectState(index, backend.ProjectState.opened)
                    }
                    onOpenStart={() =>
                        setProjectState(
                            index,
                            backend.ProjectState.openInProgress
                        )
                    }
                    onClose={() =>
                        setProjectState(index, backend.ProjectState.closed)
                    }
                />
                <span className="px-4">{item.name}</span>
            </div>
        ),
        [Column.lastModified]: () => <>aa</>,
        [Column.sharedWith]: () => <>aa</>,
        [Column.docs]: () => <>aa</>,
        [Column.labels]: () => (
            <>
                <Label status={label.Status.warning}>outdated version</Label>
                <Label status={label.Status.severeWarning}>low resources</Label>
                <Label>do not change</Label>
            </>
        ),
        [Column.dataAccess]: () => (
            <>
                <PermissionDisplay
                    permissions={{ type: permissionDisplay.Permission.admin }}
                >
                    ./user_data
                </PermissionDisplay>
                <PermissionDisplay
                    permissions={{
                        type: permissionDisplay.Permission.regular,
                        write: true,
                        read: true,
                        exec: true,
                        docsWrite: true,
                    }}
                >
                    this folder
                </PermissionDisplay>
                <PermissionDisplay
                    permissions={{
                        type: permissionDisplay.Permission.regular,
                        write: false,
                        read: false,
                        exec: false,
                        docsWrite: false,
                    }}
                >
                    no access
                </PermissionDisplay>
            </>
        ),
        [Column.usagePlan]: () => <>aa</>,
        [Column.engine]: () => <>aa</>,
        [Column.ide]: () => <>aa</>,
    };

    const getNewProjectName = (templateName: string | undefined): string => {
        const projectNameTemplateStart = templateName
            ? `${templateName}_`
            : "New_Project_";
        const projectNameTemplate = new RegExp(
            `^${projectNameTemplateStart}(?<projectIndex>\\d+)$`
        );
        let lastProjectIndex = 1;
        projectsList.forEach((projectItem) => {
            let projectIndex: string | number | undefined;
            projectIndex = projectNameTemplate.exec(projectItem.name)?.groups
                ?.projectIndex;
            if (!projectIndex) {
                return;
            }
            projectIndex = parseInt(projectIndex, 10);
            if (projectIndex > lastProjectIndex) {
                lastProjectIndex = projectIndex;
            }
        });
        lastProjectIndex++;

        return projectNameTemplateStart + lastProjectIndex.toString();
    };

    const handleCreateProject = async (templateName: string | undefined) => {
        const newProjectName = getNewProjectName(templateName);

        if (!runningOnDesktop) {
            const newProject = await backendService.createProject({
                projectName: newProjectName,
                projectTemplateName: templateName?.toLowerCase(),
            });
            setProjectsList([
                ...projectsList,
                { ...newProject, address: null },
            ]);
        } else {
            const createdProject = await projectManager!.createProject(
                newProjectName,
                templateName
            );
            const newProject: backend.ListedProject = {
                organizationId: organization.id,
                projectId: createdProject.result.projectId,
                name: createdProject.result.projectName,
                state: { type: backend.ProjectState.created },
                packageName: "Main",
                address: null,
            };
            setProjectsList([...projectsList, newProject]);
        }
    };

    React.useEffect(() => {
        void (async (): Promise<void> => {
            let newProjectsList: backend.ListedProject[] = [];

            if (!runningOnDesktop) {
                newProjectsList = await backendService.listProjects();
            } else {
                const localProjects: any[] = (
                    await projectManager!.listProjects()
                ).result.projects;
                for (const item of localProjects) {
                    newProjectsList.push({
                        organizationId: organization.id,
                        projectId: item.id,
                        name: item.name,
                        state: { type: backend.ProjectState.created },
                        packageName: "Main",
                        address: null,
                    });
                }
            }
            reactDom.unstable_batchedUpdates(() => {
                setProjectsList(newProjectsList);
            });
        })();
    }, [accessToken]);

    const setProjectState = (
        projectItemIndex: number,
        state: backend.ProjectState
    ): void => {
        setProjectsList((currProjectList) => {
            const newProjectList = [...currProjectList];
            newProjectList[projectItemIndex]!.state.type = state;
            return newProjectList;
        });
    };

    return (
        <>
            <Templates onChange={handleCreateProject} />
            <Table<backend.ListedProject>
                items={projectsList}
                getKey={(proj) => proj.projectId}
                placeholder={
                    <>
                        You have no project yet. Go ahead and create one using
                        the form above.
                    </>
                }
                columns={ALL_COLUMNS.map((column) => ({
                    id: column,
                    name: COLUMN_NAME[column],
                    render: renderers[column],
                }))}
            />
            <button onClick={signOut}>Log out</button>
        </>
    );
};

export default withRouter(Dashboard);
