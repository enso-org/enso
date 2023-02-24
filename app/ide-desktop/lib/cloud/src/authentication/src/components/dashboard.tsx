/**
 * @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components.
 */

import * as React from 'react'
import { FC, useEffect, useState } from 'react'
import {unstable_batchedUpdates as batchedUpdate} from "react-dom";

import { useAuth, useFullUserSession } from '../authentication';

import withRouter from '../navigation'
import {Project, ProjectState, useBackend} from "../api";
import {Templates} from "./templates";
import {ProjectActionButton} from "./projectActionButton";
import {ProjectManager} from "enso-studio-content/src/project_manager";



// ==========================
// === dashboardContainer ===
// ==========================

export interface DashboardProps {
    runningOnDesktop: boolean;
    projectManager: ProjectManager | undefined;
}

const columns = [
    "Projects",
    "Last modified",
    "Shared with",
    "Labels",
    "Data access",
    "Usage plan",
    "Engine",
    "IDE",
];

const tableHeaders = columns.map((columnName, index) => {
    return (
        <th
            key={index}
            className="px-6 align-middle border border-solid py-3 text-xs border-l-0 border-r-1 border-t-0 border-b-0 whitespace-nowrap font-semibold text-left"
        >
            {columnName}
        </th>
    );
});

const dashboardContainer: FC<DashboardProps> = (props: DashboardProps) => {
    const { signOut } = useAuth();
    const { accessToken, organization } = useFullUserSession();
    // FIXME [NP]: add provider for this
    const backend = useBackend();
    const { runningOnDesktop, projectManager } = props;
    const [projectsList, setProjectsList] = useState<Project[]>([]);

    const getNewProjectName = (templateName: string | undefined): string => {
        const projectNameTemplateStart = templateName ? `${templateName}_` : "New_Project_";
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
            const newProject = await backend.createProject({
                projectName: newProjectName,
                projectTemplateName: templateName?.toLowerCase()
            });
            setProjectsList([...projectsList, ...[newProject]]);
        } else {
            const createdProject = await projectManager!.createProject(newProjectName, templateName);
            const newProject = {
                organizationId: organization.id,
                projectId: createdProject["result"].projectId,
                name: createdProject["result"].projectName,
                state: {"type": "Created"},
                packageName: "Main",
                address: null,
                ami: null,
                ideVersion: null,
                engineVersion: null
            } as Project
            setProjectsList([...projectsList, ...[newProject]]);
        }
    };

    useEffect(() => {
        void (async (): Promise<void> => {
            let newProjectsList: Project[] = [];

            await backend.listProjects()
            if (!runningOnDesktop) {
                newProjectsList = await backend.listProjects();
            } else {
                const localProjects: any[] = (await projectManager!.listProjects())["result"]["projects"]
                for (let item of localProjects) {
                    newProjectsList.push({
                        organizationId: organization.id,
                        projectId: item.id,
                        name: item.name,
                        state: {"type": "Created"},
                        packageName: "Main",
                        address: null,
                        ami: null,
                        ideVersion: null,
                        engineVersion: null
                    } as Project)
                }
            }
            batchedUpdate(() => {
                setProjectsList(newProjectsList);
            });
        })();
    }, [accessToken]);

    let itemsTable: any = (
        <tr>
            <td colSpan={columns.length}>
                You have no project yet. Go ahead and create one using the form above.
            </td>
        </tr>
    );

    if (projectsList && projectsList.length > 0) {
        const setProjectOpening = (projectItemIndex: number): void => {
            setProjectsList((currProjectList) => {
                const newProjectList = [...currProjectList];
                newProjectList[projectItemIndex]!.state.type =
                    ProjectState.OpenInProgress;
                return newProjectList;
            });
        };
        const setProjectOpen = (projectItemIndex: number): void => {
            setProjectsList((currProjectList) => {
                const newProjectList = [...currProjectList];
                newProjectList[projectItemIndex]!.state.type = ProjectState.Opened;
                return newProjectList;
            });
        };
        const setProjectClosed = (projectItemIndex: number): void => {
            setProjectsList((currProjectList) => {
                const newProjectList = [...currProjectList];
                newProjectList[projectItemIndex]!.state.type = ProjectState.Closed;
                return newProjectList;
            });
        };

        itemsTable = projectsList.map((item, index) => {
            return (
                <tr
                    key={item.projectId}
                    className="transition duration-300 ease-in-out hover:bg-gray-100"
                >
                    <td className="px-6 text-left flex items-center align-middle whitespace-nowrap border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        <ProjectActionButton
                            project={item}
                            onOpen={() => {
                                setProjectOpen(index);
                            }}
                            onOpenStart={() => {
                                setProjectOpening(index);
                            }}
                            onClose={() => {
                                setProjectClosed(index);
                            }}
                        />
                        {item.name}
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        2022-08-10, 13:30
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        aa
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        aa
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        aa
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                        aa
                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">

                    </td>
                    <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">

                    </td>
                </tr>
            );
        })
    }

    return (
        <>
            <Templates onChange={handleCreateProject}/>
            <table className="items-center w-full bg-transparent border-collapse">
                <thead>
                <tr>{tableHeaders}</tr>
                </thead>
                <tbody>{itemsTable}</tbody>
            </table>
            <button onClick={signOut}>Log out</button>
        </>
    );
}

export default withRouter(dashboardContainer)
