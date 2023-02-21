/**
 * @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components.
 */

import * as React from 'react'
import { FC, useEffect, useState } from 'react'
import {unstable_batchedUpdates as batchedUpdate} from "react-dom";

import { useAuth, useFullUserSession } from '../authentication';

import withRouter from '../navigation'
import {createProject, Project, listProjects, ProjectState} from "../api";
import {Templates} from "./templates";
import {ProjectActionButton} from "./projectActionButton";



// ==========================
// === dashboardContainer ===
// ==========================

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

const dashboardContainer: FC = () => {
    const { signOut } = useAuth();
    const { accessToken } = useFullUserSession();
    const [projectsList, setProjectsList] = useState<Project[]>([]);

    const handleCreateProject = async (templateName: string | undefined) => {
        const newProjectName = "New_Project_1";
        const newProject = await createProject(accessToken, {
            projectName: newProjectName,
            projectTemplateName: templateName
        });

        setProjectsList([...projectsList, ...[newProject]]);
    };

    useEffect(() => {
        void (async (): Promise<void> => {
            const newProjectsList = await listProjects(accessToken);

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
            <button onClick={signOut}>Log out</button>
            <Templates onChange={handleCreateProject}/>
            <table className="items-center w-full bg-transparent border-collapse">
                <thead>
                <tr>{tableHeaders}</tr>
                </thead>
                <tbody>{itemsTable}</tbody>
            </table>
        </>
    );
}

export default withRouter(dashboardContainer)
