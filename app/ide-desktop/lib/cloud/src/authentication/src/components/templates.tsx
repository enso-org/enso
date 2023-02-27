import * as React from "react";
import { FC } from "react";

const templates = [
    {title: "New empty project", templateName: undefined},
    {title: "Colordo COVID", templateName: "Colorado_COVID"},
    {title: "KMeans", templateName: "Kmeans"},
    {title: "NASDAQ Returns", templateName: "NASDAQ_Returns"},
    {title: "Restaurants", templateName: "Orders"},
    {title: "Github Stars", templateName: "Stargazers"},

];

type Props = {
    onChange: (templateName: string | undefined) => void;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Templates: FC<Props> = (props) => {
    const { onChange } = props;

    return (
        <div className="bg-white">
            <div className="mx-auto py-16 px-4 sm:py-24 sm:px-6 lg:px-8">
                <div
                    className="grid grid-cols-1 gap-y-10 gap-x-6 sm:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 xl:gap-x-8">
                    {templates.map((template, index) => (
                        <div
                            key={index}
                            className="h-40 rounded-lg border-4 border-dashed border-gray-200 flex"
                        >
                            <div className="m-auto text-center">
                                <button onClick={() => {
                                    // eslint-disable-next-line @typescript-eslint/no-unsafe-call
                                    onChange(template.templateName)
                                }}>
                                    <svg
                                        xmlns="http://www.w3.org/2000/svg"
                                        fill="none"
                                        viewBox="0 0 24 24"
                                        strokeWidth={0.5}
                                        stroke="currentColor"
                                        className="w-20 h-20"
                                    >
                                        <path
                                            strokeLinecap="round"
                                            strokeLinejoin="round"
                                            d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"
                                        />
                                    </svg>
                                </button>
                                <p>{template.title}</p>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
};
