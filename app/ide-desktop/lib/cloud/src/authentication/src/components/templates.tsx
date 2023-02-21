import * as React from "react";

const templates = [
    {title: "New empty project", templateName: undefined},
    {title: "Colordo COVID", templateName: "colorado_covid"},
    {title: "KMeans", templateName: "kmeans"},
    {title: "NASDAQ Returns", templateName: "nasdaq_returns"},
    {title: "Restaurants", templateName: "restaruants"},
    {title: "Github Stars", templateName: "stargazers"},

];

export const Templates: React.FC<any> = ({onChange}) => {
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
