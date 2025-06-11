STDLIB_NAMES = [
    "AWS",
    "Base",
    "Database",
    "Examples",
    "Generic_JDBC",
    "Geo",
    "Google_Api",
    "Image",
    "Microsoft",
    "Searcher",
    "Snowflake",
    "Table",
    "Tableau",
    "Test",
    "Visualization",
]

def stdlib_source_patterns():
    """Returns the glob patterns for all the standard libraries."""
    glob_patterns = []
    for lib_name in STDLIB_NAMES:
        glob_patterns += [
            "distribution/lib/Standard/{}/0.0.0-dev/src/**".format(lib_name),
            "distribution/lib/Standard/{}/0.0.0-dev/package.yaml".format(lib_name),
        ]
    return glob_patterns
