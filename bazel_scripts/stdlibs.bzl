"""
Provides patterns for `*.enso` files within standard libraries.
"""

STDLIB_NAMES = [
    "AWS",
    "Base",
    "Database",
    "DuckDB",
    "Examples",
    "Generic_JDBC",
    "Geo",
    "Google",
    "Google_Api",
    "Image",
    "Microsoft",
    "Saas",
    "Searcher",
    "Snowflake",
    "Table",
    "Tableau",
    "Test",
    "Visualization",
]

def stdlib_source_patterns():
    """Returns the glob patterns for all the standard libraries.

    Returns:
      Array of glob patterns for standard library sources.
    """
    glob_patterns = []
    for lib_name in STDLIB_NAMES:
        glob_patterns += [
            "distribution/lib/Standard/{}/0.0.0-dev/src/**".format(lib_name),
            "distribution/lib/Standard/{}/0.0.0-dev/package.yaml".format(lib_name),
            "distribution/lib/Standard/{}/0.0.0-dev/data/**".format(lib_name),
            "distribution/lib/Standard/{}/0.0.0-dev/docs/**".format(lib_name),
            "distribution/lib/Standard/{}/0.0.0-dev/THIRD-PARTY/**".format(lib_name),
        ]
    return glob_patterns
