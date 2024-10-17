"""

Explicitly import secrets from the environment into the workspace. The 'entries'
is a string -> string key/value mapping such that the key is the name of the
environment variable to import.  If the value is the special token '<REQUIRED>'
the build will fail if the variable is unset or empty.  Otherwise the value will
be used as the default.

    environment_secrets(
        name="env", 
        entries = {
            "MAVEN_REPO_USER": "<REQUIRED>",
            "MAVEN_REPO_PASSWORD": "<REQUIRED>",
            "DOCKER_PASSWORD": "<REQUIRED>",
            "DOCKER_URL": "index.docker.io",
        },
    )

In the example above, DOCKER_URL will use the value 'index.docker.io' if the
"DOCKER_URL" environment variable is not set.

Then in build scripts you can reference these by importing a custom bzl file.

    # Import a secret into the local BUILD.bazel environment
    load("@env//:defs.bzl","MAVEN_REPO_USER")

    # Use the value
    sample_rule(arg=MAVEN_REPO_USER)

"""

BUILD_BZL_CONTENTS='''
filegroup(
    name="secrets",
    srcs=["defs.bzl"],
    visibility=["//visibility:public"]
)
'''

UNSET_VALUE = "______UNSET______"
REQUIRED_VALUE = "<REQUIRED>"


def _environment_secrets_impl(repository_ctx):
    entries = repository_ctx.attr.entries

    lines = []
    lines.append("def getenv(env, fail_missing = True):")
    lines.append("  if False:")
    lines.append("    pass")

    for key, defaultValue in entries.items():
        lines.append("  elif env == \"{0}\":".format(key))

        value = repository_ctx.getenv(key, UNSET_VALUE)
        if value == UNSET_VALUE and defaultValue == REQUIRED_VALUE:
            lines.append("    return fail(\"Tried to use required environment value without providing its value: {}\".format(env)) if fail_missing else None")
            continue
        elif value == UNSET_VALUE and defaultValue:
            value = defaultValue

        # Escape quotes and backslashes
        value = value.replace("\\","\\\\").replace("\"","\\\"")
        lines.append("    return \"{}\"".format(value))

    lines.append("  else:")
    lines.append("    fail(\"Tried to use not declared environment value: {}\").format(env)")

    repository_ctx.file("defs.bzl", "\n".join(lines))
    repository_ctx.file("BUILD.bazel", BUILD_BZL_CONTENTS)

environment_secrets_rule = repository_rule(
    implementation = _environment_secrets_impl,
    attrs = {
        "entries": attr.string_dict(default = {}),
    },
)

def _secrets_impl(ctx):
    module = ctx.modules[0]
    environment_repo = module.tags.environment_repo
    environment_secrets_rule(
        name = environment_repo[0].name,
        entries = environment_repo[0].entries
    )

environment_repo = tag_class(attrs={
    "name": attr.string(default = "env"),
    "entries": attr.string_dict(default = {}),
})

secrets = module_extension(
    implementation = _secrets_impl,
    tag_classes = {"environment_repo": environment_repo},
)