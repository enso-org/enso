"""
Simple rule that runs Enso distribution via a shell script.
"""

def _run_enso_impl(ctx):
    distribution = ctx.attr.distribution[DefaultInfo].files
    binary = ctx.actions.declare_file(ctx.label.name + ".sh")
    dist_dir = distribution.to_list()[0].basename
    src_file = ctx.file.src.path
    java_toolchain_type = ctx.toolchains["@bazel_tools//tools/jdk:runtime_toolchain_type"]
    java_home = java_toolchain_type.java_runtime.java_home_runfiles_path

    ctx.actions.write(
        output = binary,
        content = """#!/bin/bash
        export JAVA_HOME="{java_home}"
        export PATH="$JAVA_HOME/bin:$PATH"
        binary_path={dist}/enso-engine-*/enso-*/bin/enso
        if [ ! -f $binary_path ]; then
            echo "Error: Could not find enso binary in {dist}"
            exit 1
        fi
        java -version
        exec $PWD/$binary_path {args} --run {src_file}
        """.format(
            dist = dist_dir,
            args = " ".join(ctx.attr.run_args),
            src_file = src_file,
            java_home = java_home,
        ),
        is_executable = True,
    )

    # This specifies that this rule depends on `distribution` and `src` attributes.
    all_runfiles = ctx.runfiles(
        files = distribution.to_list() + [ctx.file.src],
        transitive_files = java_toolchain_type.java_runtime.files,
    )

    return [DefaultInfo(
        executable = binary,
        runfiles = all_runfiles,
    )]

def _ensure_native_enso_impl(ctx):
    """ Runs enso executable with `--version` argument to ensure it was built with native image """
    distribution = ctx.attr.distribution[DefaultInfo].files
    binary = ctx.actions.declare_file(ctx.label.name + ".sh")
    dist_dir = distribution.to_list()[0].basename
    ctx.actions.write(
        output = binary,
        content = """#!/bin/bash
        binary_path={dist}/enso-engine-*/enso-*/bin/enso
        if [ ! -f $binary_path ]; then
            echo "Error: Could not find enso binary in {dist}"
            exit 1
        fi
        $PWD/$binary_path --version
        file --mime-encoding $PWD/$binary_path | grep -q "binary"
        if [ $? -ne 0 ]; then
            echo "Error: enso binary is not a native image build"
            exit 1
        fi
        """.format(
            dist = dist_dir,
        ),
        is_executable = True,
    )

    all_runfiles = ctx.runfiles(
        files = distribution.to_list(),
    )

    return [DefaultInfo(
        executable = binary,
        runfiles = all_runfiles,
    )]

ensure_native_enso = rule(
    implementation = _ensure_native_enso_impl,
    attrs = {
        "distribution": attr.label(
            mandatory = True,
            allow_files = True,
        ),
    },
    executable = True,
)

run_enso = rule(
    implementation = _run_enso_impl,
    toolchains = [
        "@bazel_tools//tools/jdk:runtime_toolchain_type",
    ],
    attrs = {
        "distribution": attr.label(
            mandatory = True,
            allow_files = True,
        ),
        "run_args": attr.string_list(
            doc = "Additional arguments to the Enso binary",
            default = [],
        ),
        "src": attr.label(
            allow_single_file = True,
            doc = "Source file to be --run",
        ),
    },
    executable = True,
)
