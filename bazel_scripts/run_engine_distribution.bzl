# Rule that runs Enso engine distribution

def _run_enso_impl(ctx):
    distribution = ctx.attr.distribution[DefaultInfo].files
    binary = ctx.actions.declare_file(ctx.label.name + ".sh")
    dist_dir = distribution.to_list()[0].path

    ctx.actions.write(
        output = binary,
        content = """#!/bin/bash
        binary_path=built-distribution/enso-engine-*/enso-*/bin/enso
        if [ ! -f $binary_path ]; then
            echo "Error: Could not find enso binary in {dist}"
            exit 1
        fi
        exec $PWD/$binary_path {args}
        """.format(
            dist = dist_dir,
            args = " ".join(ctx.attr.run_args),
        ),
        is_executable = True,
    )

    return [DefaultInfo(
        executable = binary,
        runfiles = ctx.runfiles(files = distribution.to_list()),
    )]


run_enso = rule(
    implementation = _run_enso_impl,
    attrs = {
        "distribution": attr.label(
            mandatory = True,
            allow_files = True,
        ),
        "run_args": attr.string_list(
            doc = "Arguments to the Enso binary",
            default = ["--version"]
        )
    },
    executable = True,
)