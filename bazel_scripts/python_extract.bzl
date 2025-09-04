"""
Rule for running a Python resource extraction tool.
See `/lib/java/python-extract` directory and `python-extract` task inside `build.sbt`.
"""

def _extract_python_resources_impl(ctx):
    output_dir = ctx.actions.declare_directory(ctx.attr.out_dir)
    ctx.actions.run(
        executable = ctx.executable._tool,
        arguments = [output_dir.path],
        outputs = [output_dir],
        inputs = ctx.files._tool,
    )
    return [DefaultInfo(files = depset([output_dir]))]

extract_python_resources = rule(
    implementation = _extract_python_resources_impl,
    attrs = {
        "_tool": attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("//lib/java/python-extract:python_extract"),
            doc = "Label for the target that builds python_extract JAR",
        ),
        "out_dir": attr.string(
            mandatory = True,
            doc = "Name of the output directory, in which Python resources will be extracted",
        ),
    },
)
