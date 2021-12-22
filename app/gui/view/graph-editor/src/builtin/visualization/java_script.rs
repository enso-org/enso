//! Example of the visualization JS wrapper API usage
// TODO remove once we have proper visualizations or replace with a nice d3 example.
// These implementations are neither efficient nor pretty, but get the idea across.

use crate::component::visualization;



///////////////////////////////////////
// JavaScript builtin visualizations //
///////////////////////////////////////

/// Return a `JavaScript` Table visualization.
pub fn table_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/scrollable.js", include_str!("java_script/helpers/scrollable.js")),
        ("java_script/table.js", include_str!("java_script/table.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` SQL visualization.
pub fn sql_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/scrollable.js", include_str!("java_script/helpers/scrollable.js")),
        ("java_script/sql.js", include_str!("java_script/sql.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Scatter plot visualization.
pub fn scatter_plot_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/number.js", include_str!("java_script/helpers/number.js")),
        ("java_script/scatterPlot.js", include_str!("java_script/scatterPlot.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Histogram visualization.
pub fn histogram_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/number.js", include_str!("java_script/helpers/number.js")),
        ("java_script/histogram.js", include_str!("java_script/histogram.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Heatmap visualization.
pub fn heatmap_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/number.js", include_str!("java_script/helpers/number.js")),
        ("java_script/heatmap.js", include_str!("java_script/heatmap.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Map visualization.
pub fn geo_map_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/helpers/number.js", include_str!("java_script/helpers/number.js")),
        ("java_script/geoMap.js", include_str!("java_script/geoMap.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Bubble visualization. This should not be used as it is a demo
/// visualization.
pub fn bubble_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/bubbleVisualization.js", include_str!("java_script/bubbleVisualization.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return a `JavaScript` Image visualization.
pub fn image_base64_visualization() -> visualization::java_script::FallibleDefinition {
    let files = [
        ("java_script/helpers/loading.js", include_str!("java_script/helpers/loading.js")),
        ("java_script/imageBase64.js", include_str!("java_script/imageBase64.js")),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}

/// Return an empty minimal `JavaScript` visualization. This should not be used except for testing.
pub fn empty_visualization() -> visualization::java_script::FallibleDefinition {
    let source = r#"
        class EmptyVisualization extends Visualization {}
        return EmptyVisualization;
    "#;
    let files = [
        ("java_script/empty.js", source),
    ];
    let source = visualization::java_script::Sources::from_files(&files);

    visualization::java_script::Definition::new_builtin(source)
}
