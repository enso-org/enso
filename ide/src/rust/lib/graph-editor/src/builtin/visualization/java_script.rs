//! Example of the visualization JS wrapper API usage
// TODO remove once we have proper visualizations or replace with a nice d3 example.
// These implementations are neither efficient nor pretty, but get the idea across.

use crate::data;
use crate::component::visualization;

/// Return a `JavaScript` Bubble visualization.
pub fn bubble_visualization() -> visualization::java_script::FallibleDefinition {
    let source = r#"
        class BubbleVisualization {
            static inputType = "Any"

            onDataReceived(root, data) {
                const xmlns = "http://www.w3.org/2000/svg";
                while (root.firstChild) {
                    root.removeChild(root.lastChild);
                }
                const width = root.getAttributeNS(null, "width");
                const height = root.getAttributeNS(null, "height");

                const svgElem = document.createElementNS(xmlns, "svg");
                svgElem.setAttributeNS(null, "id"     , "vis-svg");
                svgElem.setAttributeNS(null, "viewBox", 0 + " " + 0 + " " + width + " " + height);
                svgElem.setAttributeNS(null, "width"  , "100%");
                svgElem.setAttributeNS(null, "height" , "100%");
                // svgElem.setAttributeNS(null, "preserveAspectRatio" , "xMaxYMax meet");

                root.appendChild(svgElem);

                data.forEach(data => {
                    const bubble = document.createElementNS(xmlns,"circle");
                    bubble.setAttributeNS(null,"stroke", "black");
                    bubble.setAttributeNS(null,"fill"  , "red");
                    bubble.setAttributeNS(null,"r"     , data[2]);
                    bubble.setAttributeNS(null,"cx"    , data[0]);
                    bubble.setAttributeNS(null,"cy"    , data[1]);
                    svgElem.appendChild(bubble);
                });
            }

            setSize(root, size) {
                root.setAttributeNS(null, "width", size[0]);
                root.setAttributeNS(null, "height", size[1]);
            }
        }

        return BubbleVisualization;
    "#;

    visualization::java_script::Definition::new(data::builtin_library(),source)
}

/// Return an empty minimal `JavaScript` visualization. This should not be used except for testing.
pub fn empty_visualization() -> visualization::java_script::FallibleDefinition {
    let source = r#"
        class EmptyVisualization {}
        return EmptyVisualization;
    "#;

    visualization::java_script::Definition::new(data::builtin_library(),source)
}
