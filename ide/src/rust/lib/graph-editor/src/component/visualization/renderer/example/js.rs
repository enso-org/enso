//! Example of the visualization JS wrapper API usage
// TODO remove once we have proper visualizations or replace with a nice d3 example.
// These implementations are neither efficient nor pretty, but get the idea across.

use crate::component::visualization::JsSourceClass;

/// Return an `JsSourceClass` that creates example Bubble Visualizations implemented in JS.
pub fn get_bubble_vis_class() -> JsSourceClass {
    let fn_constructor = r#"
        class BubbleVisualization {
            static inputTypes = ["[[Float,Float,Float]]"]

            onDataReceived(root, data) {
                const xmlns = "http://www.w3.org/2000/svg";
                while (root.firstChild) {
                    root.removeChild(root.lastChild);
                }

                const svgElem = document.createElementNS(xmlns, "svg");
                svgElem.setAttributeNS(null, "id"     , "vis-svg");
                svgElem.setAttributeNS(null, "viewBox", "0 0 " + 100 + " " + 100);
                svgElem.setAttributeNS(null, "width"  , 100);
                svgElem.setAttributeNS(null, "height" , 100);
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
                const width   = size[0];
                const height  = size[1];
                const svgElem = root.firstChild;
                svgElem.setAttributeNS(null, "viewBox", "0 0 " + width + " " + height);
                svgElem.setAttributeNS(null, "width"  , width);
                svgElem.setAttributeNS(null, "height" , height);
            }
        }

        return BubbleVisualization;
    "#;

    JsSourceClass::from_js_source_raw(fn_constructor).unwrap()
}
