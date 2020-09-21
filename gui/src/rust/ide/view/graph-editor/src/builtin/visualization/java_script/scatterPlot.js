function loadScript(url) {
    var script = document.createElement("script");
    script.src = url;

    document.head.appendChild(script);
}

loadScript('https://d3js.org/d3.v4.min.js');

/**
 * A d3.js ScatterPlot visualization.
 *
 * source (CSV file): | x-axis | y-axis | domain |
 * Data format (json):
 * {
 *  source : String,
 *  colors : [{
 *      domain : String,
 *      color  : String
 *  }]
 * }
 */
class ScatterPlot extends Visualization {
    static inputType = "Any"

    onDataReceived(data) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild);
        }

        let width     = this.dom.getAttributeNS(null, "width");
        let height    = this.dom.getAttributeNS(null, "height");
        const divElem = this.createDivElem(width, height);

        let margin = {top: 20, right: 20, bottom: 20, left: 20};
        width      = width - margin.left - margin.right;
        height     = height - margin.top - margin.bottom;

        let svg = d3.select(divElem)
            .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        // FIXME : SVG eagerly gets all pointer events from top of it, even if
        //         node overlaps it. Should be debugged with (#797).

        let parsedData  = JSON.parse(data);
        let dataSource  = parsedData.source || "";
        let colorDomain = []
        let colorRange  = []
        if (parsedData.colors !== undefined) {
            parsedData.colors.forEach(d => {
                colorDomain.push(d.domain);
                colorRange.push("#" + d.color);
            });
        }

        d3.csv(dataSource, this.presentDataOnScatterplot(width, svg, height, colorDomain, colorRange));
    }

    presentDataOnScatterplot(width, svg, height, colorDomain, colorRange) {
        return function (_data) {
            var headerNames = d3.keys(_data[0]);

            var x = d3.scaleLinear()
                .domain([4, 8])
                .range([0, width]);
            var xAxis = svg.append("g")
                .attr("transform", "translate(0," + height + ")")
                .call(d3.axisBottom(x));

            var y = d3.scaleLinear()
                .domain([0, 9])
                .range([height, 0]);
            svg.append("g")
                .call(d3.axisLeft(y));

            var clip = svg.append("defs").append("svg:clipPath")
                .attr("id", "clip")
                .append("svg:rect")
                .attr("width", width)
                .attr("height", height)
                .attr("x", 0)
                .attr("y", 0);

            var color = d3.scaleOrdinal()
                .domain(colorDomain)
                .range(colorRange)

            var brush = d3.brushX()
                .extent([[0, 0], [width, height]])
                .on("end", updateChart)

            var scatter = svg.append('g')
                .attr("clip-path", "url(#clip)")

            scatter
                .selectAll("circle")
                .data(_data)
                .enter()
                .append("circle")
                .attr("cx", function (d) {
                    return x(d[headerNames[0]]);
                })
                .attr("cy", function (d) {
                    return y(d[headerNames[1]]);
                })
                .attr("r", 8)
                .style("fill", function (d) {
                    return color(d[headerNames[2]])
                })
                .style("opacity", 0.5)

            scatter
                .append("g")
                .attr("class", "brush")
                .call(brush);

            var idleTimeout

            function idled() {
                idleTimeout = null;
            }

            function updateChart() {
                let extent = d3.event.selection;

                if (!extent) {
                    if (!idleTimeout) return idleTimeout = setTimeout(idled, 350);
                    x.domain([4, 8])
                } else {
                    x.domain([x.invert(extent[0]), x.invert(extent[1])])
                    scatter.select(".brush").call(brush.move, null)
                }

                xAxis.transition().duration(1000).call(d3.axisBottom(x))
                scatter
                    .selectAll("circle")
                    .transition().duration(1000)
                    .attr("cx", function (d) {
                        return x(d[headerNames[0]]);
                    })
                    .attr("cy", function (d) {
                        return y(d[headerNames[1]]);
                    })

            }
        };
    }

    createDivElem(width, height) {
        const divElem = document.createElementNS(null, "div");
        divElem.setAttributeNS(null, "class", "vis-scatterplot");
        divElem.setAttributeNS(null, "viewBox", 0 + " " + 0 + " " + width + " " + height);
        divElem.setAttributeNS(null, "width", "100%");
        divElem.setAttributeNS(null, "height", "100%");
        divElem.setAttributeNS(null, "transform", "matrix(1 0 0 -1 0 0)");

        this.dom.appendChild(divElem);
        return divElem;
    }

    setSize(size) {
        this.dom.setAttributeNS(null, "width", size[0]);
        this.dom.setAttributeNS(null, "height", size[1]);
    }
}

return ScatterPlot;
