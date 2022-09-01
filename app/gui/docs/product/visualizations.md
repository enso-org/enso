---
layout: developer-doc
title: Visualization Workflow
category: product
tags: [product]
---

# Visualization Workflow

## Purpose of visualizations

Visualizations have two main purposes:

- **Display results of nodes** Each node can be assigned with one or more
  visualization. After a node computes its new value, the visualization shows it
  in an understandable way to the user. Please note that a single node can be
  assigned with multiple visualizations at the same time. For example, a node
  might want to display a map of locations, and their list at the same time next
  to each other.

- **Provide interactive way to generate new data** In a widget mode (described
  in detail later), visualizations provide users with an interactive GUI to
  define data. For example, a map visualization can both display locations, as
  well as allowing the user to pick locations by clicking with a mouse.
  Similarly, the histogram can both display a list of numbers, and can be
  manually draw with the mouse producing such a list. Several numbers can be
  visualized as a table of sliders, which can also be used to interactively
  generate a table of numbers. Image visualizations can behave like an image
  editor, etc.

## Visualization Display Forms

Visualizations can be displayed in the following ways:

- **Attached to nodes** In this mode, visualizations display the most recent
  result of the node. They behave like an integrated part of the node. Whenever
  you move the node, the visualization moves as well. This mode can be toggled
  by tapping the spacebar.

- **Fullscreen** Visualization attached to node can grow (animate) to occupy
  full IDE visual space. This mode can be triggered on the recently selected
  node (in case many nodes are selected, the last selected node will be used) by
  either pressing keeping the spacebar pressed for longer than approx 0.5s, or
  by tapping it twice. In the former case, the visualization shrinks to each
  original form whenever we release space, in the later, whenever we press space
  again.

- **Detached** Visualizations attached to nodes can be detached, scaled, and
  placed freely across the visual canvas (we might introduce a special place
  where you can put such visualizations). This is useful when defining
  dashboards or reports. We also plan to provide a notebook-like experience
  where you can write text mixed with visualizations (including widgets for an
  interactive experience).

- **Widgets** In this mode visualizations behave like nodes but do not display
  expressions. They have one input and one output port. If the input port is
  connected, the visualization displays its value and passes its to the output
  port. In case it is not connected, the visualization becomes an interactive
  widget allowing the user to specify data. For example, a map visualization
  will allow the user to manually pick locations. After each change, the new
  locations will be sent to the output port. Under the hood, widgets are
  represented as nodes and their code lines are assigned with a dedicated
  "visualization" metadata. Visualizations generate expressions always in the
  form of `name = data`, where data is a hardcoded data produced from the
  visualization. For example, when user clicks the map to define locations, the
  data could be a string literal containing locations encoded in JSON.

### Choosing a Visualization Type.

When a new data is provided to a visualization, the visualization registry
searches for all visualizations that match it (see visualization registry to
learn more). For example, when a data of type `[Int]` (list of ints) is
produced, all visualizations which matches `[Int]`, like `[Int]`, `[a]`, or `a`
will be found. Each type can be associated with a default visualization. For
example, `[Int]` might define that its default visualization is a plot. If no
default visualization is defined, a JSON visualization is used. Each
visualization has a drop-down menu allowinh the user switching to another
visualization type.

### Active Visualizations

When visualizations are displayed on the stage, they are not active by default,
which means, they do not capture keyboard shortcuts. Visualization becomes
active when user clicks it. Visualizations are deactivated by clicking in the
background of the node editor. When a visualization is active, all other
elements should be slightly dimmed, or the visualization should get a selection
border (to be defined). Active visualizations capture all keyboard shortcuts,
but the space bar presses. Fullscreen visualizations are considered active by
default.

## HTML and Native Visualizations

There are two main types of visualizations - Html and Native. The later uses the
BaseGL shape API to draw on the screen. We prefer the later as it integrates
tightly with our framework and allows for much better performance. However,
there is already many visualizations in HTML/JS and we need to provide support
for them as well. HTML visualizations are required to be displayed in dedicated
div elements. This has several consequences. Firstly, the browser needs to
layout them, taking into account the current camera view, etc. It is costly.
Refreshing CSS3D styles of 100 visualizations can absolutely kill the
interactive performance. On the other hand, refreshing the position of 10k
Native visualizations is almost free. Secondly, they need to be handled by our
engine in such way that we can interact with them. For that purpose, the current
Scene implementation defines three layers - top HTML layer, middle WebGL layer,
and bottom HTML layer. The HTML visualizations are created and displayed on the
bottom layer by default. Whenever an HTML visualization gets active, it should
be moved to the top layer.

## Visualization Registry

Visualizations are user-defined. Enso ships with a set of predefined
visualizations, but they are in no way different than user-defined, they are
just defined for you. Visualizations can be defined either as HTML or native
visualization and can be defined in JS or WASM (or any language that compiles to
one of these). Visualizations are stored on disk on the server-side and are
provided to the GUI by the server. Users can upload their custom visualizations
as well. Each visualization is registered in the visualization map. The map maps
an Enso type to a set of visualizations defined for that type. The type might be
very generic, like `[a]` (which in Enso terms means list of any elements).

## Defining a Visualization

Currently only JavaScript visualizations can be defined. Support for native
visualizations is planned.

### Defining a JavaScript Visualization

JavaScript visualizations are defined by placing `*.js` files in the
`visualization` subfolder in the project's root directory. As IDE currently
allows only editing `Main.enso` file, users have to create `.js` file manually,
editing it outside IDE.

## Custom JavaScript Visualization Example

Every visualization must reside in the `visualization` folder of the user's
project. For instance:

```
└─ Project_Name
   ├─ src
   │  └─ Main.enso
   └─ visualization
      └─ bubble.js
```

Visualizations can be defined as a JavaScript function which returns a class of
a shape specified below. Consider the following sample definition:

```javascript
console.log("Hi, this definition is being registered now!");

return class BubbleVisualization extends Visualization {
  static inputType = "Any";

  onDataReceived(data) {
    const xmlns = "http://www.w3.org/2000/svg";
    while (this.dom.firstChild) {
      this.dom.removeChild(this.dom.lastChild);
    }
    const width = this.dom.getAttributeNS(null, "width");
    const height = this.dom.getAttributeNS(null, "height");
    const svgElem = document.createElementNS(xmlns, "svg");
    svgElem.setAttributeNS(null, "id", "vis-svg");
    svgElem.setAttributeNS(null, "viewBox", "0 0 " + width + " " + height);
    svgElem.setAttributeNS(null, "width", "100%");
    svgElem.setAttributeNS(null, "height", "100%");
    this.dom.appendChild(svgElem);
    data.forEach((data) => {
      const bubble = document.createElementNS(xmlns, "circle");
      bubble.setAttributeNS(null, "stroke", "black");
      bubble.setAttributeNS(null, "fill", "red");
      bubble.setAttributeNS(null, "r", data[2]);
      bubble.setAttributeNS(null, "cx", data[0]);
      bubble.setAttributeNS(null, "cy", data[1]);
      svgElem.appendChild(bubble);
    });
  }

  setSize(size) {
    this.dom.setAttributeNS(null, "width", size[0]);
    this.dom.setAttributeNS(null, "height", size[1]);
  }
};
```

In particular:

- ### [Required] Source code

  Visualization definition has to be a valid body of JavaScript function which
  returns a class definition. Instances of that class will be considered
  separate visualizations. You are allowed to use global variables / global
  state across visualizations of the same type, but you are highly advised not
  to do so.

- ### [Required] `Visualization` superclass

  The class returned by the definition function should extend the predefined
  `Visualization` class. Classes which do not extend it, will not be registered
  as visualizations. The superclass defines a default constructor and a set of
  utilities:

  - #### Method `setPreprocessor(module,method,...arguments)`
    Set an Enso method which will be evaluated on the server-side before sending
    data to visualization. Note that `arguments` is a vararg. If not called, a
    default unspecified method is used that will provide some JSON
    representation of the value. See [Lazy visualizations](#lazy-visualizations)
    section for details.
  - #### Field `dom`
    It is initialized in the constructor to the DOM symbol used to host the
    visualization content. Users are free to modify the DOM element, including
    adding other elements as its children.
  - #### Field `theme`
    The IDE's current color theme. Exposes the following methods.
    - ##### Method `getColorForType`
      Takes a qualified type name and returns the color that is used in the GUI
      to represent that type.
    - ##### Method `getForegroundColorForType`
      Takes a qualified type name and returns the color that should be used for
      foreground elements (e.g. text) that are shown on top of the background
      color returned by `getColorForType`.
    - ##### Method `get`
      Takes a style sheet path as string and returns the corresponding value
      from the theme. For example, `get("graph_editor.node.error.panic")`
      returns the orange color that is used to mark nodes in an error state.

- ### [Optional] Field `label`

  The static field `label` is an user-facing name used to identify the
  visualization. You are not allowed to define several visualizations of the
  same name in the same Enso library. In case the field is missing, the name
  will be inferred from the class name by splitting the camel-case name into
  chunks and converting them to lowercase string.

- ### [Optional] Field `inputType`

  The static field `inputType` is used to determine which Enso data types this
  visualization can be used for. Its value should be a valid Enso type, like
  `"String | Int"`. In case the field is an empty string or it is missing, it
  will default to `"Any"`, which is a type containing all other types. It is a
  rare case when you want to define a visualization which is able to work with
  just any data type, so you are highly advised to provide the type definition.

- ### [Optional] Field `inputFormat`

  The static field `inputFormat` is used to determine what format the data
  should be provided to the `onDataReceived` function. Currently, the only valid
  option is "json", but it will be possible to set it to "binary" in the future.
  In the later case, it is up to the visualization author to manage the binary
  stream received from the server.

- ### [Optional] Constructor

  The visualization will be instantiated by providing the constructor with a
  configuration object. The shape of the configuration object is not part of the
  public API and can change between releases of this library. You have to pass
  it unchanged to the superclass constructor.

- ### [Optional] Function `onDataReceived`

  The `onDataReceived(data)` method is called on every new data chunk received
  from the server. Note that the visualization will receive the "full data" if
  you are not using the `setPreprocessor` method.

- ### [Optional] Function `setSize`

  The `setSize(size)` method is called on every size change of the
  visualization. You should not draw outside of the provided area, however, if
  you do so, it will be clipped to the provided area automatically. The `size`
  parameter contains two fields `width` and `height` expressed in pixels.

## Sending Data to Visualizations

### Lazy Visualizations

Very important information is how visualization architecture works to make them
interactive and fast. Whenever new data is computed by the compiler and
visualization is attached to it, it is sent to GUI to be displayed. However,
sending huge chunks of data will kill the performance. When defining a
visualization user is capable of defining a chunk of Enso code (as a string).
This code is part of the visualization definition and is stored server-side.
Visualizations are allowed to change the code at runtime (in JavaScript
visualization you may use the `setPreprocessor` method). This code defines an
Enso function, which will be run by the compiler on data the visualization is
attached to. Only the results of this code will be sent to the GUI. In the case
of the JSON input format, the result of the call should be a valid JSON string.
The code will be evaluated in the context of the module where the preprocessor
method is defined - you may use any symbol defined or imported in that module.

For example, imagine you want to display a heatmap of 10 million points on a
map, and these points change rapidly. Sending such an amount of information via
WebSocket could be too much, and you (as the visualization author) might decide
that the visualization image should be generated on the server, and your
visualization is meant only to display the resulting image. In such a scenario,
you can define in your visualization an Enso function which will compute the the
image on the server!

### Binary and Text (JSON) Formats

Each visualization can choose whether it supports either binary or JSON input.
The input format defaults to JSON. The data from the server is always sent to
GUI in a binary channel, however, when JSON format is selected, it is first
converted to JSON representation on the server side. We can assume that all Enso
data types have defined conversion to JSON by default. If the visualization
input is defined as JSON input, the binary stream will be converted to JSON by
the GUI engine before passing to visualization. It is up to the visualization
author to handle the textual or binary form.

## Builtin Visualizations

IDE comes with a set of predefined visualizations, including charts.

### Dataframes Support

Some of the predefined visualizations have some special support for `Table` from
Enso Dataframes library.

#### Histogram

When using `Histogram` visualization on a `Table` value it will first look for a
column named `value`. If present, it will be used as a data source. Otherwise,
`Histogram` will use the first numerical column.

#### Scatter Plot

The `Scatter Plot` visualization has several properties for each point. If a
column of a matching name is present in the `Table` it will be used.

- `x` — position on horizontal axis. If not present, the index column will be
  used. If there is no index set, the row indices will be used. If this column
  has a missing value, the point will be omitted.
- `y` — position on vertical axis. If not present, first numerical column of
  unrecognized name will be used. If not present, first numerical column will be
  used. If this column has a missing value, the point will be omitted.
- `color` — color of the point. The default color is `black` and will be used if
  column is not present or for its missing values. `color` should be a `Text`
  column with elements being in a
  [CSS colors format](https://www.w3schools.com/cssref/css_colors_legal.asp):
  - Hexadecimal formats, like `#RGB`, `#RRGGBB` and `#RRGGBBAA`.
  - RGB function-like syntax, e.g. `rgb(255,0,128)` or `rgba(255,0,128,0.5)`.
  - HSL function-like syntax, e.g. `hsl(120, 100%, 50%)` or
    `hsla(120, 100%, 50%, 0.3)`.
  - name of one of
    [predefined colors](https://www.w3schools.com/colors/colors_names.asp), e.g.
    `red` or `SteelBlue`.
- `label` — text to be displayed next to the point.
- `shape` — shape of the point. Supported shapes are:

  - `cross`;
  - `diamond`;
  - `square`;
  - `star`;
  - `triangle`.

  The default shape is a circle.

- `size` — size of the point as a (possible floating point) number. Default size
  of the point is `1.0`.
