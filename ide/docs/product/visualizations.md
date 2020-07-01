# Visualization workflow

## Purpose of visualizations
Visualizations have two main purposes:

- **Display results of nodes**  
  Each node can be assigned with one or more visualization. After a node
  computes its new value, the visualization shows it in an understandable way to
  the user. Please note that a single node can be assigned with multiple
  visualizations at the same time. For example, a node might want to display a
  map of locations and their list at the same time next to each other.

- **Provide interactive way to generate new data**  
  In a widget mode (described in detail later), visualizations provide users
  with an interactive GUI to define data. For example, a map visualization can
  both display locations, as wekk as allow the user to pick locations by cliking
  with a mouse. Similarly, the histogram can both display a list of numbers, and
  can be manually draw with the mouse producing such a list. Several numbers can
  be visualized as a table of sliders, which can also be used to interactively
  generate a table of numbers. Image visualizations can behave like an image
  editor, etc.


## Visualization Display Forms
Visualizations can be displayed in the following ways:

- **Attached to nodes**  
  In this mode, visualizations display the most recent result of the node. They
  behave like an integrated part of the node. Whenever you move the node, the
  visualization moves as well. This mode can be toggled by tapping the spacebar.

- **Fullscreen**  
  Visualization attached to node can grow (animate) to ocupy full IDE visual
  space. This mode can be triggered on the recently selected node (in case many
  nodes are selected, the last selected node will be used) by either pressing
  keeping the spacebar pressed for longer than approx 0.5s, or by tapping it
  twice. In the former case, the visualization shrinks to each original form
  whenever we release space, in the later, whenever we press space again.

- **Detached**  
  Visualizations attached to nodes can be detached, scaled, and placed freely
  across the visual canvas (we might introduce a special place where you can put
  such visualizations). This is useful when defining dashboards or reports. We
  also plan to provide a notebook-like experience where you can write text mixed
  with visualizations (including widgets for an interactive experience). 

- **Widgets**  
  In this mode visualizations behave like nodes but do not display expressions.
  They have one input and one output port. If the input port is connected, the
  visualization displays its value and passes its to the output port. In case it
  is not connected, the visualization becomes an interactive widget allowing the
  user to specify data. For example, a map visualization will allow the user to
  manually pick locations. After each change, the new locations will be sent to
  the output port. Under the hood, widgets are represented as nodes and their
  code lines are assigned with a dedicated "visualization" metadata. 
  Visualizations generate expressions always in the form of `name = data`, where
  data is a hardcoded data produced from the visualization. For example, when
  user clicks the map to define locations, the data could be a string literal
  containing locations encoded in JSON.


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

### Defining a Visualization
This needs to be described in detail. For now, we can just assume that the user
is allowed to create visualization and register it in Enso.

### Sending Data to Visualizations

#### Lazy Visualizations
Very important information is how visualization architecture works to make them
interactive and fast. Whenever new data is computed by the compiler and
visualization is attached to it, it is sent to GUI to be displayed. However,
sending really big chunks of data will kill the performance. When defining a
visualization user is capable of defining a chunk of Luna code (as a string).
This code is part of the visualization definition and is stored server-side.
Visualizations are allowed to change the code at runtime. This code defines an
Enso function, which will be run by the compiler on data the visualization is
attached to. Only the results of this code will be sent to the GUI. For example,
imagine you want to display a heatmap of 10 million points on a map. And these
points change rapidly. Sending such amount of information via WebSocket could be
too much, and you (as the visualization author) might decide that the
visualization image should be generated on the server, and your visualization is
meant only to display the resulting image. In such a scenario, you can define in
your visualization an Enso function which will compute the image on the server!

#### Binary and Text (JSON) Formats
Each visualization can choose whether it supports either binary or JSON input.
The input format defaults to JSON. The data from the server is always sent to
GUI in a binary channel, however, when JSON format is selected, it is first
converted to JSON representation on the server side. We can assume that all Enso
data types have defined conversion to JSON by default. If the visualization
input is defined as JSON input, the binary stream will be converted to JSON by
the GUI engine before passing to visualization. It is up to the visualization
author to handle the textual or binary form. 
