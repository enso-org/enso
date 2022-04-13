# Mouse Handling

Ensogl handles mouse events (clicking, scrolling, moving the cursor) using two
different methods:

1. Most elements listen to FRP events from `MouseManager`, which internally
   attaches event listeners to [`window`][window] object. This method is
   described in the [Mouse Manager](#mouse-manager) section.
2. Some elements use their own event handlers attached to their DOM objects. Two
   examples of such elements are Welcome Screen and Visualizations. This is
   described in the [Pointer Events](#pointer-events) section.

Also see [How to Debug JS events][debug-events] and [How event propagation
works][bubbling-and-capturing].

## Mouse Manager

[Mouse Manager][mouse-manager] registers event listeners for the following mouse
event types:

- [mousedown](https://w3c.github.io/uievents/#event-type-mousedown)
- [mousemove](https://w3c.github.io/uievents/#event-type-mousemove)
- [mouseup](https://w3c.github.io/uievents/#event-type-mouseup)
- [mouseleave](https://w3c.github.io/uievents/#event-type-mouseleave)
- [wheel](https://w3c.github.io/uievents/#event-type-wheel)

These event listeners are attached to [`window`][window] object. Every DOM
object can setup specific listeners for these event types using
`application.display.default_scene.mouse.frp` FRP network.

This method is flexible and allows consistent mouse event handling across all
Enso GUI parts. However, it also requires registering `Shapes` for every
clickable object and setting up an FRP network.

## Pointer Events

If an HTML DOM element has [`pointer-events`][pointer-events] CSS property set
to `auto` it will become a target of mouse events. Keep in mind that elements
with `pointer-events: none` are never a target of such events, and this property
may be inherited from parent elements. Also, mouse events may target descendant
elements if those descendants have `pointer-events` set to some other value. In
these circumstances, mouse events will trigger event listeners on this parent
element as appropriate on their way to/from the descendant during the event
[capture/bubble phases][bubbling-and-capturing].

Ensogl uses a concept of [`Scene DOM Layers`][scene-dom-layers] to simulate
depth-sorting of DOM and canvas elements. Each of the layers is essentially an
HTML `div` element covering the whole screen at a specific `z-index`. All these
layers are created with `pointer-events: none`. This ensures that elements on
any layer will have an opportunity to capture mouse events. Otherwise, event
capturing will stop on the top-most layer with `pointer-events: auto`,
underlying layers won't get any event.

To use `click` and other event listeners attached to DOM elements, one should
explicitly set `pointer-events: auto` for the elements because otherwise
`pointer-events: none` will be inherited from the parent Scene DOM Layer. For
example, Enso's Visualizations do that using a specific [CSS
class][visualizations-css].

[visualizations-css]:
  https://github.com/enso-org/enso/blob/c4d22102cf0e36f056124aee429ed3ce7c062315/app/ide-desktop/lib/content/src/style.css#L114-L120
[scene-dom-layers]:
  https://github.com/enso-org/enso/blob/c4d22102cf0e36f056124aee429ed3ce7c062315/lib/rust/ensogl/core/src/display/scene.rs#L461-L479
[pointer-events]:
  https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events
[debug-events]:
  https://www.cookieshq.co.uk/posts/event-listeners-not-working-troublelshooting
[bubbling-and-capturing]: https://javascript.info/bubbling-and-capturing
[debug-events]:
  https://www.cookieshq.co.uk/posts/event-listeners-not-working-troublelshooting
[bubbling-and-capturing]: https://javascript.info/bubbling-and-capturing
