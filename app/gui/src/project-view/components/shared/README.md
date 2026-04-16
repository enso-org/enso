# Shared components

This directory contains components used both within custom elements, and as
normal components.

All .vue files in this directory are compiled in custom element mode. This
affects the way styles are handled.

## Writing shared components

A shared component must depend only on other shared components, as any
non-shared component used would be missing styles. An exception is utility
components with no visual representation, such as the Vue host; these can safely
be used.

## Using shared components in custom elements

These components can be used freely anywhere they will be in the subtree of a
custom element (e.g. the visualization host element).

## Using shared components outside custom elements

To use a shared component outside a custom element, a wrapper component should
be created. The wrapper's function is to re-apply all styles from the shared
component _and any of its dependencies_. The wrapper will need to forward
properties/exposed APIs to the shared component. We don't (currently) have any
way to do this generically. It includes:

- Styles should be defined in an external file so that they can be shared
  between the wrapper and the shared component.
- The shared component will need to export the type of its properties for the
  wrapper to use to define its own properties. This is needed because
  `defineProps` is compiled before any type logic is applied, so it can't get
  property type info from the shared component. See
  `components/AgGridTableView.vue` for an example.
