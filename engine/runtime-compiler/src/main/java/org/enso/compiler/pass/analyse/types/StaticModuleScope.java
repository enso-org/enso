package org.enso.compiler.pass.analyse.types;

/**
 * This is a sibling to the ModuleScope.
 *
 * <p>The ModuleScope is the runtime representation of a module, optimized for fast runtime
 * dispatch. The StaticModuleScope is an analogous structure, that can be used by static analysis
 * passes at compilation time.
 *
 * <p>It is also similar to the BindingsMap structure. In fact, it may be possible to merge the two
 * modules in the future, as StaticModuleScope is a more general variant. The BindingsMap only deals
 * with Types and their Constructors that are used during static resolution of some names. This
 * class also keeps track of all defined methods, to facilitate type checking. I'm keeping these
 * separate for now as it is easier to create a prototype that way. If later we find out they have
 * enough of similarity, we should merge them.
 */
public class StaticModuleScope {}
