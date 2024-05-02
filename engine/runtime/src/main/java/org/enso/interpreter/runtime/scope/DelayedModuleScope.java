package org.enso.interpreter.runtime.scope;

import java.util.List;

public interface DelayedModuleScope {
    ModuleScope force();
    Object getPolyglotSymbol(String symbolName);

    DelayedModuleScope withTypes(List<String> typeNames);

}
