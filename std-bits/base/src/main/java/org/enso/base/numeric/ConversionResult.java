package org.enso.base.numeric;

import java.math.BigDecimal;

// Wraps the result of a conversion to BigDecimal with a precision loss flag.
public record ConversionResult(BigDecimal newValue, boolean hasPrecisionLoss) {}
