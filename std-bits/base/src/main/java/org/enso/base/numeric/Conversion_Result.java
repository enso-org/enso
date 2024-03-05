package org.enso.base.numeric;

import java.math.BigDecimal;

// Wraps the result of a conversion to BigDecimal with a precision loss flag.
public record Conversion_Result(BigDecimal newValue, boolean hasPrecisionLoss) {}
