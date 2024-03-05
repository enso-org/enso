package org.enso.base.numeric;

import java.math.BigDecimal;

public record Conversion_Result(BigDecimal newValue, boolean hasPrecisionLoss) {}