package org.enso.base.math;

public class Math_Utils {
    /**
     * Minimum value for the `decimal_places` parameter to `roundDouble`.
     */
    private static final double ROUND_MIN_DECIMAL_PLACES = -15;

    /**
     * Maximum value for the `decimal_places` parameter to `roundDouble`.
     */
    private static final double ROUND_MAX_DECIMAL_PLACES = 15;

    /**
     * Minimum value for the `n` parameter to `roundDouble`.
     */
    private static final double ROUND_MIN_LONG = -99999999999999.0;

    /**
     * Minimum value for the `n` parameter to `roundDouble`.
     */
    private static final double ROUND_MAX_LONG = 99999999999999.0;

    /**
     * Round to a specified number of decimal places.
     *
     * By default, rounding uses "asymmetric round-half-up", also known as
     * "round towards positive infinity." If useBankers=True, then it uses
     * "round-half-even", also known as "banker's rounding".
     *
     * If the argument is `NaN` or `+/-Inf`, an `Arithmetic_Error` error is
     * thrown.
     *
     * Arguments:
     * @param decimalPlaces the number of decimal places to round to. Can be
     *   negative, which results in rounding to positive integer powers of 10.
     *   Must be between -15 and 15 (inclusive).
     * @param useBankers rounds mid-point to nearest even number.
     * @return the rounded number.
     * @throws ArithmeticException if the input is NaN/Inf.
     * @throws IllegalArgumentException if `n` is outside the allowed range.
     * @throws IllegalArgumentException if `decimalPlaces` is outside the
     *   allowed range.
     */
    public static double roundDouble(double n, int decimalPlaces, boolean useBankers) {
        if (decimalPlaces < ROUND_MIN_DECIMAL_PLACES || decimalPlaces > ROUND_MAX_DECIMAL_PLACES) {
            String msg = "round: decimalPlaces must be between " + ROUND_MIN_DECIMAL_PLACES + " and " + ROUND_MAX_DECIMAL_PLACES + " (inclusive), but was " + decimalPlaces;
            throw new IllegalArgumentException(msg);
        }
        if (Double.isNaN(n) || Double.isInfinite(n)) {
            String msg = "round cannot accept " + n;
            throw new ArithmeticException(msg);
        }
        if (n < ROUND_MIN_LONG || n > ROUND_MAX_LONG) {
            String msg = "Error: `round` can only accept values between " + ROUND_MIN_LONG + " and " + ROUND_MAX_LONG + " (inclusive), but was " + n;
            throw new IllegalArgumentException(msg);
        }

        // Algorithm taken from https://stackoverflow.com/a/7211688.
        double scale = Math.pow(10.0, decimalPlaces);
        double scaled = n * scale;
        double roundBase = Math.floor(scaled);
        double roundMidpoint = (roundBase + 0.5) / scale;
        boolean evenIsUp = n >= 0 ? (((long)scaled) % 2) != 0 : (((long)scaled) % 2) == 0;
        boolean halfGoesUp = useBankers ? evenIsUp : n >= 0;
        boolean doRoundUp = halfGoesUp ? n >= roundMidpoint : n > roundMidpoint;
        return doRoundUp ? ((roundBase + 1.0) / scale) : (roundBase / scale);
    }
}
