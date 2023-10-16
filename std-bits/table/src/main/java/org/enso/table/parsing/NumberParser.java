package org.enso.table.parsing;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;

/** A parser for numbers.
 *
 * This parser will attempt to work out what the decimal point and thousand
 * separators used in the input. It will try various ways of formatting a number
 * and can be set to allow for scientific notation, currency symbols.
 *
 * If parsing a column it will select the format that parses the longest set
 * without an issue from the top and then apply this format to all the rows.
 *
 * The separators will be tried in British, German, French and Swiss order.
 * - Thousand separator must be followed by groups of 3 numbers.
 * - Scientific notation is only allowed on decimals and must be on a value
 *   between -10 and 10. The notation is an `E` followed by an integer.
 *
 * The following formats are supported:
 * - Sign (+/-) followed by Number (e.g. +1,234.56)
 * - Using brackets to indicate a negative number (e.g. (1,234.56))
 * - Currency symbols (if enabled) can be placed before or after the sign and
 *   number.
 * - If using brackets, the currency symbol must be placed after the opening
 *   bracket.
 * */
public class NumberParser extends IncrementalDatatypeParser {
    private final static String SIGN = "(?<sign>[-+])?";
    private final static String BRACKETS = "(?<sign>\\((?=.*\\)\\s*$))?\\s*";
    private final static String BRACKET_CLOSE = "\\)?";
    private final static String CCY = "(?<ccy>[^0-9(),. '+-]+)";
    private final static String EXP = "(?<exp>[eE][+-]?\\d+)?";
    private final static String SPACE = "\\s*";

    private record Separators(String thousand, String decimal) {}

    private final Separators[] SEPARATORS;

    private final static Map<String, Pattern> PATTERNS = new HashMap<>();
    private final IntegerType integerTargetType;

    private static void validateSeparator(String name, String value) {
        if (value == null) return;

        if (value.length() != 1) {
            throw new IllegalArgumentException(name + " must be a single character, but it was '" + value + "'.");
        }

        // If we allowed separators to be a digit, super crazy stuff could happen - e.g. technically 10000 could be interpreted as 1000 by interpreting the first 0 as a thousand separator. Let's not do that.
        if (Character.isDigit(value.charAt(0))) {
            throw new IllegalArgumentException(name + " cannot be a digit, but it was '" + value + "'.");
        }
    }

    /**
     * Builds a list of possible separator pairs.
     * <p>
     * If one of the parameters is null, it is meant to be inferred (multiple separator pairs will be provided for
     * it), if
     * it is set to a concrete value, it will be fixed.
     */
    private static Separators[] buildSeparators(boolean allowDecimal, String decimalPoint, String thousandSeparator) {
        validateSeparator("Decimal point", decimalPoint);
        validateSeparator("Thousand separator", thousandSeparator);
        if (decimalPoint != null && decimalPoint.equals(thousandSeparator)) {
            throw new IllegalArgumentException("Decimal point and thousand separator cannot be the same, but they were both '" + decimalPoint + "'.");
        }

        boolean fullAutomaticMode = allowDecimal && decimalPoint == null && thousandSeparator == null;
        if (fullAutomaticMode) {
            return new Separators[] {
                new Separators(",", "."),
                new Separators(".", ","),
                new Separators(" ", ","),
                new Separators("'", ","),
            };
        }

        List<String> thousandSeparators;
        if (thousandSeparator == null) {
            List<String> autoThousandSeparators = List.of(",", ".", "'", " ");
            thousandSeparators = autoThousandSeparators.stream().filter(sep -> !sep.equals(decimalPoint)).toList();
        } else {
            thousandSeparators = List.of(thousandSeparator);
        }

        List<String> decimalPoints;
        if (decimalPoint == null) {
            if (allowDecimal) {
                List<String> autoDecimalPoints = List.of(",", ".");
                assert thousandSeparator != null;
                decimalPoints = autoDecimalPoints.stream().filter(sep -> !sep.equals(thousandSeparator)).toList();
            } else {
                // List.of(null) is not permitted...
                decimalPoints = new ArrayList<>();
                decimalPoints.add(null);
            }
        } else {
            decimalPoints = List.of(decimalPoint);
        }

        return thousandSeparators.stream()
                .flatMap(thousand -> decimalPoints.stream().map(decimal -> new Separators(thousand, decimal)))
                .toArray(Separators[]::new);

    }

    /** The number of patterns that are allowed for non-currency numbers. */
    private static final int ALLOWED_NON_CCY_PATTERNS = 2;

    /** The number of patterns that are allowed for currency numbers. */
    private static final int ALLOWED_CCY_PATTERNS = 6;

    private static Pattern buildPattern(boolean allowDecimal, boolean allowCurrency, boolean allowScientific, boolean trimValues, int patternIndex, Separators separators) {
        if (allowScientific && !allowDecimal) {
            throw new IllegalArgumentException("Scientific notation requires decimal numbers.");
        }

        if (patternIndex >= (allowCurrency ? ALLOWED_CCY_PATTERNS : ALLOWED_NON_CCY_PATTERNS)) {
            return null;
        }

        String INTEGER = "(?<integer>(\\d*)" + (separators.thousand == null ? "" : "|(\\d{1,3}([" + separators.thousand + "]\\d{3})*)") + ")";

        String decimalPoint = allowDecimal ? Objects.requireNonNull(separators.decimal) : null;
        var NUMBER = INTEGER + (allowDecimal ? "(?<decimal>[" + decimalPoint + "]\\d*)?" : "") + (allowScientific ? EXP : "");

        var pattern = switch (patternIndex) {
            case 0 -> SIGN + NUMBER;
            case 1 -> BRACKETS + NUMBER + BRACKET_CLOSE;
            case 2 -> SIGN + CCY + SPACE + NUMBER;
            case 3 -> CCY + SPACE + SIGN + NUMBER;
            case 4 -> SIGN + NUMBER + CCY;
            case 5 -> BRACKETS + CCY + SPACE + NUMBER + BRACKET_CLOSE;
            default -> throw new IllegalArgumentException("Invalid pattern index: " + patternIndex);
        };

        if (trimValues) {
            pattern = SPACE + pattern + SPACE;
        }

        return PATTERNS.computeIfAbsent("^" + pattern + "$", Pattern::compile);
    }

    private final boolean allowDecimal;
    private final boolean allowCurrency;
    private final boolean allowLeadingZeros;
    private final boolean allowScientific;
    private final boolean trimValues;

  /**
   * Creates a new integer instance of this parser.
   *
   * @param integerTargetType the target type describing how large integer values can be accepted
   * @param allowCurrency whether to allow currency symbols
   * @param allowLeadingZeros whether to allow leading zeros
   * @param trimValues whether to trim the input values
   * @param decimalPoint the decimal point set for the current format, or null if not specified; this parser does
   *     not use decimal point (since it is for integers) but it ensure that if a decimal point is chosen, the inferred
   *     thousand separator will not clash with that specific decimal point
   * @param thousandSeparator the thousand separator to use (if null then will be inferred)
   */
  public static NumberParser createIntegerParser(IntegerType integerTargetType, boolean allowCurrency,
                                                 boolean allowLeadingZeros, boolean trimValues,
                                                 String decimalPoint, String thousandSeparator) {
        return new NumberParser(false, integerTargetType, allowCurrency, allowLeadingZeros, trimValues, false, decimalPoint, thousandSeparator);
    }

    /**
     * Creates a new decimal instance of this parser.
     *
     * @param allowCurrency whether to allow currency symbols
     * @param allowLeadingZeros whether to allow leading zeros
     * @param trimValues whether to trim the input values
     * @param allowScientific whether to allow scientific notation
     * @param decimalPoint the decimal separator to use (if null, then will be inferred)
     * @param thousandSeparator the thousand separator to use (if null, then will be inferred)
     */
    public static NumberParser createDecimalParser(boolean allowCurrency, boolean allowLeadingZeros, boolean trimValues, boolean allowScientific, String decimalPoint, String thousandSeparator) {
        return new NumberParser(true, null, allowCurrency, allowLeadingZeros, trimValues, allowScientific, decimalPoint, thousandSeparator);
    }

    private NumberParser(boolean allowDecimal, IntegerType integerTargetType, boolean allowCurrency, boolean allowLeadingZeros, boolean trimValues, boolean allowScientific, String decimalPoint, String thousandSeparator) {
        this.allowDecimal = allowDecimal;
        this.integerTargetType = integerTargetType;
        this.allowCurrency = allowCurrency;
        this.allowLeadingZeros = allowLeadingZeros;
        this.trimValues = trimValues;
        this.allowScientific = allowScientific;
        SEPARATORS = buildSeparators(allowDecimal, decimalPoint, thousandSeparator);
    }

    /**
     * Creates a Pattern for the given index.
     * The index will be decoded into a specific set of separators (unless fixed
     * separators are used) and then paired with one of the valid patterns for
     * the given parser.
     */
    private Pattern patternForIndex(int index) {
        int allowedSet = (allowCurrency ? ALLOWED_CCY_PATTERNS : ALLOWED_NON_CCY_PATTERNS);
        int separatorsIndex = index / allowedSet;
        int patternIndex = index % allowedSet;

        if (separatorsIndex >= SEPARATORS.length) {
            return null;
        }

        return buildPattern(allowDecimal, allowCurrency, allowScientific, trimValues, patternIndex, SEPARATORS[separatorsIndex]);
    }

    @Override
    public Object parseSingleValue(String text, ParseProblemAggregator problemAggregator) {
        int index = 0;
        var pattern = patternForIndex(index);
        while (pattern != null) {
            var value = innerParseSingleValue(text, pattern);
            if (value != null) {
                return value;
            }

            index++;
            pattern = patternForIndex(index);
        }

        problemAggregator.reportInvalidFormat(text);
        return null;
    }

    @Override
    public Storage<?> parseColumn(Storage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
        int index = 0;
        var pattern = patternForIndex(index);

        int bestIndex = 0;
        int bestCount = -1;
        while (pattern != null) {
            ProblemAggregator inner = problemAggregator.createSimpleChild();
            Builder builder = makeBuilderWithCapacity(sourceStorage.size(), inner);
            int failedAt = parseColumnWithPattern(pattern, sourceStorage, builder, null);
            if (failedAt == -1) {
                return builder.seal();
            }

            // If there was a failure, we abandon this branch - thus we discard any problems that might have been reported by the inner aggregator.
            inner.detachFromParent();

            if (failedAt > bestCount) {
                bestCount = failedAt;
                bestIndex = index;
            }

            index++;
            pattern = patternForIndex(index);
        }

        CommonParseProblemAggregator aggregator = problemAggregator.createContextAwareChild();
        Builder fallback = makeBuilderWithCapacity(sourceStorage.size(), aggregator);
        parseColumnWithPattern(patternForIndex(bestIndex), sourceStorage, fallback, aggregator);
        return fallback.seal();
    }

    private int parseColumnWithPattern(Pattern pattern, Storage<String> sourceStorage, Builder builder, ParseProblemAggregator aggregator) {
        Context context = Context.getCurrent();
        for (int i = 0; i < sourceStorage.size(); i++) {
            var text = sourceStorage.getItemBoxed(i);
            if (text == null) {
                builder.appendNulls(1);
            } else {
                var value = innerParseSingleValue(text, pattern);
                if (value != null) {
                    builder.appendNoGrow(value);
                } else {
                    if (aggregator == null) {
                        return i;
                    }

                    aggregator.reportInvalidFormat(text);
                    builder.appendNulls(1);
                }
            }

            context.safepoint();
        }
        return -1;
    }

    @Override
    protected Builder makeBuilderWithCapacity(int capacity, ProblemAggregator problemAggregator) {
        return allowDecimal
                ? NumericBuilder.createDoubleBuilder(capacity, problemAggregator)
                : NumericBuilder.createLongBuilder(capacity, integerTargetType, problemAggregator);
    }

    private Object innerParseSingleValue(String text, Pattern pattern) {
        if (allowDecimal) {
            var trimmed = trimValues ? text.trim() : text;
            if (trimmed.equals("NaN")) {
                return Double.NaN;
            }
            if (trimmed.equals("Infinity")) {
                return Double.POSITIVE_INFINITY;
            }
            if (trimmed.equals("-Infinity")) {
                return Double.NEGATIVE_INFINITY;
            }
        }

        var parsed = pattern.matcher(text);
        if (!parsed.matches()) {
            return null;
        }

        try {
            var sign = parsed.group("sign");
            long sign_value = sign != null && !sign.equals("+") ? -1 : 1;

            var integer = parsed.group("integer").replaceAll("\\D", "");

            if (!allowLeadingZeros && integer.length() > 1 && integer.charAt(0) == '0') {
                return null;
            }

            if (allowDecimal) {
                String decimal = parsed.group("decimal");
                String decimalPrepared = decimal == null ? "" : ("." + decimal.substring(1));

                if (integer.equals("") && decimalPrepared.equals("")) {
                    return null;
                }

                integer = integer.equals("") ? "0" : integer;

                String exp = allowScientific ? parsed.group("exp") : null;
                if (exp != null) {
                    if (integer.length() > 1) {
                        return null;
                    }
                    decimalPrepared = decimalPrepared + exp;
                }

                // If there is no decimal part, we parse as integer, as this will allow us more specialized handling.
                // For example, we can get the exact value instead of a rounded one for big values. We can then round
                // later, but first handle any warnings.
                if (decimalPrepared.equals("")) {
                    long integer_part = Long.parseLong(integer);

                    // Special handling for values like `-0` - if we treat them as integers, they will lose the `-` sign.
                    if (integer_part == 0 && sign_value < 0) {
                        return -0.0;
                    }

                    return sign_value * integer_part;
                }

                return sign_value * Double.parseDouble(integer + decimalPrepared);
            }

            if (integer.equals("")) {
                return null;
            }

            long integer_value = sign_value * Long.parseLong(integer);
            if (integerTargetType.fits(integer_value)) {
                return integer_value;
            } else {
                return null;
            }
        } catch (NumberFormatException e) {
            throw new IllegalStateException("Java parse failed to parse number: " + text, e);
        }
    }
}
