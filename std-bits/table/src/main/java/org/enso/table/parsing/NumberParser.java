package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.parsing.problems.ProblemAggregator;
import org.enso.table.parsing.problems.ProblemAggregatorImpl;
import org.enso.table.problems.WithProblems;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class NumberParser extends IncrementalDatatypeParser {
    private final static String SIGN = "(?<sign>[-+])?";
    private final static String BRACKETS = "(?<sign>\\((?=.*\\)\\s*$))?\\s*";
    private final static String BRACKET_CLOSE = "\\)?";
    private final static String CCY = "(?<ccy>[^0-9(),. '+-]+)?";
    private final static String EXP = "(?<exp>[eE][+-]?\\d+)?";
    private final static String SPACE = "\\s*";
    private final static String[] SEPARATORS = new String[] {",.", ".,", " ,", "',"};

    private final static Map<String, Pattern> PATTERNS = new HashMap<>();

    private static Pattern getPattern(boolean allowDecimal, boolean allowCurrency, boolean allowScientific, boolean trimValues, int index) {
        int allowedSet = allowCurrency ? 6 : 2;
        int separatorsIndex = index / allowedSet;
        int patternIndex = index % allowedSet;

        if (separatorsIndex >= SEPARATORS.length) {
            return null;
        }

        var separators = SEPARATORS[separatorsIndex];
        return getPattern(allowDecimal, allowCurrency, allowScientific, trimValues, patternIndex, separators);
    }

    private static Pattern getPattern(boolean allowDecimal, boolean allowCurrency, boolean allowScientific, boolean trimValues, int patternIndex, String separators) {
        if (allowScientific && !allowDecimal) {
            throw new IllegalArgumentException("Scientific notation requires decimal numbers.");
        }

        if (patternIndex >= (allowCurrency ? 6 : 2)) {
            return null;
        }

        var INTEGER = "(?<integer>(\\d*)" + (separators.length() == 1 ? "" : "|(\\d{1,3}([" + separators.charAt(0) + "]\\d{3})*)") + ")";

        var decimalPoint = (separators.length() == 1 ? separators : separators.charAt(1));
        var NUMBER = INTEGER + (allowDecimal ? "(?<decimal>[" + decimalPoint + "]\\d*)?" : "") + (allowScientific ? EXP : "");

        var pattern = switch (patternIndex) {
            case 0 -> SIGN + NUMBER;
            case 1 -> BRACKETS + NUMBER + BRACKET_CLOSE;
            case 2 -> SIGN + CCY + NUMBER;
            case 3 -> CCY + SIGN + NUMBER;
            case 4 -> SIGN + NUMBER + CCY;
            case 5 -> BRACKETS + CCY + NUMBER + BRACKET_CLOSE;
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
    private final String separators;

    /**
     * Creates a new integer instance of this parser.
     *
     * @param allowCurrency whether to allow currency symbols
     * @param allowLeadingZeros whether to allow leading zeros
     * @param trimValues whether to trim the input values
     * @param thousandSeparator the thousand separator to use
     */
    public NumberParser(boolean allowCurrency, boolean allowLeadingZeros, boolean trimValues, String thousandSeparator) {
        this.allowCurrency = allowCurrency;
        this.allowLeadingZeros = allowLeadingZeros;
        this.trimValues = trimValues;

        this.allowDecimal = false;
        this.allowScientific = false;

        this.separators = thousandSeparator == null ? null : (thousandSeparator + '_');
    }

    /**
     * Creates a new decimal instance of this parser.
     *
     * @param allowCurrency whether to allow currency symbols
     * @param allowLeadingZeros whether to allow leading zeros
     * @param trimValues whether to trim the input values
     * @param allowScientific whether to allow scientific notation
     */
    public NumberParser(boolean allowCurrency, boolean allowLeadingZeros, boolean trimValues, boolean allowScientific) {
        this.allowCurrency = allowCurrency;
        this.allowLeadingZeros = allowLeadingZeros;
        this.trimValues = trimValues;

        this.allowDecimal = true;
        this.allowScientific = allowScientific;

        this.separators = null;
    }

    /**
     * Creates a new decimal instance of this parser.
     *
     * @param allowCurrency whether to allow currency symbols
     * @param allowLeadingZeros whether to allow leading zeros
     * @param trimValues whether to trim the input values
     * @param allowScientific whether to allow scientific notation
     * @param thousandSeparator the thousand separator to use
     * @param decimalSeparator the decimal separator to use
     */
    public NumberParser(boolean allowCurrency, boolean allowLeadingZeros, boolean trimValues, boolean allowScientific, String thousandSeparator, String decimalSeparator) {
        if (decimalSeparator.length() != 1) {
            throw new IllegalArgumentException("Decimal separator must be a single character.");
        }

        this.allowCurrency = allowCurrency;
        this.allowLeadingZeros = allowLeadingZeros;
        this.trimValues = trimValues;

        this.allowDecimal = true;
        this.allowScientific = allowScientific;

        this.separators = thousandSeparator + decimalSeparator;
    }

    private Pattern patternForIndex(int index) {
        return separators == null
            ? getPattern(allowDecimal, allowCurrency, allowScientific, trimValues, index)
            : getPattern(allowDecimal, allowCurrency, allowScientific, trimValues, index, separators);
    }

    @Override
    protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
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
    public WithProblems<Storage<?>> parseColumn(String columnName, Storage<String> sourceStorage) {
        int index = 0;
        var pattern = patternForIndex(index);

        int bestIndex = 0;
        int bestCount = -1;
        while (pattern != null) {
            Builder builder = makeBuilderWithCapacity(sourceStorage.size());
            int failedAt = parseColumnWithPattern(pattern, sourceStorage, builder, null);
            if (failedAt == -1) {
                return new WithProblems<>(builder.seal(), Collections.emptyList());
            }

            if (failedAt > bestCount) {
                bestCount = failedAt;
                bestIndex = index;
            }

            index++;
            pattern = patternForIndex(index);
        }

        Builder fallback = makeBuilderWithCapacity(sourceStorage.size());
        ProblemAggregator aggregator = new ProblemAggregatorImpl(columnName);
        parseColumnWithPattern(patternForIndex(bestIndex), sourceStorage, fallback, aggregator);
        return new WithProblems<>(fallback.seal(), aggregator.getAggregatedProblems());
    }

    private int parseColumnWithPattern(Pattern pattern, Storage<String> sourceStorage, Builder builder, ProblemAggregator aggregator) {
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
        }
        return -1;
    }

    @Override
    protected Builder makeBuilderWithCapacity(int capacity) {
        return allowDecimal
                ? NumericBuilder.createDoubleBuilder(capacity)
                : NumericBuilder.createLongBuilder(capacity);
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
            var sign_value = sign != null && !sign.equals("+") ? -1 : 1;

            var integer = parsed.group("integer").replaceAll("\\D", "");

            if (!allowLeadingZeros && integer.length() > 1 && integer.charAt(0) == '0') {
                return null;
            }

            if (allowDecimal) {
                var decimal = parsed.group("decimal");
                var decimalPrepared = decimal == null ? "" : ("." + decimal.substring(1));

                if (integer.equals("") && decimalPrepared.equals("")) {
                    return null;
                }

                integer = integer.equals("") ? "0" : integer;

                if (allowScientific) {
                    var exp = parsed.group("exp");
                    if (exp != null) {
                        if (integer.length() > 1) {
                            return null;
                        }
                        decimalPrepared = decimalPrepared + exp;
                    }
                }

                return sign_value * Double.parseDouble(integer + decimalPrepared);
            }

            return integer.equals("") ? null : sign_value * Long.parseLong(integer);
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
