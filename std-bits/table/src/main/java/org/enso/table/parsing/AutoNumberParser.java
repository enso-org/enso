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

public class AutoNumberParser extends DatatypeParser {
    private final static String SIGN = "(?<sign>[-+])?\\s*";
    private final static String BRACKETS = "(?<sign>\\((?=.*\\)\\s*$)\\s*";
    private final static String BRACKET_CLOSE = "\\)\\s*";
    private final static String CCY = "(?<ccy>\\D+)?\\s*";
    private final static String[] SEPARATORS = new String[] {",.", ".,", " ,", "',"};

    private final static Map<String, Pattern> PATTERNS = new HashMap<>();

    private static Pattern getPattern(boolean allowDecimal, int index) {
        int separatorsIndex = index / 6;
        if (separatorsIndex >= SEPARATORS.length) {
            return null;
        }

        var separators = SEPARATORS[separatorsIndex];

        var INTEGER = "(?<integer>(\\d+)|(\\d{1,3}([" + separators.charAt(0) + "]\\d{3})))";
        var NUMBER = INTEGER + (allowDecimal ? "(?<decimal>[" + separators.charAt(1) + "]\\d+)?" : "") + "\\s*";

        int patternIndex = index % 6;
        var pattern = switch (patternIndex) {
            case 0 -> SIGN + NUMBER;
            case 1 -> SIGN + CCY + NUMBER;
            case 2 -> CCY + SIGN + NUMBER;
            case 3 -> SIGN + NUMBER + CCY;
            case 4 -> BRACKETS + NUMBER + BRACKET_CLOSE;
            case 5 -> BRACKETS + CCY + NUMBER + BRACKET_CLOSE;
            default -> throw new IllegalArgumentException("Invalid pattern index: " + patternIndex);
        };

        return PATTERNS.computeIfAbsent(pattern, Pattern::compile);
    }

    private final boolean allowDecimal;

    public AutoNumberParser(boolean allowDecimal) {
        this.allowDecimal = allowDecimal;
    }

    @Override
    protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
        int index = 0;
        var pattern = getPattern(allowDecimal, index);
        while (pattern != null) {
            var value = innerParseSingleValue(text, pattern);
            if (value != null) {
                return value;
            }

            index++;
            pattern = getPattern(allowDecimal, index);
        }

        problemAggregator.reportInvalidFormat(text);
        return null;
    }

    @Override
    public WithProblems<Storage<?>> parseColumn(String columnName, Storage<String> sourceStorage) {
        int index = 0;
        var pattern = getPattern(allowDecimal, index);

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
            pattern = getPattern(allowDecimal, index);
        }

        Builder fallback = makeBuilderWithCapacity(sourceStorage.size());
        ProblemAggregator aggregator = new ProblemAggregatorImpl(columnName);
        parseColumnWithPattern(getPattern(allowDecimal, bestIndex), sourceStorage, fallback, aggregator);
        return new WithProblems<>(fallback.seal(), aggregator.getAggregatedProblems());
    }

    private int parseColumnWithPattern(Pattern pattern, Storage<String> sourceStorage, Builder builder, ProblemAggregator aggregator) {
        for (int i = 0; i < sourceStorage.size(); i++) {
            var text = sourceStorage.getItemBoxed(i);
            if (text == null) {
                sourceStorage.appendNulls(1);
            } else {
                var value = innerParseSingleValue(text, pattern);
                if (value != null) {
                    builder.appendNoGrow(value);
                } else {
                    if (aggregator == null) {
                        return i;
                    } else {
                        aggregator.reportInvalidFormat(text);
                    }
                }
            }
        }
        return -1;
    }

    protected Builder makeBuilderWithCapacity(int capacity) {
        return allowDecimal
                ? NumericBuilder.createDoubleBuilder(capacity)
                : NumericBuilder.createLongBuilder(capacity);
    }

    private Object innerParseSingleValue(String text, Pattern pattern) {
        var parsed = pattern.matcher(text);
        if (!parsed.matches()) {
            return null;
        }

        try {
            var sign = parsed.group("sign");
            var sign_value = sign.equals("-") || sign.equals("(") ? -1 : 1;

            var integer = parsed.group("integer").replaceAll("\\D", "");
            if (allowDecimal) {
                var decimal = parsed.group("decimal");
                var decimalPrepared = decimal == null ? "" : ("." + decimal.substring(1));
                return sign_value * Double.parseDouble(integer + decimalPrepared);
            }

            return sign_value * Long.parseLong(integer);
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
