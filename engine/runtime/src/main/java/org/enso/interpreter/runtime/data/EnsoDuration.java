package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "date", name = "Duration", stdlibName = "Standard.Base.Data.Time.Duration")
public class EnsoDuration implements TruffleObject {
    private final Duration duration;

    public EnsoDuration(Duration duration) {
        this.duration = duration;
    }

    public Duration getJavaDuration() {
        return duration;
    }

    @ExportMessage
    boolean hasType() {
        return true;
    }

    @ExportMessage
    Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
        return Context.get(thisLib).getBuiltins().duration();
    }

    @Builtin.Method(
        name = "new_builtin",
        description = "Constructs a new Duration from hours, minutes, seconds, milliseconds and nanoseconds")
    @TruffleBoundary
    public static EnsoDuration create(long hours, long minutes, long seconds, long milliseconds, long nanoseconds) {
        var duration = Duration.ofHours(hours)
                .plusMinutes(minutes)
                .plusSeconds(seconds)
                .plusMillis(milliseconds)
                .plusNanos(nanoseconds);
        return new EnsoDuration(duration);
    }

    @Builtin.Method(
        name = "between_builtin",
        description = "Construct a new Duration that is between the given start date inclusive, and end date exclusive")
    @Builtin.Specialize
    public static EnsoDuration between(Object startInclusive, Object endExclusive, InteropLibrary interop) {
        ZonedDateTime startDateTime = convertToZonedDateTime(startInclusive, interop);
        ZonedDateTime endDateTime = convertToZonedDateTime(endExclusive, interop);
        return new EnsoDuration(
            Duration.between(startDateTime.toInstant(), endDateTime.toInstant())
        );
    }

    private static ZonedDateTime convertToZonedDateTime(Object dateObject, InteropLibrary interop) {
        assert interop.isDate(dateObject);
        try {
            LocalDate date = interop.asDate(dateObject);
            LocalTime time = interop.isTime(dateObject) ? interop.asTime(dateObject) : LocalTime.MIN;
            ZoneId zone = interop.isTimeZone(dateObject) ? interop.asTimeZone(dateObject) : ZoneId.systemDefault();
            return ZonedDateTime.of(date, time, zone);
        } catch (UnsupportedMessageException e) {
            // TODO: Panic
            throw new RuntimeException(e);
        }
    }

    @Builtin.Method(description = "Gets the hours")
    @CompilerDirectives.TruffleBoundary
    public long hours() {
        return duration.toHours();
    }

    @Builtin.Method(description = "Gets the minutes")
    @CompilerDirectives.TruffleBoundary
    public long minutes() {
        return duration.toMinutes();
    }

    @Builtin.Method(description = "Gets the seconds")
    @CompilerDirectives.TruffleBoundary
    public long seconds() {
        return duration.toSeconds();
    }

    @Builtin.Method(description = "Gets the milliseconds")
    @CompilerDirectives.TruffleBoundary
    public long milliseconds() {
        return duration.toMillis();
    }

    @Builtin.Method(description = "Gets the nanoseconds")
    @CompilerDirectives.TruffleBoundary
    public long nanoseconds() {
        return duration.toNanos();
    }

    @Builtin.Method(name = "plus_builtin", description = "Adds another Duration")
    @Builtin.ReturningGuestObject
    public EnsoDuration plus(EnsoDuration otherDuration) {
        return new EnsoDuration(duration.plus(otherDuration.duration));
    }

    @Builtin.Method(name = "minus_builtin", description = "Subtracts another Duration")
    @Builtin.ReturningGuestObject
    public EnsoDuration minus(EnsoDuration otherDuration) {
        return new EnsoDuration(duration.minus(otherDuration.duration));
    }

    @Builtin.Method(name = "compare_to_builtin", description = "Compares to other duration")
    public long compareTo(EnsoDuration otherDuration) {
        return duration.compareTo(otherDuration.duration);
    }

    @Builtin.Method(name = "equals_builtin")
    public boolean equalsDuration(EnsoDuration otherDuration) {
        return duration.equals(otherDuration.duration);
    }

    @ExportMessage
    public boolean isDuration() {
        return true;
    }

    @ExportMessage
    public Duration asDuration() {
        return duration;
    }

    @ExportMessage
    @TruffleBoundary
    public String toDisplayString(boolean allowSideEffects) {
        return duration.toString();
    }
}
