package utils;

public class IncompatibleFormatException
  extends RuntimeException {
    public IncompatibleFormatException(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
    public IncompatibleFormatException(String errorMessage) {
        super(errorMessage);
    }
}
