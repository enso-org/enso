package org.enso.downloader.http;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodySubscriber;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Flow;
import org.enso.cli.task.TaskProgressImplementation;
import scala.Option;
import scala.Some;

/** A {@link HttpResponse} body handler for {@link Path} that keeps track of the progress. */
public class StringProgressBodyHandler implements HttpResponse.BodyHandler<String> {
  private final ByteArrayOutputStream destination = new ByteArrayOutputStream();
  private final TaskProgressImplementation<?> progress;
  private final Charset encoding;
  private Long total;

  private StringProgressBodyHandler(
      TaskProgressImplementation<?> progress, Charset encoding, Long total) {
    this.progress = progress;
    this.encoding = encoding;
    this.total = total;
  }

  public static StringProgressBodyHandler of(
      TaskProgressImplementation<?> progress, Long sizeHint, Charset encoding) {
    return new StringProgressBodyHandler(progress, encoding, sizeHint);
  }

  @Override
  public HttpResponse.BodySubscriber<String> apply(HttpResponse.ResponseInfo responseInfo) {
    if (total == null) {
      var reportedLenOpt = responseInfo.headers().firstValueAsLong("Content-Length");
      if (reportedLenOpt.isPresent()) {
        total = reportedLenOpt.getAsLong();
      }
    }
    if (total != null) {
      progress.reportProgress(0, Some.apply(total));
    } else {
      progress.reportProgress(0, Option.empty());
    }
    return new ProgressSubscriber();
  }

  private class ProgressSubscriber implements BodySubscriber<String> {
    private Flow.Subscription subscription;
    private final CompletableFuture<String> result = new CompletableFuture<>();

    @Override
    public void onSubscribe(Flow.Subscription subscription) {
      this.subscription = subscription;
      this.subscription.request(Long.MAX_VALUE);
    }

    @Override
    public void onNext(List<ByteBuffer> items) {
      for (ByteBuffer item : items) {
        var len = item.remaining();
        progress.reportProgress(len, total == null ? Option.empty() : Some.apply(total));
        byte[] bytes = new byte[len];
        item.get(bytes);
        destination.write(bytes, 0, bytes.length);
      }
      subscription.request(Long.MAX_VALUE);
    }

    @Override
    public void onError(Throwable throwable) {
      try {
        destination.close();
      } catch (IOException e) {
        throwable.addSuppressed(e);
      }
      result.completeExceptionally(throwable);
    }

    @Override
    public void onComplete() {
      try {
        destination.close();
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
      result.complete(destination.toString(encoding));
    }

    @Override
    public CompletionStage<String> getBody() {
      return result;
    }
  }
}
