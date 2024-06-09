package org.enso.downloader.http;

import java.io.IOException;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodySubscriber;
import java.net.http.HttpResponse.ResponseInfo;
import java.nio.ByteBuffer;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Flow;
import org.enso.cli.task.TaskProgressImplementation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.Some;

/** A {@link HttpResponse} body handler for {@link Path} that keeps track of the progress. */
class PathProgressBodyHandler implements HttpResponse.BodyHandler<Path> {
  private static final Logger LOGGER = LoggerFactory.getLogger(PathProgressBodyHandler.class);

  private final Path destination;
  private final TaskProgressImplementation<Path> progress;
  private Long total;

  private PathProgressBodyHandler(
      Path destination, TaskProgressImplementation<Path> progress, Long total) {
    this.destination = destination;
    this.progress = progress;
    this.total = total;
  }

  static PathProgressBodyHandler of(
      Path destination, TaskProgressImplementation<Path> progress, Long sizeHint) {
    return new PathProgressBodyHandler(destination, progress, sizeHint);
  }

  @Override
  public BodySubscriber<Path> apply(ResponseInfo responseInfo) {
    if (total == null) {
      var reportedLenOpt = responseInfo.headers().firstValueAsLong("Content-Length");
      if (reportedLenOpt.isPresent()) {
        total = reportedLenOpt.getAsLong();
        LOGGER.debug("Content-Length: {}", total);
      }
    }
    if (total != null) {
      progress.reportProgress(0, Some.apply(total));
    } else {
      progress.reportProgress(0, Option.empty());
    }
    LOGGER.debug("Initializing download into {}, estimated total is {}", destination, total);
    WritableByteChannel destChannel;
    try {
      destChannel =
          Files.newByteChannel(destination, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    return new ProgressSubscriber(destChannel);
  }

  private class ProgressSubscriber implements BodySubscriber<Path> {
    private Flow.Subscription subscription;
    private final WritableByteChannel destChannel;
    private final CompletableFuture<Path> result = new CompletableFuture<>();
    private long downloaded;

    ProgressSubscriber(WritableByteChannel destChannel) {
      this.destChannel = destChannel;
    }

    @Override
    public void onSubscribe(Flow.Subscription subscription) {
      this.subscription = subscription;
      this.subscription.request(1);
    }

    @Override
    public void onNext(List<ByteBuffer> items) {
      try {
        for (ByteBuffer item : items) {
          var len = item.remaining();
          downloaded += len;
          progress.reportProgress(downloaded, Option.apply(total));
          destChannel.write(item);
        }
        subscription.request(1);
      } catch (IOException e) {
        subscription.cancel();
        throw new RuntimeException(e);
      }
    }

    @Override
    public void onError(Throwable throwable) {
      LOGGER.warn(
          "Error while downloading into {}. Download progress {}/{}",
          destination,
          downloaded,
          total,
          throwable);
      try {
        destChannel.close();
      } catch (IOException e) {
        throwable.addSuppressed(e);
      }
      result.completeExceptionally(throwable);
    }

    @Override
    public void onComplete() {
      LOGGER.debug("Downloaded complete into {}", destination);
      try {
        destChannel.close();
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
      result.complete(destination);
    }

    @Override
    public CompletionStage<Path> getBody() {
      return result;
    }
  }
}
