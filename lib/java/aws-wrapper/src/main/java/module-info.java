module org.enso.aws.wrapper {
    requires org.slf4j;
    requires org.reactivestreams; // Automatic module

    exports software.amazon.awssdk.auth.credentials;
    exports software.amazon.awssdk.core.exception;
    exports software.amazon.awssdk.awscore.exception;
    exports software.amazon.awssdk.http;
    exports software.amazon.awssdk.services.s3;
    exports software.amazon.awssdk.services.s3.model;
    exports software.amazon.awssdk.profiles;
    exports software.amazon.awssdk.regions;
    exports software.amazon.awssdk.regions.providers;

    uses software.amazon.awssdk.http.SdkHttpService;
    provides software.amazon.awssdk.http.SdkHttpService with
        software.amazon.awssdk.http.apache.ApacheSdkHttpService;
}
