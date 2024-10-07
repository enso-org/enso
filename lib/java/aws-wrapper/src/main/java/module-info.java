module org.enso.aws.wrapper {
    // Class software.amazon.awssdk.protocols.query.unmarshall.XmlDomParser acesses javax.xml.stream.XMLInputFactory
    requires java.xml;
    requires org.slf4j;
    requires org.reactivestreams; // Automatic module
    requires org.apache.httpcomponents.httpcore; // Automatic module
    requires org.apache.httpcomponents.httpclient; // Automatic module
    requires commons.logging; // Automatic module (with derived name)

    exports software.amazon.awssdk.auth.credentials;
    exports software.amazon.awssdk.core;
    exports software.amazon.awssdk.core.exception;
    exports software.amazon.awssdk.core.sync;
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
