import com.oracle.truffle.api.provider.InternalResourceProvider;

module org.enso.python.resource.provider {
  requires org.graalvm.truffle;

  exports org.enso.python.resource.provider;

  provides InternalResourceProvider with
      org.enso.python.resource.provider.PythonResourceProvider;
}
