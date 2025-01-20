package org.enso.aws.file_system;

import org.enso.base.cache.APIRequestCache;

public class S3DataLinkCache extends APIRequestCache {
  public static final S3DataLinkCache INSTANCE = new S3DataLinkCache();
}
