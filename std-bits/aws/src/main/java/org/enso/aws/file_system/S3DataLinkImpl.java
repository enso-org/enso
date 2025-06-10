package org.enso.aws.file_system;

import org.enso.base.enso_cloud.DataLinkSPI;

<<<<<<< HEAD:std-bits/aws/src/main/java/org/enso/aws/file_system/S3DataLinkSPI.java
public final class S3DataLinkSPI extends DataLinkSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DataLinkSPI.class)
public final class S3DataLinkImpl extends DataLinkSPI {
>>>>>>> origin/develop:std-bits/aws/src/main/java/org/enso/aws/file_system/S3DataLinkImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.AWS.S3.S3_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "S3_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "S3";
  }
}
