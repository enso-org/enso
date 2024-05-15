#!/bin/bash -x
set -e

if ! [ -f $JAVA_HOME/bin/jlink ]; then
  echo Specify JAVA_HOME environment variable
  exit 1
fi

JDK=$1
echo Generating small JDK to $JDK from $JAVA_HOME
rm -rf $JDK

JS_MODULES=org.graalvm.nativeimage,org.graalvm.nativeimage.builder,org.graalvm.nativeimage.base,org.graalvm.nativeimage.driver,org.graalvm.nativeimage.librarysupport,org.graalvm.nativeimage.objectfile,org.graalvm.nativeimage.pointsto,com.oracle.graal.graal_enterprise,com.oracle.svm.svm_enterprise,jdk.compiler.graal,jdk.httpserver,java.naming,java.net.http
DEBUG_MODULES=jdk.jdwp.agent
PYTHON_MODULES=jdk.security.auth,java.naming

$JAVA_HOME/bin/jlink --module-path $JAVA_HOME/lib/svm/bin/../../graalvm/svm-driver.jar:$JAVA_HOME/lib/svm/bin/../builder/native-image-base.jar:$JAVA_HOME/lib/svm/bin/../builder/objectfile.jar:$JAVA_HOME/lib/svm/bin/../builder/pointsto.jar:$JAVA_HOME/lib/svm/bin/../builder/svm-enterprise.jar:$JAVA_HOME/lib/svm/bin/../builder/svm.jar:$JAVA_HOME/lib/svm/bin/../library-support.jar --output $JDK  --add-modules $JS_MODULES,$DEBUG_MODULES,$PYTHON_MODULES
cp -r $JAVA_HOME/lib/graalvm/ $JAVA_HOME/lib/svm/ $JAVA_HOME/lib/static/ $JAVA_HOME/lib/truffle/ $JDK/lib/

cd $JDK/bin/
ln -s ../lib/svm/bin/native-image ./
