#!/bin/bash -x
set -e

if ! [ -f $JAVA_HOME/bin/jlink ]; then
  echo Specify JAVA_HOME environment variable
  exit 1
fi

rm -rf jdk

JS_MODULES=org.graalvm.nativeimage,org.graalvm.nativeimage.builder,org.graalvm.nativeimage.base,org.graalvm.nativeimage.driver,org.graalvm.nativeimage.librarysupport,org.graalvm.nativeimage.objectfile,org.graalvm.nativeimage.pointsto,com.oracle.graal.graal_enterprise,com.oracle.svm.svm_enterprise,jdk.compiler.graal,jdk.httpserver,java.naming,java.net.http
DEBUG_MODULES=jdk.jdwp.agent
PYTHON_MODULES=jdk.security.auth,java.naming

$JAVA_HOME/bin/jlink --module-path $JAVA_HOME/lib/svm/bin/../../graalvm/svm-driver.jar:$JAVA_HOME/lib/svm/bin/../builder/native-image-base.jar:$JAVA_HOME/lib/svm/bin/../builder/objectfile.jar:$JAVA_HOME/lib/svm/bin/../builder/pointsto.jar:$JAVA_HOME/lib/svm/bin/../builder/svm-enterprise.jar:$JAVA_HOME/lib/svm/bin/../builder/svm.jar:$JAVA_HOME/lib/svm/bin/../library-support.jar --output jdk  --add-modules $JS_MODULES,$DEBUG_MODULES,$PYTHON_MODULES
cp -r $JAVA_HOME/lib/graalvm/ $JAVA_HOME/lib/svm/ $JAVA_HOME/lib/static/ $JAVA_HOME/lib/truffle/ ./jdk/lib/

cd jdk/bin/
ln -s ../lib/svm/bin/native-image ./
cd ../../

cp $HOME/.m2/repository/org/graalvm/shadowed/icu4j/24.1.0-SNAPSHOT/icu4j-24.1.0-SNAPSHOT.jar ./built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/component/icu4j-24.0.0.jar

#rm ./built-distribution/enso-engine*/enso-*/component/icu4j*.jar
#rm ./built-distribution/enso-engine*/enso-*/component/js-language*.jar
#rm ./built-distribution/enso-engine*/enso-*/component/regex*.jar
#rm ./built-distribution/enso-engine*/enso-*/component/python-*.jar
