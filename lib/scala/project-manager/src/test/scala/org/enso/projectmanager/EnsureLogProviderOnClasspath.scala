package org.enso.projectmanager

import org.scalatest.Suite
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers.{be, contain, empty}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.slf4j.spi.SLF4JServiceProvider

import java.util.ServiceLoader
import scala.collection.mutable.ListBuffer

class EnsureLogProviderOnClasspath extends Suite with AnyFlatSpecLike {

  /** In the testing suite, `org.enso.logger.TestLogProvider` should be among the log providers.
    * Note that we want to run the whole suite of `project-manager` with that provider
    */
  "Test log providers" should "have org.enso.logger.TestLogProvider" in {
    val sl                                          = ServiceLoader.load(classOf[SLF4JServiceProvider])
    val serviceIterator                             = sl.iterator()
    val providers: ListBuffer[SLF4JServiceProvider] = ListBuffer()
    while (serviceIterator.hasNext) {
      providers.addOne(serviceIterator.next())
    }
    val providerClasses = providers.toList.map(_.getClass.getName)
    providers shouldNot be(empty)
    providerClasses should contain("org.enso.logger.TestLogProvider")
  }
}
