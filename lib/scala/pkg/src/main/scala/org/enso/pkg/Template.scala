package org.enso.pkg

/** Base trait for the project templates. */
sealed trait Template {

  /** The template name. */
  def name: String
}
object Template {

  /** Create a template from string.
    *
    * @param template the template name
    * @return the template for the provided name
    */
  def fromString(template: String): Option[Template] =
    allTemplates.find(_.name == template.toLowerCase)

  /** The default project template. */
  case object Default extends Template {
    override val name = "default"
  }

  case object ColoradoCovid extends Template {
    override val name = "colorado_covid"
  }

  case object Kmeans extends Template {
    override val name = "kmeans"
  }

  case object NasdaqReturns extends Template {
    override val name = "nasdaqreturns"
  }

  case object Orders extends Template {
    override val name = "orders"
  }

  case object Restaurants extends Template {
    override val name = "restaurants"
  }

  case object Stargazers extends Template {
    override val name = "stargazers"
  }

  case object MonthlySales extends Template {
    override val name = "monthly_sales"
  }

  case object BankHolidayRain extends Template {
    override val name = "bank_holiday_rain"
  }

  case object GettingStartedReading extends Template {
    override val name = "getting_started_reading"
  }

  case object GettingStartedAggregating extends Template {
    override val name = "getting_started_aggregating"
  }

  case object GettingStartedCleansing extends Template {
    override val name = "getting_started_cleansing"
  }

  case object GettingStartedSelecting extends Template {
    override val name = "getting_started_selecting"
  }

  val allTemplates = Seq(
    Default,
    Orders,
    Restaurants,
    Stargazers,
    ColoradoCovid,
    Kmeans,
    NasdaqReturns,
    MonthlySales,
    BankHolidayRain,
    GettingStartedReading,
    GettingStartedAggregating,
    GettingStartedCleansing,
    GettingStartedSelecting
  )
}
