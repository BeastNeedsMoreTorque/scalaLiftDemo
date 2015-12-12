package code.model

import net.liftweb.common.Loggable

/**
  * Created by philippederome on 15-11-01.
  * The elements of a product from LCBO catalogue that we deem of relevant interest for this toy demo.
  * This is captured from JSON parsing. Provides primitive data item converters for intuitive formatting and unit usage.
  */
case class Product(id: Int,
                   is_discontinued: Boolean,
                   `package`: String,
                   total_package_units: Int,
                   primary_category: String,
                   name: String,
                   image_thumb_url: String,
                   origin: String,
                   price_in_cents: Int,
                   alcohol_content: Int,
                   volume_in_milliliters: Int) extends Loggable {
  // intentional aliasing allowing more standard naming convention.
  val primaryCategory = primary_category
  val isDiscontinued = is_discontinued
  val totalPackageUnits = total_package_units
  val imageThumbUrl = image_thumb_url
  val Package = `package` // alias to avoid back ticks

  // Change unit of currency from cents to dollars and Int to String
  def price: String = {
    val p = price_in_cents.toInt / 100.0
    f"$p%1.2f"
  } // since we perverted meaning somewhat by changing unit from cents to dollars

  // Change scale by 100 to normal conventions, foregoing LCBO's use of Int (for ex. 1200 becomes "12.0%" including the percent sign)
  def alcoholContent: String = {
    val a = alcohol_content.toInt / 100.0
    f"$a%1.1f%%"
  }

  // intentional change of scale from ml to L, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def volumeInLitre: String = {
    val v = volume_in_milliliters.toInt / 1000.0
    f"$v%1.3f L"
  }

  /**
    *
    * @return an ordered list of DOM li element values to be listed in sequence vertically, representing most of the interesting data of the product
    */
  def createProductLIElemVals: List[String] =
  // order is important and would be dependent on web designer input, we could possibly find ordering rule either in database or in web design. This assumes order can be fairly static.
    s"Package: $Package" ::
      s"Package Units: $totalPackageUnits" ::
      "Price: $" + price ::
      s"Alcohol content: $alcoholContent" ::
      s"Origin: $origin" ::
      s"Volume: $volumeInLitre" ::
      Nil
}
