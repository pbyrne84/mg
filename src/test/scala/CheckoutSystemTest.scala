import com.github.pbyrne84.mg.BaseSpec

class CheckoutSystemTest extends BaseSpec {

  //itemToPriceMap and deals would really be data driven from a data source
  private val itemToPriceMap: Map[String, Int] = Map("Apple" -> 60, "Orange" -> 25)
  private val deals = List(
    ItemFreeDeal("Apple", 1),
    ItemFreeDeal("Orange", 2)
  )

  private val checkoutSystem = new CheckoutSystem(itemToPriceMap, deals)

  "the checkout system" should {
    "calculate zero cost for zero items" in {
      //could use EitherValues but it fails horribly if Left was returned with no detail, not a nice gift to leave
      checkoutSystem.calculateCost(List.empty) shouldBe Right(0)
    }

    "calculate the price of a single apple, no-one is forced to take a free one when there is an offer" in {
      checkoutSystem.calculateCost(List("Apple")) shouldBe Right(60)
    }

    "return an error if an item is not found when there is only one item" in {
      checkoutSystem.calculateCost(List("Banana")) shouldBe Left(ItemsNotFoundError(List("Banana")))
    }

    "return the list of invalid items as an error" in {
      //These sort of decisions really should involve a customer rep. A collection of
      //invalid items etc.
      checkoutSystem.calculateCost(List("Apple", "Apple", "Kumquat", "Apple", "Tangerine")) shouldBe Left(
        ItemsNotFoundError(List("Kumquat", "Tangerine"))
      )
    }

    "calculate an offer for apples when the items exactly match the offer of buy one get one free" in {
      checkoutSystem.calculateCost(List("Apple", "Apple")) shouldBe Right(60)
    }

    "calculate an offer for apples when there are multiple matches" in {
      checkoutSystem.calculateCost(List("Apple", "Apple", "Apple", "Apple")) shouldBe Right(120)
    }

    "calculate an offer for oranges when the items exactly match the offer of buy two get one free" in {
      checkoutSystem.calculateCost(List("Orange", "Orange", "Orange")) shouldBe Right(50)
    }

    "calculate the price for oranges handling the fact the potential free one was not taken" in {
      checkoutSystem.calculateCost(List("Orange", "Orange")) shouldBe Right(50)
    }

    "calculate the price for multiple items including deals" in {
      checkoutSystem.calculateCost(List("Apple", "Apple", "Orange", "Apple")) shouldBe Right(145)
    }

  }
}
