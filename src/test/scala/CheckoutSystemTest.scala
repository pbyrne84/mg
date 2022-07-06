import com.github.pbyrne84.mg.BaseSpec

class CheckoutSystemTest extends BaseSpec {

  private val checkoutSystem = new CheckoutSystem(
    Map(
      "Apple" -> 60,
      "Orange" -> 25
    )
  )

  "the checkout system" should {
    "calculate zero cost for zero items" in {
      checkoutSystem.calculateCost(List.empty) shouldBe Right(0)
    }

    "calculate the price of a single apple" in {
      checkoutSystem.calculateCost(List("Apple")) shouldBe Right(60)
    }

    "return an error if an item is not found when there is only one item" in {
      checkoutSystem.calculateCost(List("Banana")) shouldBe Left(ItemNotFoundError("Banana"))
    }

    "calculate the price for multiple items" in {
      checkoutSystem.calculateCost(List("Apple", "Apple", "Orange", "Apple")) shouldBe Right(205)
    }

    "fail early on any unknown items" in {
      //These sort of decisions really should involve a customer rep. A collection of
      //invalid items etc.
      checkoutSystem.calculateCost(List("Apple", "Apple", "Kumquat", "Apple")) shouldBe Left(
        ItemNotFoundError("Kumquat")
      )
    }

  }
}
