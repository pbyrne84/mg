case class ItemsNotFoundError(items: List[String])

case class ItemFreeDeal(itemText: String, quantityPerReduction: Int) {
  def calculatePriceWithAnyDeals(amount: Int, pricePerItem: Int): Int = {
    val itemsThatCanBeFree = amount / (quantityPerReduction + 1)
    (amount - itemsThatCanBeFree) * pricePerItem
  }
}

sealed trait ItemOrder

case class ValidItemOrder(itemText: String, quantity: Int, pricePerItem: Int) extends ItemOrder

case class InvalidItemOrder(itemText: String) extends ItemOrder

class CheckoutSystem(itemToPriceMap: Map[String, Int], deals: List[ItemFreeDeal]) {

  /**
    * @param items In the real world this would likely be ISBN codes which would follow a structure
    *              that could be deserialized similar to how UUIDs are deserialized
    * @return
    */
  def calculateCost(items: List[String]): Either[ItemsNotFoundError, Int] = {
    val groupedItems: List[ItemOrder] = groupItems(items)
    val invalidOrders =
      groupedItems.collect { case i: InvalidItemOrder => i }.map(_.itemText)

    if (invalidOrders.nonEmpty) {
      Left(ItemsNotFoundError(invalidOrders))
    } else {
      val validOrders = groupedItems.collect { case i: ValidItemOrder => i }
      Right(validOrders.map(calculateTotalCostPerItemType).sum)
    }
  }

  private def groupItems(items: List[String]): List[ItemOrder] = {
    items
      .groupBy(text => text)
      .map {
        case (itemText, items) =>
          itemToPriceMap
            .get(itemText)
            .map { itemPrice =>
              ValidItemOrder(itemText, items.length, itemPrice)
            }
            .getOrElse(
              InvalidItemOrder(itemText)
            )
      }
      .toList
  }

  private def calculateTotalCostPerItemType(validItemOrder: ValidItemOrder): Int = {
    deals
      .find(itemFreeDeal => itemFreeDeal.itemText == validItemOrder.itemText)
      .map { foundDeal =>
        foundDeal.calculatePriceWithAnyDeals(validItemOrder.quantity, validItemOrder.pricePerItem)
      }
      .getOrElse {
        val normalPriceByAmountCalculation = validItemOrder.pricePerItem * validItemOrder.quantity
        normalPriceByAmountCalculation
      }

  }

}
