import scala.annotation.tailrec

case class ItemNotFoundError(item: String)

class CheckoutSystem(itemToPriceMap: Map[String, Int]) {
  def calculateCost(items: List[String]): Either[ItemNotFoundError, Int] = {
    processItems(items, 0)
  }

  @tailrec // I find tailrec stuff easier to break out else I confuse myself with potential variable shadowing
  private def processItems(
      remainingItems: List[String],
      total: Int
  ): Either[ItemNotFoundError, Int] = {
    remainingItems match {
      case ::(currentItem, next) =>
        itemToPriceMap.get(currentItem) match {
          case Some(itemPrice) =>
            val newTotal = total + itemPrice
            processItems(next, newTotal)

          case None =>
            Left(ItemNotFoundError(currentItem))
        }

      case Nil =>
        Right(total)
    }

  }
}
