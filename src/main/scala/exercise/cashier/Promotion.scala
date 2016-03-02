package exercise.cashier

trait Promotion {

  def id: Int

  def name: String

  def priority: Int

  def apply(item: Item): Item
}

case class ExtraForFree(
  id: Int,
  priority: Int,
  name: String,
  minimum: Int,
  extra: Int)
    extends Promotion {

  def apply(item: Item): Item = {
    if (item.quantity >= minimum + extra) {
      val saved = item.product.unitPrice * extra
      item.copy(
        amount = item.amount - saved,
        saved = item.saved + saved,
        promotion = this :: item.promotion)
    } else {
      item
    }
  }
}

case class Discount(
  id: Int,
  priority: Int,
  name: String,
  percentage: Int)
    extends Promotion {

  def apply(item: Item): Item = {
    val amount = item.amount * percentage / 100
    item.copy(
      amount = amount,
      saved = item.amount - amount,
      promotion = this :: item.promotion
    )
  }
}
