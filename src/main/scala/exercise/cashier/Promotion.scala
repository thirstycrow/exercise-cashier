package exercise.cashier

object Promotion {

  implicit val byPriority = new Ordering[Promotion] {
    def compare(a: Promotion, b: Promotion): Int = {
      a.priority - b.priority
    }
  }
}

trait Promotion {

  def id: Int

  def name: String

  def priority: Int

  def apply(item: Item): Item
}

trait PrintInline

trait PrintAppliedItems {
  def printItem(item: Item, printer: BillingPrinter) = {
    printer.printName(item.product.name)
      .separator().printQuantity(item.saved / item.product.unitPrice, item.product.unit)
      .println()
  }
}

case class ExtraForFree(
  id: Int,
  priority: Int,
  name: String,
  minimum: Int,
  extra: Int)
    extends Promotion
    with PrintAppliedItems {

  def apply(item: Item): Item = {
    if (item.quantity > minimum) {
      val sum = minimum + extra
      val part2 = item.quantity % sum - minimum
      val freeQuantity = (item.quantity / sum).intValue() * extra +
        (if (part2 > 0) part2 else BigDecimal.valueOf(0))
      val saved = (item.product.unitPrice * freeQuantity).intValue()
      item.copy(
        amount = item.amount - saved,
        saved = item.saved + saved,
        promotion = Some(this))
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
    extends Promotion
    with PrintInline {

  def apply(item: Item): Item = {
    val amount = item.amount * percentage / 100
    item.copy(
      amount = amount,
      saved = item.amount - amount,
      promotion = Some(this)
    )
  }
}
