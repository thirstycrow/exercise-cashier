package exercise.cashier

import java.io.PrintWriter

object Item {

  def apply(product: Product, quantity: BigDecimal) = {
    new Item(product, quantity, (quantity * BigDecimal(product.unitPrice)).intValue(), 0, None)
  }
}

case class Item(
    product: Product,
    quantity: BigDecimal,
    amount: Int,
    saved: Int,
    promotion: Option[Promotion]) {

  def this(product: Product, quantity: Int) = {
    this(product, quantity, quantity * product.unitPrice, 0, None)
  }
}
