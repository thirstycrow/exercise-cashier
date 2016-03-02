package exercise.cashier

import java.io.PrintWriter

object Item {

  val separator = '，'

  def apply(product: Product, quantity: Int) = {
    new Item(product, quantity, quantity * product.unitPrice, 0, None)
  }
}

case class Item(
    product: Product,
    quantity: Int,
    amount: Int,
    saved: Int,
    promotion: Option[Promotion]) {

  def this(product: Product, quantity: Int) = {
    this(product, quantity, quantity * product.unitPrice, 0, None)
  }
}
