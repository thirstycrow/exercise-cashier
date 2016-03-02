package exercise.cashier

object Item {

  def apply(product: Product, quantity: Int) = {
    new Item(product, quantity, quantity * product.unitPrice, 0, Nil)
  }
}

case class Item(
    product: Product,
    quantity: Int,
    amount: Int,
    saved: Int,
    promotion: List[Promotion]) {

  def this(product: Product, quantity: Int) = {
    this(product, quantity, quantity * product.unitPrice, 0, Nil)
  }
}
