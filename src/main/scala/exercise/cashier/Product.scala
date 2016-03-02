package exercise.cashier

case class Product(
  barcode: String,
  name: String,
  unit: String,
  unitPrice: Int,
  categoryId: Option[Int] = None)
