package exercise.cashier

class Cashier(title: Option[String] = None) {

  def aggregateByProduct(items: Seq[Item]): Iterable[Item] = {
    val summedItems = items.foldLeft(Map[Product, Item]()) { (m, i) =>
      val total = m.get(i.product)
        .map(total => Item(i.product, total.quantity + i.quantity))
        .getOrElse(i)
      m + (i.product -> total)
    }
    summedItems.values
  }
}
