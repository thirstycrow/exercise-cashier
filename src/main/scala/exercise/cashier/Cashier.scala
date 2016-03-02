package exercise.cashier

import scala.collection.mutable

class Cashier(repo: Repository, title: Option[String] = None) {

  def aggregateByProduct(items: Seq[Item]): Iterable[Item] = {
    val summedItems = items.foldLeft(Map[Product, Item]()) { (m, i) =>
      val total = m.get(i.product)
        .map(total => Item(i.product, total.quantity + i.quantity))
        .getOrElse(i)
      m + (i.product -> total)
    }
    summedItems.values
  }

  def applyPromotion(item: Item) = {
    repo.promotionsForProduct(item.product)
      .map(_.apply(item))
      .find(_.promotion.nonEmpty)
      .getOrElse(item)
  }
}

trait Repository {

  /**
   * Add a promotion.
   * @param promotion
   */
  def addPromotion(promotion: Promotion): Unit

  /**
   * Add a product.
   * @param product
   */
  def addProduct(product: Product): Unit

  /**
   * Add a product for a promotion
   * @param promotion
   * @param product
   */
  def addProductForPromotion(promotion: Promotion, product: Product): Unit

  /**
   * Get all promotions that is applicable to a product, ordered by their priority ascendingly.
   * @param product
   */
  def promotionsForProduct(product: Product): Seq[Promotion]
}
