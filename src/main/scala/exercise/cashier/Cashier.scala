package exercise.cashier

import scala.collection.mutable
import java.io.PrintWriter

class Cashier(repo: Repository, printer: BillingPrinter, title: Option[String] = None) {

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

  def printBilling(items: Seq[Item]): Unit = {

    val billingItems = aggregateByProduct(items)
      .map(applyPromotion)

    printer.printTitle(title)
      .printItems(billingItems)
      .paragraph()
      .printAppliedItems(billingItems)
      .printTotal(billingItems)
      .endOfBilling()
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

trait BillingPrinter {

  def print(x: Any): BillingPrinter

  def println(): BillingPrinter

  def printMoney(amount: Int) = print(BigDecimal(amount).setScale(2) / 100).print("(元)")

  def separator() = print("，")

  def paragraph() = print("-------------").println()

  def endOfBilling() = print("*************").println()

  def printTitle(title: Option[String]) = {
    print("***")
    title.foreach(t => print('<').print(t).print('>'))
    print("购物清单***")
    println()
  }

  def printName(name: String) = print("名称：").print(name)

  def printQuantity(quantity: BigDecimal, unit: String) = print("数量：").print(quantity).print(unit)

  def printItems(items: Iterable[Item]) = {
    items.foreach(printItem)
    this
  }

  def printItem(item: Item) = {
    printName(item.product.name)
      .separator().printQuantity(item.quantity, item.product.unit)
      .separator().print("单价：").printMoney(item.product.unitPrice)
      .separator().print("小计：").printMoney(item.amount)
    item.promotion.filter(_.isInstanceOf[PrintInline]).foreach(_ =>
      separator().print("节省").printMoney(item.saved))
    println()
  }

  def printAppliedItems(items: Iterable[Item]) = {
    items.filter(i => i.promotion.isDefined && i.promotion.get.isInstanceOf[PrintAppliedItems])
      .groupBy(_.promotion)
      .foreach {
        case (Some(promotion: PrintAppliedItems), appliedItems) =>
          print(promotion.name).print("商品：").println()
          appliedItems.foreach { item =>
            promotion.printItem(item, this)
          }
          paragraph()
      }
    this
  }

  def printTotal(items: Iterable[Item]) = {
    val total = items.iterator.map(_.amount).sum
    val saved = items.iterator.map(_.saved).sum
    print("总计：").printMoney(total).println()
    if (saved > 0) {
      print("节省：").printMoney(saved).println()
    }
    this
  }
}
