package exercise.cashier

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class CashierSpec extends FlatSpec with Matchers {

  val cola = Product("ITEM000001", "可口可乐", "瓶", 300)
  val baminton = Product("ITEM000002", "羽毛球", "个", 100)
  val apple = Product("ITEM000003", "羽毛球", "斤", 550)

  val extraForFree = ExtraForFree(1, 1, "买二送一", 2, 1)
  val discount95 = Discount(2, 2, "95折", 95)

  it should "aggregate items by product" in {
    val cashier = new Cashier(null);
    val items = Seq(
      Item(cola, 1),
      Item(baminton, 1),
      Item(cola, 1),
      Item(apple, 2),
      Item(cola, 1),
      Item(apple, 3))
    cashier.aggregateByProduct(items).toSet shouldBe
      Set(Item(cola, 3), Item(baminton, 1), Item(apple, 5))
  }

  it should "apply promotion" in {
    val cashier = new Cashier(new TestRepository {
      override def promotionsForProduct(product: Product) = Seq(discount95)
    });
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, List(discount95))
    cashier.applyPromotion(Item(cola, 10)) shouldBe Item(cola, 10, 2850, 150, List(discount95))
  }

  it should "apply promotion properly" in {
    val cashier = new Cashier(new TestRepository {
      override def promotionsForProduct(product: Product) = Seq(extraForFree, discount95)
    });
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, List(discount95))
    cashier.applyPromotion(Item(cola, 2)) shouldBe Item(cola, 2, 570, 30, List(discount95))
    cashier.applyPromotion(Item(cola, 3)) shouldBe Item(cola, 3, 600, 300, List(extraForFree))
  }
}

class TestRepository extends Repository {
  def addProduct(product: Product): Unit = ???
  def addProductForPromotion(promotion: Promotion, product: Product): Unit = ???
  def addPromotion(promotion: Promotion): Unit = ???
  def promotionsForProduct(product: Product): Seq[Promotion] = ???
}
