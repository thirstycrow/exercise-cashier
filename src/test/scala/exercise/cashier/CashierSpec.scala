package exercise.cashier

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class CashierSpec extends FlatSpec with Matchers {

  val cashier = new Cashier();

  val cola = Product("ITEM000001", "可口可乐", "瓶", 300)
  val baminton = Product("ITEM000002", "羽毛球", "个", 100)
  val apple = Product("ITEM000003", "羽毛球", "斤", 550)

  it should "aggregate items by product" in {
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
}
