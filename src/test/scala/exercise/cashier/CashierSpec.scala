package exercise.cashier

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import java.io.StringWriter
import java.io.PrintWriter

class CashierSpec extends FlatSpec with Matchers {

  val cola = Product("ITEM000001", "可口可乐", "瓶", 300)
  val baminton = Product("ITEM000002", "羽毛球", "个", 100)
  val apple = Product("ITEM000003", "苹果", "斤", 550)

  val extraForFree = ExtraForFree(1, 1, "买二赠一", 2, 1)
  val discount95 = Discount(2, 2, "95折", 95)

  it should "aggregate items by product" in {
    val cashier = new Cashier(null, new TestBillingPrinter);
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
    }, new TestBillingPrinter);
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, Some(discount95))
    cashier.applyPromotion(Item(cola, 10)) shouldBe Item(cola, 10, 2850, 150, Some(discount95))
  }

  it should "apply promotion properly" in {
    val cashier = new Cashier(new TestRepository {
      override def promotionsForProduct(product: Product) = Seq(extraForFree, discount95)
    }, new TestBillingPrinter);
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, Some(discount95))
    cashier.applyPromotion(Item(cola, 2)) shouldBe Item(cola, 2, 570, 30, Some(discount95))
    cashier.applyPromotion(Item(cola, 3)) shouldBe Item(cola, 3, 600, 300, Some(extraForFree))
  }

  it should "print billing" in {
    testBilling(
      product => if (product != apple) Seq(extraForFree) else Nil,
      Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)),
      """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：6.00(元)
名称：羽毛球，数量：5个，单价：1.00(元)，小计：4.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：11.00(元)
-------------
买二赠一商品：
名称：可口可乐，数量：1瓶
名称：羽毛球，数量：1个
-------------
总计：21.00(元)
节省：4.00(元)
*************
""")

    testBilling(
      _ => Nil,
      Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)),
      """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：9.00(元)
名称：羽毛球，数量：5个，单价：1.00(元)，小计：5.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：11.00(元)
-------------
总计：25.00(元)
*************
""")

    testBilling(
      product => if (product == apple) Seq(discount95) else Nil,
      Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)),
      """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：9.00(元)
名称：羽毛球，数量：5个，单价：1.00(元)，小计：5.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：10.45(元)，节省0.55(元)
-------------
总计：24.45(元)
节省：0.55(元)
*************
""")

    testBilling(
      product => if (product == apple) Seq(discount95) else Seq(extraForFree),
      Seq(Item(cola, 3), Item(baminton, 6), Item(apple, 2)),
      """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：6.00(元)
名称：羽毛球，数量：6个，单价：1.00(元)，小计：4.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：10.45(元)，节省0.55(元)
-------------
买二赠一商品：
名称：可口可乐，数量：1瓶
名称：羽毛球，数量：2个
-------------
总计：20.45(元)
节省：5.55(元)
*************
""")
  }

  private def testBilling(
    getPromotions: Product => Seq[Promotion],
    items: Seq[Item],
    expected: String) {
    val printer = new TestBillingPrinter
    val repository = new TestRepository {
      override def promotionsForProduct(product: Product) = getPromotions(product)
    }
    val cashier = new Cashier(repository, printer, Some("没钱赚商店"));
    cashier.printBilling(items)
    printer.getBilling shouldBe expected
  }
}

class TestRepository extends Repository {
  def addProduct(product: Product): Unit = ???
  def addProductForPromotion(promotion: Promotion, product: Product): Unit = ???
  def addPromotion(promotion: Promotion): Unit = ???
  def promotionsForProduct(product: Product): Seq[Promotion] = ???
}

class TestBillingPrinter extends BillingPrinter {

  private val writer = new StringWriter()
  private val printer = new PrintWriter(writer)

  def print(x: Any): BillingPrinter = {
    printer.print(x)
    this
  }

  def println(): BillingPrinter = {
    printer.println()
    this
  }

  override def endOfBilling() = {
    super.endOfBilling()
    System.out.println(getBilling)
    this
  }

  def getBilling: String = {
    writer.toString()
  }
}
