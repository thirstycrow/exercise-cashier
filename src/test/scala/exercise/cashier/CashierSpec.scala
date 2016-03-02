package exercise.cashier

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import java.io.StringWriter
import java.io.PrintWriter

class CashierSpec extends FlatSpec with Matchers {

  val cola = Product("ITEM000005", "可口可乐", "瓶", 300)
  val baminton = Product("ITEM000001", "羽毛球", "个", 100)
  val apple = Product("ITEM000003", "苹果", "斤", 550)
  val products = List(cola, baminton, apple).map(p => p.barcode -> p).toMap

  val extraForFree = ExtraForFree(1, 1, "买二赠一", 2, 1)
  val discount95 = Discount(2, 2, "95折", 95)

  it should "aggregate items by product" in {
    val cashier = new Cashier(null, TestBillingPrinter)
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
    }, TestBillingPrinter);
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, Some(discount95))
    cashier.applyPromotion(Item(cola, 10)) shouldBe Item(cola, 10, 2850, 150, Some(discount95))
  }

  it should "apply promotion properly" in {
    val cashier = new Cashier(new TestRepository {
      override def promotionsForProduct(product: Product) = Seq(extraForFree, discount95)
    }, TestBillingPrinter)
    cashier.applyPromotion(Item(cola, 1)) shouldBe Item(cola, 1, 285, 15, Some(discount95))
    cashier.applyPromotion(Item(cola, 2)) shouldBe Item(cola, 2, 570, 30, Some(discount95))
    cashier.applyPromotion(Item(cola, 3)) shouldBe Item(cola, 3, 600, 300, Some(extraForFree))
  }

  it should "parse input json" in {
    val cashier = new Cashier(new TestRepository {
      override def promotionsForProduct(product: Product) = Seq(extraForFree, discount95)
    }, TestBillingPrinter)
    cashier.parse("""[
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000003-2",
        "ITEM000005",
        "ITEM000005",
        "ITEM000005"
      ]""").toSet shouldBe Set(
      Item(cola, 1),
      Item(cola, 1),
      Item(cola, 1),
      Item(cola, 1),
      Item(cola, 1),
      Item(apple, 2),
      Item(baminton, 1),
      Item(baminton, 1),
      Item(baminton, 1)
    )
  }

  it should "print billing" in {
    testCashier(product => if (product != apple) Seq(extraForFree) else Nil)
      .printBilling(Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)))
    TestBillingPrinter.getBilling shouldBe """***<没钱赚商店>购物清单***
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
"""

    testCashier(_ => Nil)
      .printBilling(Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)))
    TestBillingPrinter.getBilling shouldBe """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：9.00(元)
名称：羽毛球，数量：5个，单价：1.00(元)，小计：5.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：11.00(元)
-------------
总计：25.00(元)
*************
"""

    testCashier(product => if (product == apple) Seq(discount95) else Nil)
      .printBilling(Seq(Item(cola, 3), Item(baminton, 5), Item(apple, 2)))
    TestBillingPrinter.getBilling shouldBe """***<没钱赚商店>购物清单***
名称：可口可乐，数量：3瓶，单价：3.00(元)，小计：9.00(元)
名称：羽毛球，数量：5个，单价：1.00(元)，小计：5.00(元)
名称：苹果，数量：2斤，单价：5.50(元)，小计：10.45(元)，节省0.55(元)
-------------
总计：24.45(元)
节省：0.55(元)
*************
"""

    testCashier(product => if (product == apple) Seq(discount95) else Seq(extraForFree))
      .printBilling(Seq(Item(cola, 3), Item(baminton, 6), Item(apple, 2)))
    TestBillingPrinter.getBilling shouldBe """***<没钱赚商店>购物清单***
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
"""
  }

  it should "work as one piece" in {
    val input = """[
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000001",
        "ITEM000003-2.5",
        "ITEM000005",
        "ITEM000005",
        "ITEM000005"
      ]"""
    testCashier(product => if (product == apple) Seq(discount95) else Seq(extraForFree))
      .printBilling(input)
    TestBillingPrinter.getBilling shouldBe """***<没钱赚商店>购物清单***
名称：羽毛球，数量：5.0个，单价：1.00(元)，小计：4.00(元)
名称：苹果，数量：2.5斤，单价：5.50(元)，小计：13.06(元)，节省0.69(元)
名称：可口可乐，数量：3.0瓶，单价：3.00(元)，小计：6.00(元)
-------------
买二赠一商品：
名称：羽毛球，数量：1个
名称：可口可乐，数量：1瓶
-------------
总计：23.06(元)
节省：4.69(元)
*************
"""
  }

  private def testCashier(getPromotions: Product => Seq[Promotion]) = {
    val repository = new TestRepository {
      override def promotionsForProduct(product: Product) = getPromotions(product)
    }
    new Cashier(repository, TestBillingPrinter, Some("没钱赚商店"));
  }

  private def testBilling(
    getPromotions: Product => Seq[Promotion],
    items: Seq[Item],
    expected: String) {
    val repository = new TestRepository {
      override def promotionsForProduct(product: Product) = getPromotions(product)
    }
    val cashier = new Cashier(repository, TestBillingPrinter, Some("没钱赚商店"));
    cashier.printBilling(items)
    TestBillingPrinter.getBilling shouldBe expected
  }

  private def testBilling(
    getPromotions: Product => Seq[Promotion],
    json: String,
    expected: String) {
    val repository = new TestRepository {
      override def promotionsForProduct(product: Product) = getPromotions(product)
    }
    val cashier = new Cashier(repository, TestBillingPrinter, Some("没钱赚商店"));
    cashier.printBilling(json)
    TestBillingPrinter.getBilling shouldBe expected
  }

  class TestRepository extends Repository {
    def addProduct(product: Product): Unit = ???
    def addProductForPromotion(promotion: Promotion, product: Product): Unit = ???
    def addPromotion(promotion: Promotion): Unit = ???
    def promotionsForProduct(product: Product): Seq[Promotion] = ???
    def getProductByCode(code: String) = products(code)
  }
}

object TestBillingPrinter extends BillingPrinter {

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

  def getBilling: String = {
    val billing = writer.toString()
    System.out.println(billing)
    writer.getBuffer.setLength(0)
    billing
  }
}
