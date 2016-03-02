package exercise.cashier

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PromotionSpec extends FlatSpec with Matchers {

  val dummyProduct = Product("ITEM_DUMMY", "DUMMY", "个", 100)

  it should "get extra for free" in {
    val promo = ExtraForFree(1, 1, "买二送一", 2, 1)
    promo(Item(dummyProduct, 3)) shouldBe Item(dummyProduct, 3, 200, 100, List(promo))
    promo(Item(dummyProduct, 6)) shouldBe Item(dummyProduct, 6, 500, 100, List(promo))
  }

  it should "not get extra for free if not bought enough" in {
    val promo = ExtraForFree(1, 1, "买二送一", 2, 1)
    promo(Item(dummyProduct, 1)) shouldBe Item(dummyProduct, 1, 100, 0, Nil)
    promo(Item(dummyProduct, 2)) shouldBe Item(dummyProduct, 2, 200, 0, Nil)
  }

  it should "apply discount" in {
    val promo = Discount(2, 2, "95折", 95)
    promo(Item(dummyProduct, 1)) shouldBe Item(dummyProduct, 1, 95, 5, List(promo))
    promo(Item(dummyProduct, 3)) shouldBe Item(dummyProduct, 3, 285, 15, List(promo))
  }
}
