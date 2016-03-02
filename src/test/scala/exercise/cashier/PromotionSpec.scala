package exercise.cashier

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PromotionSpec extends FlatSpec with Matchers {

  val dummyProduct = Product("ITEM_DUMMY", "DUMMY", "个", 100)

  it should "get extra for free" in {
    val promo = ExtraForFree(1, 1, "买三送二", 3, 2)
    promo(Item(dummyProduct, 4)) shouldBe Item(dummyProduct, 4, 300, 100, Some(promo))
    promo(Item(dummyProduct, 5)) shouldBe Item(dummyProduct, 5, 300, 200, Some(promo))
    promo(Item(dummyProduct, 6)) shouldBe Item(dummyProduct, 6, 400, 200, Some(promo))
    promo(Item(dummyProduct, 7)) shouldBe Item(dummyProduct, 7, 500, 200, Some(promo))
    promo(Item(dummyProduct, 8)) shouldBe Item(dummyProduct, 8, 600, 200, Some(promo))
    promo(Item(dummyProduct, 9)) shouldBe Item(dummyProduct, 9, 600, 300, Some(promo))
    promo(Item(dummyProduct, 10)) shouldBe Item(dummyProduct, 10, 600, 400, Some(promo))
  }

  it should "not get extra for free if not bought enough" in {
    val promo = ExtraForFree(1, 1, "买二送一", 2, 1)
    promo(Item(dummyProduct, 1)) shouldBe Item(dummyProduct, 1, 100, 0, None)
    promo(Item(dummyProduct, 2)) shouldBe Item(dummyProduct, 2, 200, 0, None)
  }

  it should "apply discount" in {
    val promo = Discount(2, 2, "95折", 95)
    promo(Item(dummyProduct, 1)) shouldBe Item(dummyProduct, 1, 95, 5, Some(promo))
    promo(Item(dummyProduct, 3)) shouldBe Item(dummyProduct, 3, 285, 15, Some(promo))
  }
}
