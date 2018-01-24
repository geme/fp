import org.scalatest.{FlatSpecLike, Matchers}

class ListTest extends FlatSpecLike with Matchers{

  behavior of "ListTest"

  it should "drop" in {
    List.drop(List(1,2,3), 1) shouldEqual List(2,3)
    List.drop(List(1,2,3), 2) shouldEqual List(3)
    List.drop(List(1,2,3), 3) shouldEqual Nil
    List.drop(List(1,2,3), 4) shouldEqual Nil
  }

  it should "sum" in {
    List.sum(List(1,2,3)) shouldEqual 6
    List.sum(List(3,2,1)) shouldEqual 6
  }

  it should "foldLeft" in {
    List.foldLeft(List(1,2,3), 4)(_ + _) shouldEqual 10
    List.foldLeft(List("a", "b", "c"), "d")(_ + _) shouldEqual "dabc"
  }

  it should "foldRight" in {
    List.foldRight(List("a", "b", "c"), "d")(_ + _) shouldEqual "abcd"
  }

  it should "foldRightWithLeft" in {
    List.foldRightWithLeft(List("a", "b", "c"), "d")(_ + _) shouldEqual "abcd"
  }

  it should "product" in {
    List.product(List(1,2,3)) shouldEqual 6
    List.product(List(1,2,3,0)) shouldEqual 0
    List.product(List(4,5,6)) shouldEqual 120
  }

  it should "apply" in {
    List.apply(1,2,3) shouldEqual Cons(1, Cons(2, Cons(3, Nil)))
  }

  it should "tail" in {
    List.tail(List(1,2,3)) shouldEqual List(2,3)
  }

  it should "length" in {
    List.length(List(1,2,3)) shouldEqual 3
  }

  it should "setHead" in {
    List.setHead(4, List(1,2,3)) shouldEqual List(4,2,3)
  }

  it should "dropWhile" in {
    List.dropWhile(List(1,2,3,4,5,6))(_ < 3) shouldEqual List(3,4,5,6)
  }

  it should "reverse" in {
    List.reverse(List(1,2,3)) shouldEqual List(3,2,1)
  }

  it should "append" in {
    List.append(List(1,2,3), 4) shouldEqual List(1,2,3,4)
  }

}
