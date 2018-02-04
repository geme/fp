import org.scalatest.{FlatSpecLike, Matchers}
import stream._
import list._

class StreamTest extends FlatSpecLike with Matchers {
   
    behavior of "Stream"

    it should "toList" in {
        val stream: Stream[Int] = Stream(1,2,3,4,5)
        val list: List[Int] = List(1,2,3,4,5)

        Stream.toList(stream) shouldEqual list
    }
    
    it should "take" in {
        val stream: Stream[Int] = Stream(1,2,3,4,5)
//      Stream.take(stream, 3) shouldEqual Stream(1,2,3)
    }

}
