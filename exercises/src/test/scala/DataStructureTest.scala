
import fpinscala.datastructures._
import org.scalatest.{FlatSpec, Matchers}

class DataStructureTest extends FlatSpec with Matchers {


  "SubSequence" should "be able find subseq List(1,2,3) in List(2,3,4,1,2,3)" in {
    assert(List.hasSubsequence(fpinscala.datastructures.List(2, 3, 4, 1, 2, 3), fpinscala.datastructures.List(1, 2, 3)))
  }

  it should "be able to find it in List(2,3,4,1,2,5,1,2,3,4,5)" in {
    assert(List.hasSubsequence(fpinscala.datastructures.List(2,3,4,1,2,5,1,2,3,4,5), fpinscala.datastructures.List(1, 2, 3)))
  }

  it should "not find it in a List(3,4,5,2,3,5,6,7)" in {
    assert(!List.hasSubsequence(fpinscala.datastructures.List(3,4,5,2,3,5,6,7), fpinscala.datastructures.List(1,2,3)))
  }

  it should "not find a in empty list" in {
    assert(!List.hasSubsequence(fpinscala.datastructures.List(), fpinscala.datastructures.List(1,2,3)))
  }

  it should "not find an empty list in any list" in {
    assert(List.hasSubsequence(fpinscala.datastructures.List(1,2,3), fpinscala.datastructures.List()))
  }

}
