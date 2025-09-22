package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    // descending 테스트를 위한 복합 데이터
    val e = new Tweet("e", "e body", 100)
    val f = new Tweet("f", "f body", 0)
    val complexSet = set5.incl(e).incl(f) // {a(20), b(20), c(7), d(9), e(100), f(0)}
  }

  // TweetList를 Scala의 List로 변환하는 헬퍼 함수
  def tweetListToList(tl: TweetList): List[Tweet] =
    if (tl.isEmpty) List()
    else tl.head :: tweetListToList(tl.tail)

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)

      // 유저가 'c'이면서 리트윗이 7개인 트윗 필터링
      assert(size(set5.filter(tw => tw.user == "c" && tw.retweets == 7)) === 1)

      // 결과가 아무것도 없는 필터링
      assert(size(set5.filter(tw => tw.retweets > 100)) === 0)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)

      // 여러 원소가 겹치는 두 집합의 union
      assert(size(set5.union(set4c)) === 4)

      // 자기 자신과의 union
      assert(size(set3.union(set3)) === 2)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")

      // 리트윗 수가 다양한 집합의 정렬 순서 검증
      val complexTrends = tweetListToList(complexSet.descendingByRetweet)
      val retweets = complexTrends.map(tw => tw.retweets)
      assert(retweets === List(100, 20, 20, 9, 7, 0))

      // 원소가 하나인 집합의 정렬
      val singleTrend = set2.descendingByRetweet
      assert(!singleTrend.isEmpty)
      assert(singleTrend.head.user === "a")
      assert(singleTrend.tail.isEmpty)
    }
  }
}
