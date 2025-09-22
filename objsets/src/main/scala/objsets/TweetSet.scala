package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */

  /*
  추상 클래스 TweetSet -> 이를 상속받는 자식 클래스 Empty/NonEmpty
  구현해야할 method를 추상 or 자식 어디에 정의해야 효율적인지 생각해야함
  */

  // 조건 p에 맞는 TweetSet을 반환
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty) // 조건 p와 비어있는 acc 전달

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  // this와 that의 합집합인 TweetSet을 반환
  // Empty와 NonEmpty에 foreach와 incl이 각각 구현되어있어서 다형성이 보장되므로 직접 method 구현
  def union(that: TweetSet): TweetSet = {
     var resultSet = that
     this.foreach(tweet => resultSet = resultSet.incl(tweet)) // that에 this의 tweet을 하나씩 추가하는 형태
     resultSet
   }

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  // TweetSet에서 Retweet이 가장 많은 Tweet을 반환
  // Empty는 예외처리, NonEmpty는 실제로 반환. 각 로직이 다르기에 자식 클래스에 분리하여 로직 구현
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  // TweetSet에서 Retweet을 기준으로 내림차순 정렬된 TweetList를 반환
  // Empty와 NonEmpty 각 로직이 다르기에 자식 클래스에 분리하여 로직 구현
  def descendingByRetweet: TweetList
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  // 예외처리
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("Calling `mostRetweeted` on an empty set")

  def descendingByRetweet: TweetList = Nil
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val newAcc = if (p(elem)) acc.incl(elem) else acc // 조건에 맞는 tweet elem을 acc에 추가
    right.filterAcc(p, left.filterAcc(p, newAcc)) // 왼쪽 파트 먼저 newAcc를 업데이트 -> 이후 오른쪽 파트 진행
  }

  def mostRetweeted: Tweet = {
    var mostTweet = this.elem
    this.foreach(tweet => { // 순회하며 Tweet 클래스의 retweets 값이 가장 큰 값을 찾기
      if(tweet.retweets > mostTweet.retweets) mostTweet = tweet
    })
    mostTweet
  }

  def descendingByRetweet: TweetList = {
    val mostTweet = this.mostRetweeted // 현재 가장 retweets이 큰 tweet을 찾음
    new Cons(mostTweet, this.remove(mostTweet).descendingByRetweet) // mostTweet을 머리로, 나머지는 재귀적으로 다시 정렬
  }
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  // google 키워드를 포함하는지 필터
  lazy val googleTweets: TweetSet =
    TweetReader.allTweets.filter(tweet => google.exists(keyword => tweet.text.contains(keyword)))
  // apple 키워드를 포함하는지 필터
  lazy val appleTweets: TweetSet =
    TweetReader.allTweets.filter(tweet => apple.exists(keyword => tweet.text.contains(keyword)))
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
