package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  // 새로운 타입 "Set" : Int 파라미터를 받아서 Boolean 반환
  // 아래 함수들은 Int => Boolean 타입의 "함수"를 반환해야함. Anonymous Function
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  // singletonSet: 정수 하나를 원소로 갖는 단일 원소 집합. x가 해당 집합에 속하는지 판단
  def singletonSet(elem: Int): Set = (x: Int) => (x == elem)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  // union: x가 s와 t의 합집합에 속하는지 판단
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  // intersect: x가 s와 t의 교집합에 속하는지 판단
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  // diff: x가 s와 t의 차집함(s - t)에 속하는지 판단
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  // filter: x가 s 집합에 대해 p를 만족하는 부분집합에 속하는지 판단
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  // forall: 범위 내에서 s의 모든 원소가 조건 p를 만족하는지 검사
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true // -1000 ~ 1000까지 모든 원소 통과
      else if (s(a) && !p(a)) false // 조건을 만족하지 않는 원소가 있다면 false
      else iter(a + 1)
    }
    iter(-bound) // -1000부터 시작
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  // exists: s에 조건 p를 만족하는 원소가 하나라도 존재하는지 검사
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x: Int) => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  // map: y가 집합 s의 각 원소에 함수 f가 적용된 새로운 집합에 속하는지 판단
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, (x: Int) => y == f(x))

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
