package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val oneToThree = union(singletonSet(3), union(singletonSet(2), singletonSet(1)))
  printSet(intersect(oneToThree, union(singletonSet(3), union(singletonSet(4), singletonSet(5)))))
  printSet(diff(oneToThree, union(singletonSet(3), union(singletonSet(4), singletonSet(5)))))

  val oneToHundred: Set = x => x > 0 & x < 101
  println(forall(filter(oneToHundred, x => x % 2 == 0), x => x > 0))
  println(exists(filter(oneToHundred, x => x % 2 == 0), x => x == 100))
  println(exists(filter(oneToHundred, x => x % 2 == 0), x => x == 1))

  printSet(map(oneToThree, x => x * x))
}
