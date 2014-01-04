package crap

import org.scalatest.FlatSpec

/**
  */
class esoterica extends FlatSpec {

  it should "something goes awry" in {
    val list1 = List(Some(1), Some(2), None)
    val list2: List[Option[String]] = List(Some("1"), Some("2"), None)

    val funk = list1.flatMap(
      x => {
        list2.map(
          y =>
            x.toString + y
        )
      }
    )

    println(funk)

    val f2 = for {
      l1 <- list1
      l2 <- list2
    } yield (l1.toString + l2)
    println(f2)
  }


}
