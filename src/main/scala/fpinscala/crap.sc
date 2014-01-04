import scala._
import scala.Some

val crap = List("2")

List(Some(1)).flatMap(
  x => x.map(_ + 1)
)

for {
  l <- List(Some(1), None, Some(2))
  r <- l
  l2 <- List(Some(1), None, Some(2))
  r2 <- l2
} yield {
  (r, r2)
}





