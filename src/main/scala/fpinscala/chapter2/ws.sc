
import fpinscala.datastructures._

List.product(List(1, 2, 3, 4))
List.product(List(2))
List.product(List[Double]())
List(1, 2, 3, 4).toString
List(1).toString
List().toString
println("fill")
List.fill(10, "a")
List.fill(10, "a").length
List.fill(0, "a").length
println("fold left")
List[Int]().foldLeft(0)(_ + _)
List[Int]().foldLeft(0)(_ * _)
List(1, 2, 3, 4).foldLeft(1)(_ + _)
List(1).foldLeft(1)(_ + _)
List[Int]().foldLeft(-1)(_ + _)
println("fold right")
List[Int]().foldRight(0)(_ + _)
List[Int]().foldRight(0)(_ * _)
List(1, 2, 3, 4).foldRight(1)(_ + _)
List(1).foldRight(1)(_ + _)
List[Int]().foldRight(-1)(_ + _)
println("fold with Nil")
List(1,2,3).foldRight(Nil : List[Int])(Cons(_,_))
Cons(1,Cons(2,Cons(3,Nil)))
println("head and tail")
List().headOption
List(1).head
List(1).tail
List(1, 2, 3).tail
List().tailOption
List(1, 2, 3).tail
List.setHead(0, List(1, 2, 3)) // should be List(0,2,3)
println("drops")
val l = List(1, 2, 3, 4)
l.drop(0)
l.drop(2)
l.drop(l.length)
l.drop(l.length + 1)
println("drop while")
List(1, 2, 3, 4).dropWhile(_ < 5)
List(1, 2, 3, 4).dropWhile(_ < 1)
List(1, 2, 3, 4).dropWhile(_ < 2)
println("reversals")
Nil.reverse
List(1).reverse
List(1, 2).reverse
List(1, 2, 3, 4, 5).reverse
println("takes")
List(1, 2, 3, 4, 5).take(0)
List(1, 2, 3, 4, 5).take(1)
List(1, 2, 3, 4, 5).take(4)
List(1, 2, 3, 4, 5).take(5)
List(1, 2, 3, 4, 5).take(10)
println("init")
List(1, 2, 3, 4, 5).init
println("append")
List.append( List(1,2,3), List(4,5,6))
List.appendInTermsOfFoldRight( List(1,2,3), List(4,5,6) )
List.appendInTermsOfFoldLeft( List(1,2,3), List(4,5,6) )