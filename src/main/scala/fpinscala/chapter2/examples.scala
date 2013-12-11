package fpinscala.chapter2

/**
 * Created with IntelliJ IDEA.
 * User: billa
 * Date: 12/6/13
 * Time: 6:59 AM
 * To change this template use File | Settings | File Templates.
 */
class examples {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a=> b=>f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C  = (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
