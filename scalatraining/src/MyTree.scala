/**
  * Created by user on 2016/07/17.
  */
sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree{
  //Exercise3.25
  def size[A](tr: MyTree[A]): Int = tr match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  //Exercise3.26
  def maximum(tr: MyTree[Int]): Int = tr match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //Exercise3.27
  def depth[A](tr: MyTree[A]): Int = tr match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //Exercise3.28
  def map[A, B](tr: MyTree[A])(f: A => B): MyTree[B] = tr match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise3.29
  def fold[A, B](tr: MyTree[A])(f: A => B)(g: (B, B) => B): B = tr match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def size2[A](tr: MyTree[A]): Int = fold(tr)(x => 1)((x, y) => x + y + 1)
  def maximum2(tr: MyTree[Int]): Int = fold(tr)(x => x)((x, y) => x max y)
  def depth2[A](tr: MyTree[A]): Int = fold(tr)(x => 1)((x, y) => 1 + (x max y))
  def map2[A, B](tr: MyTree[A])(f: A => B): MyTree[B] = fold(tr)(x => Leaf(f(x)): MyTree[B])((x, y) => Branch(x, y))

}