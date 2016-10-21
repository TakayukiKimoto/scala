package my.list

import scala.annotation.tailrec

/**
  * Created by user on 2016/07/07.
  * リスト系のデータ処理を関数型言語で行う場合
  * 単方向リスト
  *
  * head = データ
  * tail = 次のデータへのポインタみたいなもの
  */
sealed trait MyList[+A] //+はAが共変(covariant)である事を示す http://hogepiyo.hatenablog.jp/entry/2015/05/31/225103
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] //Consはconstructorの略

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  //引数が可変長の時の構文(いわゆるMyList("a","b","c","d")みたいな呼び出し方でMyListを作成出来るようになる)
  def apply[A](as: A*): MyList[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  @tailrec
  def dropWhile2[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Cons(x, xs) if(f(x)) => dropWhile2(xs)(f)
    case _ => as
  }

  //リストの再帰と高階関数の一般化
  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum2(as: MyList[Int]): Int = foldRight(as, 0)((x, y) => x + y)
  def product2(as: MyList[Double]): Double = foldRight(as, 1.0)(_ * _) //これは(x, y) => x * y の簡易記法

  //Exercise3.2
  def tail[A](as: MyList[A]): MyList[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  //Exercise3.3
  def setHead[A](head: A, as: MyList[A]): MyList[A] = as match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }

  //Exercise3.4
  @tailrec
  def drop[A](as: MyList[A], n: Int): MyList[A] = as match {
    case Cons(_, xs) if(n > 0) => drop(xs, n - 1)
    case _ => as
  }

  //Exercise3.5
  @tailrec
  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = as match{
    case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
    case _ => as
  }

  //Exercise3.6
  //処理時間がO(1)にならない理由 → 単方向リストである為、リストの先頭から末尾に掛けて処理を実施する必要があるO
  // *O(n)となる
  def init[A](as: MyList[A]): MyList[A] = as match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //Exercise3.9
  def length[A](as: MyList[A]): Int = foldRight(as, 0)((x, y) => y + 1)

  //Exercise3.10
  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match{
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //Exercise3.11
  def sum3(as: MyList[Int]): Int = foldLeft(as, 0)((y, x) => x + y)
  def product3(as: MyList[Double]): Double = foldLeft(as, 1.0)((y, x) => x * y)
  def length2[A](as: MyList[A]): Int = foldLeft(as, 0)((y, x) => y + 1)

  //Exercise3.12
  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil: MyList[A])((y, x) => Cons(x, y))

  //Exercise3.13
  def foldRightViaLeft_1[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a)) //一旦、リストをreverseにしてからfoldLeftする
  def foldRightViaLeft_2[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b:B) => b)((g, a) => b => g(f(a, b)))(z)
  def foldLeftViaRight_1[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = foldRight(l, (b:B) => b)((a, g) => b => g(f(b, a)))(z)
  def foldLeftViaFoldRight_1[A,B](as: MyList[A], outerIdent: B)(combiner: (B, A) => B): B = {
    // foldLeft processes items in the reverse order from foldRight.  It's
    // cheating to use reverse() here because that's implemented in terms of
    // foldLeft!  Instead, wrap each operation in a simple identity function to
    // delay evaluation until later and stack (nest) the functions so that the
    // order of application can be reversed.  We'll call the type of this
    // particular identity/delay function BtoB so we aren't writing B => B
    // everywhere:
    type BtoB = B => B

    // Here we declare a simple instance of BtoB according to the above
    // description.  This function will be the identity value for the inner
    // foldRight.
    def innerIdent:BtoB = (b:B) => b

    // For each item in the 'as' list (the 'a' parameter below), make a new
    // delay function which will use the combiner function (passed in above)
    // when it is evaluated later.  Each new function becomes the input to the
    // previous function (delayFunc).
    //
    //                        This much is just the type signature
    //                  ,-------^-------.
    def combinerDelayer:(A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b:B) => delayFunc(combiner(b, a))
    // `----------v---------'    `----------------v---------------'
    //         Paramaters                 The returned function

    // Pass the original list 'as', plus the simple identity function and the
    // new combinerDelayer to foldRight.  This will create the functions for
    // delayed evaluation with an combiner inside each one, but will not
    // apply any of those functions.
    def go:BtoB = foldRight(as, innerIdent)(combinerDelayer)

    // This forces all the evaluations to take place
    go(outerIdent)
  }

  //Exercise3.14
  def append2[A](l1: MyList[A], l2:MyList[A]): MyList[A] = foldRight(l1, l2)((x, y) => Cons(x, y))

  //Exercise3.15
  def concat[A](as: MyList[MyList[A]]): MyList[A] = foldRight(as, Nil: MyList[A])(append2)

  //Exercise3.16
  def intAddOne(as: MyList[Int]): MyList[Int] = foldRight(as, Nil: MyList[Int])((x, y) => Cons(x + 1, y))

  //Exercise3.17
  def doubleToStr(as: MyList[Double]): MyList[String] = foldRight(as, Nil: MyList[String])((x, y) => Cons(x.toString + "d to Str!!", y))

  //Exercise3.18
  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = foldRight(as, Nil: MyList[B])((x, y) => Cons(f(x), y))

  //Exercise3.19
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = foldRight(as, Nil: MyList[A])((x, y) => if(f(x)) Cons(x, y) else y)

  //Exercise3.20
  //def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = foldRight(as, Nil: MyList[B])((x, y) => append2(f(x), y))
  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = concat(map(as)(f))

  //Exercise3.21
  def filter2[A](as: MyList[A])(f: A => Boolean): MyList[A] = flatMap(as)(x => if(f(x)) MyList(x) else Nil)

  //Exercise3.22
  /*def zipPairwize(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Nil) => Cons(x, xs)
    case (Nil, Cons(y, ys)) => Cons(y, ys)
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipPairwize(xs, ys))
  }*/
  def zipPairwize(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipPairwize(xs, ys))
  }

  //Exercise3.23
  /*def zipWith[A, B](l1: MyList[A], l2: MyList[A], z: A)(f: (A, A) => B): MyList[B] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Nil) => Cons(f(x, z), zipWith(xs, Nil, z)(f))
    case (Nil, Cons(y, ys)) => Cons(f(z, y), zipWith(Nil, ys, z)(f))
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys, z)(f))
  }*/
  def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  //Exercise3.24
  @tailrec
  def hasSubsequence[A](as: MyList[A], target: MyList[A]): Boolean = {
    @tailrec
    def loop(l1: MyList[A], l2: MyList[A]): Boolean = (l1, l2) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if(x == y) => loop(xs, ys)
      case _ => false
    }

    as match {
      case Nil => as == Nil
      case Cons(_, xs) => if(loop(as, target)) true else hasSubsequence(xs, target)
      case _ => false
    }
  }
}
