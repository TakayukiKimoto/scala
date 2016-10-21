/**
  * Created by user on 2016/07/31.
  */
package my.stream

sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  //Exercise5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  //Exercise5.2
  def take(n: Int): MyStream[A] = this match{
    case Cons(h, t) if(n > 1) => MyStream.cons(h(), t().take(n - 1))
    case Cons(h, _) => MyStream.cons(h(), MyStream.empty)
    case _ => MyStream.empty
  }
  def drop(n: Int): MyStream[A] = this match {
    case Cons(_, t) if(n > 1) => t().drop(n - 1)
    case Cons(_, t) => t()
    case _ => MyStream.empty
  }

  //Exercise5.3
  def takeWhile_1(f: A => Boolean): MyStream[A] = this match {
    case Cons(h, t) => if(f(h())) MyStream.cons(h(), t().takeWhile_1(f)) else MyStream.empty
    case _ => MyStream.empty
  }

  // || の第二引数が非正格である点に注目
  //この指定によって、第一引数でtrueとなった場合、第二引数は評価されずに済む
  def exists_1(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().exists_1(f)
    case _ => false
  }

  //foldRight形式の汎用的な機能によってexistsを実装
  //foldRightのfに注目
  //「f: (A, B) => B」ではなく「f: (A, => B) => B」となっている
  //これによってfoldRightのfの第二引数は、必要になるまで評価されなくなる
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def exists(f: A => Boolean): Boolean = this.foldRight(false)((a, b) => f(a) || b)

  //Exercise5.4
  def forAll(f: A => Boolean): Boolean = this.foldRight(true)(f(_) && _ ) //f(_) && _ => (a, b) => f(a) && b

  //Exercise5.5
  def takeWhile(f: A => Boolean): MyStream[A] = this.foldRight(MyStream.empty[A])((h, t) => if(f(h)) MyStream.cons(h, t) else MyStream.empty)

  //Exercise5.6
  def headOption_1: Option[A] = this.foldRight(None: Option[A])((h, _) => Some(h))

  //Exercise5.7
  def map[B](p: A => B): MyStream[B] = this.foldRight(MyStream.empty[B])((h, t) => MyStream.cons(p(h), t))
  def filter(p: A => Boolean): MyStream[A] = this.foldRight(MyStream.empty[A])((h, t) => if(p(h)) MyStream.cons(h, t) else t)
  def append[B >: A](s: => MyStream[B]): MyStream[B] = this.foldRight(s)((h, t) => MyStream.cons(h, t))
  def flatMap[B](p: A => MyStream[B]): MyStream[B] = this.foldRight(MyStream.empty[B])((h, t) => p(h).append(t))

  //述語に一致した最初の要素を返すfind関数に、filterメソッドが再利用できる
  //filterはストリーム全体を変換するが、findはマッチするものを検出した時点で終了します
  def find(f: A => Boolean): Option[A] = this.filter(f).headOption

  //Exercise5.8
  //def constant[A](a: A): MyStream[A] = MyStream.cons(a, constant(a))
  def constant[A](a: A): MyStream[A] = {
    lazy val tail: MyStream[A] = Cons(() => a, () => tail)
    tail
  }

  //Exercise5.9
  def from(n: Int): MyStream[Int] = MyStream.cons(n, from(n + 1))

  //Exercise5.10
  private def fibsloop(n: Int, m: Int): MyStream[Int] = MyStream.cons(n, fibsloop(m, n + m))
  def fibs: MyStream[Int] = fibsloop(0, 1)

  //Exercise5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match {
    case Some(x) => MyStream.cons(x._1, unfold(x._2)(f))
    case _ => MyStream.empty
  }

  //Exercise5.12
  //fibs
  def fibs_1: MyStream[Int] = unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
  //from
  def from_1(n: Int): MyStream[Int] = unfold(n)(x => Some(x, x + 1))
  //constant
  def constant_1[A](a: A): MyStream[A] = unfold(a)(x => Some(x, x))
  //ones
  def ones_1[Int](n: Int): MyStream[Int] = constant_1(n)

  //Exercise5.13
  //map
  /*def map[B](f: A => B): MyStream[B] = unfold(this)(s =>
    s.headOption.map(y => (f(y), s.drop(1)))
  )*/
  def map_1[B](f: A => B): MyStream[B] = unfold(this)(s => s match {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  })
  //take
  /*def take(n: Int): MyStream[A] = unfold((this, n))(s =>
    if(s._2 > 0)
      s._1.headOption.map(y => (y, (s._1.drop(1), s._2 - 1)))
    else
      None
  )*/
  def take_1(n: Int): MyStream[A] = unfold((this, n))(s => s match {
    case (Cons(h, t), n) => Some(h(), (t(), n - 1))
    case _ => None
  })
  //takeWhile
  /*def takeWhile(f: A => Boolean): MyStream[A] = unfold(this)(s =>
    s.filter(f).headOption.map(y => (y, s.drop(1)))
  )*/
  def takeWhile_2(f: A => Boolean): MyStream[A] = unfold(this)(s => s match {
    case Cons(h, t) if(f(h())) => Some(h(), t())
    case _ => None
  })
  //zipWith
  def zipWith[B, C](b: MyStream[B])(f: (A, B) => C): MyStream[C] = unfold((this, b))(s => s match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  })
  def zip[B](b: MyStream[B]): MyStream[(A, B)] = zipWith(b)((_, _)) //Spetial Test
  //zipAll (zipWithと似た関数。両方のストリームが尽きるまで再帰を続ける。ストリームの終了はOptionで判定する)
  def zipAll[B, C](b: MyStream[B])(f: (Option[A], Option[B]) => C): MyStream[C] = unfold((this, b))(s => s match {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), MyStream.empty[B])) //Some(f(Some(h()), None), (t(), MyStream.empty[B])) の意味
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (MyStream.empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
  })

  //Exercise5.13.5
  def hasSequence[A](b: MyStream[A]): Boolean = {
    if(this == Empty) this == b
    else {
      unfold((this, b))(x => x match {
        case (Empty, Empty) => None
        case (Empty, _) => Some(false, (Empty, Empty))
        case (_, Empty) => Some(true, (Empty, Empty))
        case (Cons(h1, t1), Cons(h2, t2)) => if(h1() == h2()) Some(true, (t1(), t2())) else Some(false, (Empty, Empty))
      }).forAll(a => a) | this.drop(1).hasSequence(b)
    }
  }
  def test1[A](b: MyStream[A]): MyStream[Boolean] = {
    unfold((this, b))(x => x match{
      case (Empty, Empty) => None
      case (Empty, _) => Some(false, (Empty, Empty))
      case (_, Empty) => Some(true, (Empty, Empty))
      case (Cons(h1, t1),  Cons(h2, t2)) => if(h1() == h2()) Some(true, (t1(), t2())) else Some(false, (Empty, Empty))
    })
  }
  def hasSequence_1[A](s: MyStream[A]): Boolean = tails.exists_1(_ startWith(s))

  //Exercise5.14
  def startWith[A](s: MyStream[A]): Boolean = {
    if(this == Empty) this == s
    else {
      unfold((this, s))(x => x match{
        case (Empty, Empty) => None
        case (Empty, _) => Some(false, (Empty, Empty))
        case (_, Empty) => Some(true, (Empty, Empty))
        case (Cons(h1, t1), Cons(h2, t2)) => if(h1() == h2()) Some(true, (t1(), t2())) else Some(false, (Empty, Empty))
      }).forAll(x => x)
    }
  }

  //Exercise5.15
  def tails: MyStream[MyStream[A]] = unfold(this)(x => x match {
    case Cons(_, t) => Some(x, t())
    case _ => None
  })

  //Exercise5.16
  //unfoldを使った方法
  /*def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] = unfold(this)(s => s match {
    case Cons(_, t) => Some(s.foldRight(z)(f), t())
    case _ => None
  })*/
  def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] = foldRight((z, MyStream(z)))((a, p0) => {
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, MyStream.cons(b2, p1._2))
  })._2

}

case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] //名前がMyListと一緒になるのでConstに変更

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] = {
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  //無限ストリーム
  // *ただし、この書き方の場合、ones変数は前方定義となるので、ローカル変数であってはならない(コンパイルエラーとなる)
  //  なので、この位置(MyStream Object)に記述する
  val ones: MyStream[Int] = cons(1, ones)

}