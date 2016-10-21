/**
  * Created by user on 2016/07/18.
  */
package my.option

sealed trait MyOption[+A]{
  //必要な機能は可能な限りtraitに含める。

  //Exercise4.1
  def map[B](f: A => B): MyOption[B] = this match {case None => None case Some(x) => Some(f(x))}
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this.map(f).getOrElse(None)
  //def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {case None => None case Some(x) => f(x)}
  def getOrElse[B >: A](default: => B): B = this match {case Some(x) => x case _ => default}
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this.map(Some(_)).getOrElse(ob)
  //def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {case None => ob case _ => this}
  def filter(f: A => Boolean): MyOption[A] = this.flatMap(x => if(f(x)) Some(x) else None)
  //def filter(f: A => Boolean): MyOption[A] = this match{case Some(x) if(f(x)) => this case _ => None}
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption{
  //例外をスローする関数。(例外を試してみる)
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try{
      val x = 42 + 5
      x + y
    }catch{
      case e: Exception => 43
    }
  }

  //tryブロック内で例外が発生するため、結果が異なるようになる
  def failingFn2(i: Int): Int = {
    try{
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }catch{
      case e: Exception => 43
    }
  }

  //例外に変わる手法
  //まずは通常のエラー処理的な書き方。throw以外にも、0.0を返したり、エラーと定義した値を返したり、
  //nullに相当する値を返したりと、やり方はいくらでもある。
  //が、関数型ではそれらを使用しないでエラー処理することとなる。
  def mean_1(xs: Seq[Double]): Double = {
    if(xs.isEmpty) throw new Exception("mean of empty list!")
    else xs.sum / xs.length
  }

  //2つ目の方法は、入力の処理方法がわからない場合、どのように処理するかを引数で渡させる事とする
  //これにより完全な関数とはなるが、これには欠点もある
  //呼び出し元が未定義の対処方法を知っていなければならず、呼び出し元がDoubleを返す事しかできなくなる
  def mean_2(xs: Seq[Double], onEmpty: Double): Double = {
    if(xs.isEmpty) onEmpty
    else xs.sum / xs.length
  }

  //MyOptionを使用したエラー処理(mean)
  def mean(xs: Seq[Double]): MyOption[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  //Option基本関数を使用するシナリオ
  case class Employee(name: String, department: String)
  def lookupByName(name: String): MyOption[Employee] = {
    if(name == "Joe"){
      Some(Employee(name, "JoeDepartment"))
    }else{
      None
    }
  }

  //MyOptionの合成
  //既存の関数をMyOption型で使用する場合、既存の関数を変更する必要はない。
  //このように、既存の関数をリフトする事でMyOption型の関数に変換できる
  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f //_はMyOption[A]を表している。


  //自動車保険のWebサイトのロジックを例として見る
  //2つの主要項目から年間自動車保険量を計算する極秘関数式
  def insuranceRateQuote(age: Int, numberOfSpeedingTicket: Int): Double = {
    1.0 //とりあえず1.0を返却しておく
  }
  //Web画面からの送信では、値はStringで送られてくる。
  //これを数値に変換しなくてはならないが、その際にエラーが発生する可能性がある
  //そこで、以下のようにMyOptionを上手く利用します
  def Try[A](a: => A): MyOption[A] = {
    try Some(a) //a = [エラーが発生する可能性のある処理]
    catch {case e: Exception => None} //Exceptionが発生したらNoneにする
  }
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTicket: String): MyOption[Double] = {
    val optAge: MyOption[Int] = Try(age.toInt)
    val optTicket: MyOption[Int] = Try{numberOfSpeedingTicket.toInt} //引数一個の場合は、{}でもOK
    //insuranceRateQuote(optAge, optTicket) //このままではエラーが発生する。Exercise4.3のmap2を利用して、既存関数をMyOption型に変換する
    map2(optAge, optTicket)(insuranceRateQuote)
  }

  //失敗する可能性のある関数を使ってリストをマッピングし、
  //リストのいずれかの要素に適用した結果、Noneが返された場合は、Noneを返したい場合があります。
  //例えばString型からなるリストをMyOption[Int]として解析したい場合はどうでしょう
  //その場合、単にmapの結果をsequenceするという方法があります。
  def parseInts(a: List[String]): MyOption[List[Int]] = sequence(a.map(i => Try{i.toInt}))

  //for内包表記
  //map2をfor内包表記で実装すると、以下のうようになる
  def map2f[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    for{
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }



  //Exercise4.2
  /*def variance(xs: Seq[Double]): MyOption[Double] = {
    if(xs.length <= 0) None
    val ave: Double = xs.sum / xs.length
    val tmplis: Seq[Double] = xs.map(x => math.pow(x - ave, 2))
    Some(tmplis.max / tmplis.length)
  }*/
  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //Exercise4.3
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  //Exercise4.4
  //def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] = Try{xs.map(_.getOrElse(throw new Exception))} //違った。。。確かに無理やりException発生させてたし
  def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] = xs match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
  }
  def sequence_1[A](xs: List[MyOption[A]]): MyOption[List[A]] = xs.foldRight(Some(Nil): MyOption[List[A]])((x, y) => map2(x, y)(_ :: _))

  //Exercise4.5
  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))((x, y) => x :: y)
  }
  def traverse_1[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    a.foldRight(Some(Nil): MyOption[List[B]])((x, y) => map2(f(x), y)((xx, yy) => xx :: yy))

}