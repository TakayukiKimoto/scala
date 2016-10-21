/**
  * Created by user on 2016/07/31.
  */
package my.either

sealed trait MyEither[+E, +A] {
  //Exercise4.6
  def map[B](f: A => B): MyEither[E, B] = this match {case Left(x) => Left(x) case Right(x) => Right(f(x))}
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {case Left(x) => Left(x) case Right(x) => f(x)}
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {case Left(_) => b case Right(_) => this}
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = this.flatMap(aa => b.map(bb => f(aa, bb)))

}
case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]


object MyEither{
  //MyOptionで作成したmean関数を
  //今度はエラーが発生したらStringでエラー内容を返すようにする
  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] = {
    if(xs.isEmpty)
      Left("mean of empty List")
    else
      Right(xs.sum / xs.length)
  }

  //エラーコードを返したい場合はこのようにする
  def safeDiv(x: Int, y: Int): MyEither[Exception, Double] = {
    try Right(x / y)
    catch{case e: Exception => Left(e)}
  }

  //MyOptionで行ったように、
  //例外が発生した場合、共通した処理を行うTry関数も実装できます
  def Try[A](f: => A): MyEither[Exception, A] = {
    try Right(f)
    catch{case e: Exception => Left(e)}
  }

  //exercise4.6の関数を定義すると(主にmap/flatMap)、
  //以下に示すようにMyEither内でfor内包表記を使用できるようになる事に注目してください
  def parseInsuranceRateQuote(age: String, numberOfSpeedTicket: String): MyEither[Exception, Double] = {
    for{
      a <- Try{age.toInt}
      ticket <- Try{numberOfSpeedTicket.toInt}
    } yield my.option.MyOption.insuranceRateQuote(a, ticket)
  }

  //map2の応用例
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  def mkName(name: String): MyEither[String, Name] = {
    if(name == "" || name == null)
      Left("Name is empty.")
    else
      Right(new Name(name))
  }
  def mkAge(age: Int): MyEither[String, Age] = {
    if(age <= 0)
      Left("Age is out of range")
    else
      Right(new Age(age))
  }
  def mkPerson(name: String, age: Int): MyEither[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_, _))
  }

  //Exercise4.7
  /*def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
    case scala.collection.immutable.Nil => Right(scala.collection.immutable.Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
  }*/
  /*def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    es.foldRight(Right(scala.collection.immutable.Nil): MyEither[E, List[A]])((h, t) => h.map2(t)(_ :: _))*/
  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = traverse(es)(x => x)
  /*def traverse[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = es match {
    case scala.collection.immutable.Nil => Right(scala.collection.immutable.Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }*/
  def traverse[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    es.foldRight(Right(scala.collection.immutable.Nil): MyEither[E, List[B]])((h, t) => f(h).map2(t)(_ :: _))

  //Exercise4.8
  sealed trait Partial[+E, +A]
  case class Errors[+E](value: Seq[E]) extends Partial[E, Nothing]
  case class Success[+A](value: A) extends Partial[Nothing, A]


}