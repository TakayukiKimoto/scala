import scala.annotation.tailrec
import my.list.MyList
import my.option.MyOption
import my.either.MyEither
import my.stream.{Cons, MyStream}

/**
  * Created by user on 2016/07/04.
  */
object TestScala1 {
  def main(args: Array[String]): Unit = {
    //Scala構文
    println(formatAbs(-42))

    //関数型のループ
    println(formatFactrial(5))
    println(fib(32)) //exercise2.1

    //高階関数
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 6, factorial))

    //多相関数
    println(findFirstStr(Array("aa","eiab","pibk","2wia","bbike","aaib","aa","eiab"), "eiab"))
    println(findFirst[Int](Array(1,3,20,10,232,47,8,89,92,64), (n: Int) => n == 232))
    val sortedArray = Array(1,2,5,6,9,10,28,48,62,90,102)
    println(isSorted[Int](sortedArray, (n: Int, m: Int) => n <= m)) //exercise2.2

    //関数リテラル
    val lessThen = new Function2[Int, Int, Boolean] {
      def apply(n:Int, m:Int) = n <= m
    }
    println(isSorted[Int](sortedArray, lessThen))

    //関数に従う実装 (引数の部分適用)
    def mysum(n: Int, m: Int): Int = n + m
    val fpar1 = partial1[Int, Int, Int](1, mysum)
    println(fpar1(2))

    //curry (部分適用と非常に似ているが別のもの。)
    //詳細とかは　http://qiita.com/KDKTN/items/6a27c0e8efa66b1f7799
    val fcurry1 = curry[Int, Int, Int](mysum) //exercise2.3
    val fcurry2 = fcurry1(3)
    val fcurry3 = fcurry2(4)
    println(fcurry3)
    println(fcurry1(5)(6))
    println(fcurry2(7))

    //uncurry (カリー化された関数から、実際の処理をする関数を抜き出すイメージ)
    val funcurry1 = uncurry[Int, Int, Int](fcurry1) //exercise2.4
    println(funcurry1(8, 9))

    //関数合成
    val fcompose = compose[Int, Int, String]((n: Int) => "String Value factorial %d".format(n), factorial)
    println(fcompose(7)) //exercise2.5

    //データ処理(リスト)
    val ex1: MyList[Double] = my.list.Nil //空のリスト
    val ex2: MyList[Int] = my.list.Cons(3, my.list.Nil) //1つの要素
    val ex3: MyList[String] = my.list.Cons("aa", my.list.Cons("bbb", my.list.Nil)) //2つの要素
    println(MyList.sum(ex2))
    println(MyList.product(ex1))
    println(exercise3_1()) //exercise3.1
    val mylist1 = MyList('a','b','c','d','e','f','g')
    println(MyList.tail(mylist1)) //exercise3.2
    val mylist2 = MyList.setHead('b', mylist1)
    println(mylist2) //exercise3.3
    val mylist3 = MyList.drop(mylist2, 3)
    println(mylist3) //exercise3.4
    val mylist4 = MyList.dropWhile(mylist2, (a: Char) => a != 'f')
    println(mylist4) //exercise3.5
    val mylist5 = MyList.dropWhile(MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (x: Int) => x <= 5)
    println(mylist5) //exercise3.6
    println(MyList.dropWhile2(MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(x => x <= 5)) //カリー化によって型推論が可能に
    val mylist6 = MyList.length(mylist1)
    println(mylist6) //exercise3.9
    println(MyList.sum3(MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))) //exercise3.11
    println(MyList.product3(MyList(2.5, 3.17, 1.0, 184.1, 8.82))) //exercise3.11
    println(MyList.length2(MyList("ajf", "ajieaf", "13u8g", "ajifeaj1", "aji2"))) //exercise3.11
    val mylist7 = MyList.reverse(MyList(1, 2, 3, 4, 5, 6))
    println(mylist7) //exercise3.12
    val mylist8 = MyList.append2(mylist7, mylist5)
    println(mylist8) //exercise3.14
    val mylist9 = MyList.concat(MyList(MyList(1, 2, 3), MyList(5, 6, 7), MyList(9, 10, 11)))
    println(mylist9) //exercise3.15
    val mylist10 = MyList.intAddOne(MyList(1, 2, 3, 4, 5))
    println(mylist10) //exercise3.16
    val mylist11 = MyList.doubleToStr(MyList(3.21, 8.28, 7.18))
    println(mylist11) //exercise3.17
    val mylist12 = MyList.map(mylist10)(x => x - 1)
    println(mylist12) //exercise3.18
    val mylist13 = MyList.filter(mylist9)(x => x % 2 == 0)
    println(mylist13) //exercise3.19
    val mylist14 = MyList.flatMap(MyList(1, 2, 3))(x => MyList(x, x))
    println(mylist14) //exercise3.20
    val mylist15 = MyList.filter2(mylist14)(x => x >= 2)
    println(mylist15) //exercise3.21
    val mylist16 = MyList.zipPairwize(MyList(1, 2, 3), MyList(4, 5, 6))
    println(mylist16) //exercise3.22
    val mylist17 = MyList.zipWith(MyList(1, 2, 3, 4, 5), MyList(4, 5, 6))((x, y) => x - y)
    println(mylist17) //exercise3.23
    println(MyList.hasSubsequence(MyList(1, 1, 1, 2, 3, 4), MyList(4, 5))) //exercise3.24

    //データ処理(ツリー)
    val mytree1 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(8), Leaf(7))))
    println(MyTree.size(mytree1)) //exercise3.25
    println(MyTree.maximum(mytree1)) //exercise3.26
    val mytree2 = Branch(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4)), Leaf(5))
    println(MyTree.depth(mytree2)) //exercise3.27
    val mytree3 = MyTree.map(mytree1)(x => x.toString + "i to str!!")
    println(mytree3)
    println(MyTree.size2(mytree1)) //exercise3.28
    println(MyTree.maximum2(mytree2)) //exercise3.28
    println(MyTree.depth2(mytree3)) //exercise3.28
    println(MyTree.map2(mytree2)(x => x.toDouble * 1.17)) //exercise3.28

    //エラー処理
    //Option
    //println(MyOption.failingFn(12)) //エラーが発生する。コメントアウト
    println(MyOption.failingFn2(12))
    val joeDepartment: MyOption[String] = MyOption.lookupByName("Joe").map(_.department) //Joeのdepartmentが取得できる
    val maiDepartment: MyOption[String] = MyOption.lookupByName("Mai").map(_.department)
    println(joeDepartment)
    println(maiDepartment)
    val dept: String = MyOption.lookupByName("Joe").map(_.department).filter(_ == "Accounting").getOrElse("Default Dept") //一般的なエラー処理の手順。map / filter / getOrElseを順番に使う
    println(dept)
    val factO: MyOption[Int] = MyOption.lift(factorial)(my.option.Some(5)) //既存の関数をMyOptionに変換
    println(factO)
    val insuranceRate: MyOption[Double] = MyOption.parseInsuranceRateQuote("112352", "82914")
    println(insuranceRate)
    val parseInts1: MyOption[List[Int]] = MyOption.parseInts(List("1", "2", "3", "4", "5"))
    println(parseInts1)
    val parseInts2: MyOption[List[Int]] = MyOption.traverse(List("11", "aa", "33", "44", "55"))(x => MyOption.Try{x.toInt})
    println(parseInts2)
    //Either
    val parseInts3: MyEither[Exception, List[Int]] = MyEither.traverse(List("11", "aa", "33", "44", "55"))(x => MyEither.Try{x.toInt})
    println(parseInts3)
    val person1: MyEither[String, MyEither.Person] = MyEither.mkPerson(null, 0)
    println(person1)

    //正確と遅延
    println(List(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)) //この式では中間リストが生成される為、効率が良くない
    false && {println("!!"); true} //何も出力しない
    true || {println("!!"); true} //何も出力しない
    if2(true, () => println("a"), () => println("b"))
    val maybeTwi: Int = maybeTwice(true, {println("hi"); 1 + 41})
    println(maybeTwi)
    val maybeTwi2: Int = maybeTwice2(true, {println("hi"); 1 + 41})
    println(maybeTwi2)
    //MyStreamのプログラムトレース
    val mystream1 = MyStream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    //上記のプログラムは以下のように処理される
    //Cons(11, MyStream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
    //MyStream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    //Cons(12, MyStream(3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
    //12 :: MyStream(3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    //12 :: Cons(13, MyStream(4).map(_ + 10)).filter(_ % 2 == 0).toList
    //12 :: MyStream(4).map(_ + 10).filter(_ % 2 == 0).toList
    //12 :: Cons(14, MyStream().map(_ + 10)).filter(_ % 2 == 0).toList
    //12 :: 14 :: MyStream().map(_ + 10).filter(_ % 2 == 0).toList
    //12 :: 14 :: List()
    //filterとmapが交互に実行されているのが分かる。これによって、mapの処理結果を
    //中間ストリームとして完全にインスタンス化しない
    println(mystream1)
    println(MyStream(1, 2, 3, 4)) //戻り値は「cons(as.head, apply(as.tail: _*))」
    println(MyStream(1, 2, 3, 4).map(_ + 10)) //foldRightの「f: (A, => B) => B」のAは値渡しなので、fの呼び出し前に評価され、初めてh「as.head」が評価される。しかし、それ以上hは評価されない
                                              //戻り値は「Cons(() => p(1), () => apply(as.tail: _*).foldRight(z)(f))」
    println(MyStream(1, 2, 3, 4).filter(_ % 2 == 0)) //filter一回目のチェックはfalseなので、t(foldRightの「=> B」引数)が評価され、
                                                     //初めてapply(as.tail: _*)　(正しくは「apply(as.tail: _*).foldRight(z)(f)」） が評価される。
                                                     //二回目のチェックはtrueなので、MyStream.consが呼ばれ、それ以上tは評価されない
                                                     //戻り値は「Cons(() => 2, () => apply(as.tail: _*).foldRight(z)(f))」
    //無限ストリーム
    println(MyStream.ones.take(5).toList)
    println(MyStream.ones.map(_ + 1).exists_1(_ % 2 == 0))
    println(MyStream.ones.takeWhile_1(_ == 1))
    println(MyStream.ones.forAll(_ != 1))
    //MyStreamでhasSequence
    val checkSeqStream = MyStream(0, 1, 1, 2, 3, 1, 4, 10)
    val test1 = checkSeqStream.drop(3).test1(MyStream(2, 3, 1))
    println(test1.forAll(x => x))
    val scanRight = MyStream(1, 2, 3).scanRight(0)(_ + _)
    println(scanRight.toList)
    //scanRight
    val tails = MyStream(1, 2, 3).scanRight(MyStream.empty[Int])((x, y) => MyStream.cons(x, y))
    val hasSequence = tails.exists_1(_ startWith(MyStream(1, 2)))
    println(hasSequence)

    //scalaで状態の管理
    val rng = SimpleMyRNG(-2)
    val (i1, rng1) = rng.nonNegativeInt(rng)
    println(i1.toDouble)

  }

  //ここからScalaの構文基礎
  def abs(n: Int): Int = {
    if(n < 0) -n
    else n
  }
  private def formatAbs(n: Int): String = {
    val msg = "The absolute value of %d of %d"
    msg.format(n, abs(n))
  }

  //ここから関数によるループ
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if(n <= 1) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }
  private def formatFactrial(n: Int): String = {
    val msg = "The factorial value of %d of %d"
    msg.format(n, factorial(n))
  }

  //ここから高階関数
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  //ここから多相関数
  def findFirstStr(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) -1
      else if(ss(n) == key) n
      else loop(n+1)
    }
    loop(0)
  }
  def findFirst[A](as: Array[A], f: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if(n >= as.length) -1
      else if(f(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  //ここから関数に従う実装
  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = {
    //(b: B) => f(a, b)
    b => f(a, b)
  }

  //ここから高階関数の型推論の改善
  //MyListに記載

  //リストの再帰と高階関数の一般化
  //MyListに記載
  //MyTreeに記述

  //ここからエラー処理
  //MyOptionに記載
  //MyEitherに記述

  //ここから正確と遅延
  //if分は非正確な処理(遅延評価)です
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = { // () => A、は引数なしでAを返す関数
    if(cond) onTrue() else onFalse()
  }
  //Scalaでは、引数の評価結果をデフォルトではキャッシュしません
  //この場合、iの関数が2回評価されます
  def maybeTwice(b: Boolean, i: => Int) = if(b) i + i else 0
  //lazyキーワードを追加すると、そのlazy宣言の右辺の評価が最初に参照される時まで先送りされます
  //また、その後の参照で評価が繰り返されないよう、結果がキャッシュされます
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if(b) j + j else 0
  }


  //ここからExercise

  //Exercise 2.1
  //n番目のフィボナッチ数を取得する再帰関数を記述
  //ローカルな末尾再帰関数を使用すること
  def fib(n: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int = {
      if(acc > n) acc
      else go(acc, x + acc)
    }
    go(0, 1)
  }

  //Exercise 2.2
  //指定された比較関数に従って、Array[A]がソートされているか判定せよ
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(idx: Int): Boolean = {
      if(idx + 1 >= as.length) true
      else if(!ordered(as(idx), as(idx + 1))) false
      else loop(idx + 1)
    }
    loop(0)
  }

  //Exercise 2.3
  //カリー化(currying)では、引数2つの関数fが、fを部分的に適用する引数1つの関数に変換される
  //この場合もコンパイルできる実装は一つだけである。
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //(a: A) => ((b: B) => f(a, b))
    a => b => f(a, b)
  }

  //Exercise 2.4
  //curryの変換を逆方向に実行するuncurryを実装せよ
  //=>は右結合である為、A => (B => C)はA => B => Cと表現できる
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    //(a: A, b: B) => f(a)(b)
    (a, b) => f(a)(b)
  }

  //Exercise 2.5
  //関数合成。2つの関数を合成する高階関数を実装せよ
  def compose[A, B, C](f: B => C, g: A => B):A => C = {
    //(a: A) => f(g(a))
    a => f(g(a))
  }

  //Exercise 3.1
  //以下のマッチ式はどのような結果になるか
  def exercise3_1(): Int = {
    MyList(1, 2, 3, 4, 5) match {
      case my.list.Cons(x, my.list.Cons(2, my.list.Cons(4, _))) => x
      case my.list.Nil => 42
      case my.list.Cons(x, my.list.Cons(y, my.list.Cons(3, my.list.Cons(4, _)))) => x + y //これが最初にマッチ
      case my.list.Cons(h, t) => h + MyList.sum(t)
      case _ => 101
    }
  }

  //Exercise3.2
  //MyListの最初の要素を削除する関数tailを実装せよ
  //この関数の実行速度が一定[O(1)]であることに注意
  //MyListがNilである場合、実装上の選択肢として他に何があるか
  // *実装はMyList Objectに記述

  //Exercise3.3
  //Exercise3.2と同じ考え方に基づいて、MyListの最初の要素を変更する
  //setHead関数を実装せよ
  // *実装はMyList Objectに記述

  //Exercise3.4
  //tail関数を一般化して、先頭からn個の要素を削除するdrop関数を作成(書き換え)せよ
  //この関数の実行時間は、削除する要素の数のみに比例[O(n)]する事に注意
  //List全体をコピーする必要はない
  // *実装はMyList Objectに記述

  //Exercise3.5
  //述語とマッチする場合に限り、MyListからその要素までを削除するdropWhileを実装せよ
  // *実装はMyList Objectに記述

  //Exercise3.6
  //MyListの末尾を除く全ての要素で構成されたMyListを返却するinit関数を実装せよ
  //MyList(1, 2, 3, 4)が渡された場合、MyList(1, 2, 3)を返却する
  //この関数がtailのように一定時間で処理できないのはなぜか
  // *実装はMyList Objectに記述

  //Exercise3.7
  //foldRightを使って記述されたproductは0.0を検出した場合、直ちに再帰を終了して0.0を返せるか
  //その理由を説明せよ
  //   → 不可。リストの最後まで再帰を行わないと、再帰の式を評価出来ない為
  //大きなリストでfoldRightを呼び出された場合の短絡の仕組みについて検討せよ
  //   → 不可。foldRight内にて、直ちに再帰処理が終了するのは、Exceptionなどで強制的に処理が終了する必要が有る（はず）
  //　　　最初の式を評価する為に、全てのリストを展開しなければならない。

  //Exercise3.8
  //foldRight(MyList(1, 2, 3), Nil: MyList[Int])(Cons(_, _))のように
  //Nil及びCons自体をfoldRightに渡したらどうなるか。
  //   → リストのコピーを作成する
  //これがfoldRightとMyListのデータコンストラクタとの関係について何を表していると思うか
  //   → Cons(1, Cons(2, Cons(3, Nil))) ・ foldRight(1, foldRight(2, foldRight(3, foldRight(Nil))))
  //     といった感じで、foldRightの再帰式はMyListのデータ構造を良く似ている

  //Exercise3.9
  //foldRightを使ってリストの長さを計算せよ
  // *実装はMyList Objecに記述

  //Exercise3.10
  //このfoldRightは末尾再帰ではなく、リストが大きい場合はStackOverFlowになってしまう
  //これをスタックセーフでは無いと言う。
  //前章で説明した手法を使って、リスト再帰の総称関数foldLeftを記述せよ
  // *実装はMyList Objectに記述

  //Exercise3.11
  //foldLeftを使って、sum / product / lengthを記述せよ
  // *実装はMyList Objectに記述

  //Exercise3.12
  //要素が逆に並んだリストを返す関数を記述せよ
  //MyList(1, 2, 3)を与えられた時、MyList(3, 2, 1)を返す
  //畳み込みを使って記述できるかどうか確認すること
  // *実装はMyList Objectに記述

  //Exercise3.13
  //難問 : foldLeftをベースにfoldRightを記述する事は可能か。その逆はどうか
  //foldLeftを使ってfoldRightを実装すると、末尾再帰的に実装する事が可能となり、
  //大きなリストでもStackOverFlowが発生しなくて便利である。
  // *実装はMyList Objectに記述
  // *答えを丸写ししたけど、かなり意味不明。何やってるか後で解析する

  //Exercise3.14
  //foldLeft or foldRightをベースとしてappendを実装せよ
  // *実装はMyList Objectに記述

  //Exercise3.15
  //難問 : 複数のリストからなるリストを1つのリストとして連結する関数を記述せよ
  //この関数の実行時間は全てのリストの長さの合計に対して線形になるはずである。
  //すでに定義された関数を使用して見る事
  // *実装はMyList Objectに記述

  //Exercise3.16
  //各要素に1を足す事で整数のリストを変換する関数を記述せよ。
  //これは新しいリストを返す純粋関数になるはずである
  // *実装はMyList Objectに記述

  //Exercise3.17
  //MyList[Double]の各値をStringに変換する関数を記述せよ
  //d.toStringというメソッドを使ってDoubleをStringに変換出来る
  // *実装はMyList Objectに記述

  //Exercise3.18
  //リストの要素を変更し、かつリストの構造をそのまま保つ総称関数mapを記述せよ
  // *実装はMyList Objectに記述

  //Exercise3.19
  //与えられた述語条件が満たされる要素以外をリストから削除するfilter関数を実装せよ
  // *実装はMyList Objectに記述

  //Exercise3.20
  //mapと同じような働きをするflatMap関数を記述せよ
  //この関数は単一の値では無くリストを返し、そのリストは最終的な結果のリストに挿入されなければならない
  //例えば、flatMap(MyList(1, 2, 3))(i => MyList(i, i))は、MyList(1, 1, 2, 2, 3, 3)になるはずである
  // *実装はMyList Objectに記述

  //Exercise3.21
  //flatMapを使ってfilterを実装せよ
  // *実装はMyList Objectに記述

  //Exercise3.22
  //リスト2つを受け取り、対応する要素同士を足しあわせて新しいリストを生成する関数を記述せよ。
  //例えば、MyList(1, 2, 3)とMyList(4, 5, 6)を渡された場合、MyList(5, 7, 9)が返る
  // *実装はMyList Objectに記述

  //Exercise3.23
  //exercise3.22で作成した関数を整数及び加算に限定されないよう、一般化せよ
  //作成した関数にはzipWithという名前を付けること
  // *実装はMyList Objectに記述

  //Exercise3.24
  //リストの問題の１つは、極めて汎用的な関数に演算やアルゴリズムを表現出来る反面、
  //結果として得られる実装が必ずしも効率的ではないことです。
  //同じ入力に対するパスを複数作成しないといけないことや、再帰ループを途中で抜ける事が出来るように記述しなければならない事があります。
  //難問 : 例として、MyListにMyListがサブシーケンスとして含まれているかどうかを判別するhasSubsequenceを実装
  //例えば、MyList(1, 2, 3, 4, 5)には、MyList(1, 2, 2) MyList(2, 3) MyList(5)等が含まれている
  // *実装はMyList Objectに記述

  //Exercise3.25
  //2分木ノード(Leaf / Branch)の数を数えるsize関数を実装せよ
  // *実装はMyTree Objectに記述

  //Exercise3.26
  //MyTree[Int]の最大の要素を返す、maximum関数を記述せよ
  //なおScalaでは、x.max(y)またはx max yで2つの整数値の最大値を計算できる
  // *実装はMyTree Objectに記述

  //Exercise3.27
  //2分木のルートから、Leafまでの最長パスを返すdepth関数を記述せよ
  // *実装はMyTree Objectに記述

  //Exercise3.28
  //2分木の各要素を特定の関数を使って変更するmap関数を記述せよ。
  //この関数はListと同じ名前のメソッドと類似している。
  // *実装はMyTree Objectに記述

  //Exercise3.29
  //size / maximum / depth / mapを一般化し、
  //これらの類似点を抽象化する関数foldを作成せよ。
  //そして、fold関数をベースにmap / maximum / depth / mapを再定義せよ
  //このfold関数とListの左畳み込み及び右畳み込みの間にある、類似性を抽出する事は可能か
  //    → ？？？
  // *実装はMyTree Objectに記述

  //Exercise4.1
  //以下の関数(map / flatMap / getOrElse / orElse / filter)の実装を書け
  //関数名から、関数の意味と、それを使用するであろう状況について考えること
  //パターンマッチングを使用しても良いが、map / getOrElse以外は使用しなくても実装できるはずである
  // *実装はMyOptionに記述

  //Exercise4.2
  //flatMapをベースとして、分散を表すvariance関数を実装せよ
  //シーケンスの平均をm、シーケンスの各要素をxとすると、
  //分散はmath.pow(x - m, 2)の平均となる
  // *実装はMyOption Objectに記述

  //Exercise4.3
  //2項関数を使ってMyOption型2つを合成する総称関数map2を作成せよ
  //引数どちらかがNoneの場合、戻り値もNoneになる
  // *実装はMyOption Objectに記述

  //Exercise4.4
  //MyOptionのリストを1つのMyOptionにまとめるsequence関数を実装せよ
  //リストの中に一つでもNoneが含まれていた場合、Noneが返却される
  //それ以外の場合は、値を含んだリストのSomeを返却する
  // *実装はMyOption Objectに記述

  //Exercise4.5
  //parseIntsでは、リストを2回走査することになるため、効率はよくありません
  //1回目は、StringをMyOption[Int]に変換し、2回目はそれらをMyOption[List[Int]]にまとめます
  //mapの結果をシーケンスしたい場合がたびたび発生することを考えると、
  //以下のシネグチャを持つ新しい総称関数を作成した方が良さそうです
  //このtraverse関数を実装せよ。リストを1回だけ走査する効率の良い実装にすること
  // *実装はMyOption Objectに記述

  //Exercise4.6
  //Right値を操作するmap / flatMap / orElse / map2を実装せよ
  // *実装はMyEitherに記述

  //Exercise4.7
  //MyEitherでsequenceとtraverseを実装せよ。
  //これらはエラーが発生した場合、最初に検出されたエラーを返すものとする
  // *実装はMyEither Objectに記述

  //Exercise4.8
  //mkPersonでは、nameとageどちらも無効であったとしても
  //１つのエラーしか報告できない。両方のエラーを報告するには何をすれば良いか
  //map2を変更するのか、mkPersonのシグネチャを変更するのか、
  //あるいは新しい構造を追加して、MyEitherよりも効果的に要件を満たす
  //データ型を作成することは可能か。
  //そのデータ型では、orElse、sequence、traverseの振る舞いはどのように変化するか
  // *実装はMyEitherに記述

  //Exercise5.1
  //MyStreamをListに変換し、それによりMyStreamを強制的に評価する
  //関数を記述せよ
  // *実装はMyStreamに記述

  //Exercise5.2
  //MyStreamの先頭からn個の要素を取り出す関数takeと、
  //先頭からn個の要素をスキップするdrop関数を実装せよ
  // *実装はMyStreamに記述

  //Exercise5.3
  //MyStreamの先頭から、指定された述語と一致する間、
  //要素を取得するtakeWhileを実装せよ
  // *実装はMyStreamに記述

  //Exercise5.4
  //MyStreamの要素のうち、指定された述語と全ての要素が一致するかチェックする
  //forAll関数を実装せよ
  //この実装では、一致しない述語が検出された場合、直ちに処理を終了しなければならない
  // *実装はMyStreamに記述

  //Exercise5.5
  //foldRightをベースにtakeWhileを実装せよ
  // *実装はMyStreamに記述

  //Exercise5.6
  //難問 : foldRightをベースにheadOptionを実装せよ
  // *実装はMyStreamに記述

  //Exercise5.7
  //foldRightをベースにmap / filter / append / flatMapを実装せよ
  //appendメソッドは、その引数に関して非正格でなければならない
  // *実装はMyStreamに記述

  //Exercise5.8
  //onesを少し一般化し、指定された値の無限ストリームを返すconstant関数を記述せよ
  // *実装はMyStreamに記述

  //Exercise5.9
  //nで始まって、n+1,n+2...と続く整数の無限ストリームを生成する関数を記述せよ
  // *実装はMyStreamに記述

  //Exercise5.10
  //フィボナッチ数列の無限ストリームを生成するfibs関数を実装せよ
  // *実装はMyStreamに記述

  //Exercise5.11
  //より汎用的なストリーム生成関数unfoldを記述せよ
  //この関数は初期状態に加えて、以下の状態と、生成されるストリームの次の値を生成する関数を受け取る
  //    def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A]
  //Optionは、MyStream生の成を終了させるタイミングを指定するのに使用されます
  //unfoldは余再帰と呼ばれる関数の一例です。
  //再帰関数がデータを消費するのに対し、余再帰はデータを生成します。
  //また、再帰関数が再帰処理によって入力を小さくして終了するのに対し、
  //余再帰関数は生産性が続く限り終了しません。unfold関数の生産性は、fが終了しない(Noneにならない)限り続きます
  // *実装はMyStreamに記述

  //Exercise5.12
  //unfoldを使ってfibs / from / constant / onesを記述せよ
  // *実装はMyStreamに記述

  //Exercise5.13
  //unfoldを使って(３章で示したような)map / take / takeWhile / zipWith / zipAllを実装せよ
  //zipAll関数は、どちらかのストリーム要素が残っている限り、評価を続ける必要がある。
  //この関数は完全に評価されたかどうかを示すのにOptionを使用する
  // *実装はMyStreamに記述

  //Exercise5.13.5
  // *問題としては挙がってないけど。。。
  //ここでhasSequence関数について改めて考えてみましょう。
  //正格なリスト処理関数を利用する場合、余分な作業を行わずにこの関数を実装するには
  //かなりややこしい巨大なループを記述する必要がありました。
  //遅延リストと、これまで作成した関数を組み合わせた場合、
  //どのように実装できるでしょうか
  // *実装はMyStreamに記述

  //Exercise5.14
  //難問 : ここまで記述してきた関数を使ってstartWithを実装せよ
  //この関数は、あるMyStreamが別のMyStreamのプレフィックスであるかどうかを調べる
  //例えば、MyStream(1, 2, 3).startWith(MyStream(1, 2))はTrueである
  // *実装はMyStreamに記述

  //Exercise5.15
  //unfoldを使ってtailsを実装せよ。与えられたMyStreamに対し、
  //tailsは元のMyStreamから始まる入力シーケンスのサフィックスであるMyStreamを返す
  //例えば、MyStream(1, 2, 3)が与えられた場合、
  //MyStream(MyStream(1, 2, 3), MyStream(2, 3), MyStream(3), MyStream())を返す
  // *実装はMyStreamに記述

  //Exercise5.16
  //難問 : tailsをscanRight関数として一般化せよ
  //foldRightと同様に、このストリームは中間ストリームを返す
  //MyStream(1, 2, 3).scanRight(_ + _).toList
  // => List(6, 5, 3, 0)
  //この関数は、List(1+2+3+0, 2+3+0, 3+0, 0)と同じ意味になるはずである
  //中間結果を再利用することで、n個の要素を持つMyStreamの評価に掛かる時間が常にnに対して
  //線形になるはずである。
  //この実装にunfoldを使うことは可能か。その場合はどうなるか。
  //実装できない場合、その理由はなぜか
  //既に記述した関数を使って実装する事は可能か
  // *実装はMyStreamに記述

  //Exercise6.1
  //MyRNG.nextIntを使って、0〜Int.MaxValue(0とInt.MaxValueを含む)のランダムな整数
  //を生成する関数を記述せよ
  //なお、nextIntがInt.MinValueを返す時には、対応する自然数が無い
  //この特異なケースにも対処する必要が有る
  // *実装はMyRNGに記述

  //Exercise6.2
  //0〜1(1を含まない)のDouble型の値を生成する関数を記述せよ
  //Int.MaxValueを使って整数の最大値を取得できることと、
  //x.toDoubleを使ってx: IntをDoubleに変換できる事に注意
  // *実装はMyRNGに記述

}
