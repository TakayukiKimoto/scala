/**
  * Created by user on 2016/08/28.
  */
sealed trait MyRNG{
  //参照透過性を取り戻す鍵は、状態の更新を明示的なものにすることです
  //状態を副作用として更新するのではなく、
  //生成された値と共に新しい状態を返すようにするのです。
  //乱数ジェネレータのインターフェイスとして考えられる1つの例としては
  //以下のような関数です
  def nextInt: (Int, MyRNG)

  def randomPair_1(rng: MyRNG): (Int, Int)
  def randomPair(rng: MyRNG): ((Int, Int), MyRNG)

  //Exercise6.1
  def nonNegativeInt(rng: MyRNG): (Int, MyRNG)

  //Exercise6.2
  def double(rng: MyRNG): (Double, MyRNG)
}

case class SimpleMyRNG(seed: Long) extends MyRNG {
  //ですが、実装が必要な事に変わりは無いので、簡単なものを実装してみましょう
  override def nextInt: (Int, MyRNG) = {
    val newseed = (seed * 0x5DEECE66DL + 0xBL) * 0xFFFFFFFFFFFFL //ビット論理積。元のシードを使って新しいシードを生成
    val nextRNG = SimpleMyRNG(newseed) //次の状態
    val n = (newseed >>> 16).toInt //>>>は0埋めバイナリシフト。値nは新しい擬似乱数
    (n, nextRNG)
  }

  //この場合、i1とi2は同じになります
  //2つの異なる値を生成したい場合は、nextIntの1つ目の呼び出しから返されたMyRNGを使って、
  //2つ目のIntを生成する必要があります。
  override def randomPair_1(rng: MyRNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }
  //rng2を使ってnextIntを生成している
  //また、MyRNGを返却する事によって、呼び出し元が元の新しい状態を使って乱数をさらに生成できる
  override def randomPair(rng: MyRNG): ((Int, Int), MyRNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  //Exercise6.1
  override def nonNegativeInt(rng: MyRNG): (Int, MyRNG) = {
    val (i2, rng2) = rng.nextInt
    if(i2 > 0) (i2, rng2)
    else if(i2 <= Int.MinValue) (Int.MaxValue, rng2)
    else (-i2, rng2)
  }

  //Exercise6.2
  override def double(rng: MyRNG): (Double, MyRNG) = ???
}