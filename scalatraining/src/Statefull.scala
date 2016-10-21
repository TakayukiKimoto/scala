/**
  * Created by user on 2016/08/28.
  */
object Statefull {
  //サイコロを振るシミュレーション
  //*この方法には色々な問題がある
  def folldie_1: Int = {
    val rnd = new scala.util.Random
    rnd.nextInt(6)
  }
  //乱数ジェネレータを渡してみる
  //そうすれば、失敗するテストを再現したい時、失敗した乱数ジェネレータと同じものが渡せます
  //しかし、この方法にも問題があります。
  //これを解決する方法はどうするのか？ => MyRngの[nextint]に記載
  def rolldie_2(rnd: scala.util.Random): Int = {
    rnd.nextInt(6)
  }
}
