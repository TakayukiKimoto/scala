/**
  * Created by user on 2016/07/07.
  *
  * 共変[+T]って？
  * 　→ 多分だけど、多相(Generic)に関する特化(サブクラス)制限とか？
  * 　　下の例で言えば、共変TにAnimalを指定した場合、Tに渡せるものはAnimalもしくはCat(つまり特化方面)となる。
  * 　　この制約の突破条件として「下限境界定義」がある [T1 >: T]
  * 反変[-T]って？
  * 　→ こっちも多分だけど、多相(Generic)に関する汎化(スーパークラス)制限とか？
  * 　　下の例で言えば、共変TにAnimalを指定した場合、Tに渡せるものはAnimalもしくはTestKyohen(つまり汎化方面)となる。
  *     この制約の突破条件として「上限境界定義」がある [T1 < : T]
  * 非変[T]って？
  *   → よく使ってるやつ。簡単に言って仕舞えば、T型以外は不可となる、つまり不変の指定。
  *     制約としては、共変や反変のような代入が出来ない
  */
class TestKyohen
class Animal extends TestKyohen
class Cat extends Animal

class Container[+T, -U] {
  //NG
  //共変は引数には使えない
  /*
  def f1(t: T) = {
  }
  */

  //下限境界定義。なんでもOKとなる。最初から、色んな物がくることを想定しておく、といった使い方？
  def fk1[T1 >: T](t: T1) = {
  }

  //OK
  //反変は引数に使える。共変は戻り値に使える
  //実際はnew T()なんてやっちゃダメって言われるから、これコンパイル通らないけど
  /*
  def f2(u: U): T = {
    new T()
  }
  */

  //NG
  //反変は戻り値に使えない
  /*
  def f3(): U = {
    new U()
  }
  */
  //上限境界定義。サブクラスのみ来る事を保証してやる。今回はエラーが分かりやすくする為、引数をそのまま返却
  def fk3[U1 <: U](u1: U1): U1 = {
    u1
  }
  /*
  def fk3[U1 <: U](): U1 = {
    new U1()
  }
  */
}

object TestMyKyohen{
  def test():Unit = {
    //オブジェクト指向の大原則。スーパークラスにサブクラスのインスタンスは含める事が出来るが逆はダメ
    //val ngani1: Animal = new TestKyohen() //NG
    val ani1: Animal = new Cat() //OK
    //val ngcon1: Container[Animal, Animal] = new Container[TestKyohen, Animal]() //NG
    val con1: Container[Animal, Animal] = new Container[Cat, Animal]() //OK

    //多相に関する制約である反変を指定している場合、上記大原則の逆の指定が可能
    //val ngcon2: Container[Animal, Animal] = new Container[Animal, Cat] //反変 NG
    val con2: Container[Animal, Animal] = new Container[Animal, TestKyohen] //反変 OK


    /*
    //この場合、con1インスタンスはこうなっている
    class Container[Cat, Animal] {
      def f1(t: Cat): Cat = {
        new Cat()
      }
      //f2,f3省略
    }
    //しかしcon1の状態では、TはAnimalと見なされてしまう為、CatにAnimalを渡すという矛盾が発生する。
    //これが、共変は引数には指定できないという制約
     */


    //Tは共変である為、Animalより特化したクラスしか戻り値としてはなり得ない(つまりはAnimalかCat)
    //その為、共変は戻り値に指定できる。
    //戻り値はAnimalだがインスタンスの実態はCat(のはず)。つまり、AnimalクラスにCatインスタンスを含めたものが返ってくるイメージ (多分)
    //ただしインスタンスの実態がCatでも、返ってくるクラスがAnimalなのでCatクラスで受け取る事は出来ない
    //val rtn1: Animal = con1.f2(new Animal)
    //val rtn2: TestKyohen = con1.f2(new Animal)
    //val rtn3: Cat = con1.f2(new Animal) //NG

    /*
    //また、Uは反変である為、Animalより汎化したクラスしか引数にはなり得ない(つまりはAnimalかTestKyohen)
    //con2のインスタンスはこうなっている
    class Container[Animal, TestKyohen] {
      //f1,f3省略
      def f2(u: TestKyohen): Animal = {
        new Animal()
      }
    }
    //con2の状態では、UはAnimalと見なされているが、f2の引数で受け取るのはスーパークラスであるTestKyohenとなり、矛盾は発生しない。
    //これが、反変は引数に指定できるという事。
    */


    /*
    //ここまでの説明でほぼ理解できるが、反変が引数でダメな理由
    con2のインスタンスのf3の実態
    class Container[Cat, TestKyohen] {
      //f1,f2省略
      def f3(): TestKyohen = {
        new TestKyohen()
      }
    }
    //con2のUはAnimalと見なしているのでAnimalクラスを返却するが、実態はTestKyohenとなる為、
    //サブクラスにスーパークラスのインスタンスを渡すという矛盾が発生してしまう。
    //これが、反変は引数に指定できないという制約。
     */

    //下限境界定義 (どう使うんだろう。。。)
    con1.fk1(new TestKyohen) //OK
    con1.fk1(new Animal) //OK
    con1.fk1(new Cat) //OK
    con1.fk1("Test") //これもOK

    //上限境界定義
    //con2.fk3[Animal](new Cat) //引数なしの場合は、型推論出来ないからこうやって書く
    //con2.fk3(new TestKyohen) //NG(コンパイルエラー)
    con2.fk3(new Animal) //OK
    con2.fk3(new Cat) //OK
    //con2.fk3("Test") //NG(もちろんコンパイルエラー)

  }
}