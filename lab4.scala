import scala.reflect.ClassTag


abstract class Summator[T] {
  def mutate(a: T, b: T) : T
}

object Summator {
  def mutate[T: Summator](a: T, b: T) : T = {
    implicitly[Summator[T]].mutate(a, b)
  }
  implicit val long2Summator: Summator[Long] = new Summator[Long] {
    override def mutate(a: Long, b: Long) : Long = a + b
  }
  implicit val int2Summator: Summator[Int] = new Summator[Int] {
    override def mutate(a: Int, b: Int) : Int = a + b
  }

  implicit val str2Summator: Summator[String] = new Summator[String] {
    override def mutate(a: String, b: String) : String = s"${a}${b}"
  }
}


class Fib[T] (f: T, s: T) {
  val first = f
  val second = s


  def next(implicit sum: Summator[T]) : Fib[T] = {
    new Fib(second, sum.mutate(first, second))
  }


  def getValue() : T = {
    return first
  }
}

object Lab4{
  def main(args: Array[String]): Unit = {
    var a = new Fib(0, 1)
    var i = 0
    while(i < 10){
      println(a.getValue)
      a = a.next
      i += 1
    }
  }
}
