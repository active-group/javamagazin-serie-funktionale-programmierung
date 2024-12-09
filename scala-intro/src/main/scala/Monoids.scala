trait Monoid[T] {
  val empty: T
  def combine(a: T, b: T): T
}

object Monoids {

  def myCombine_old1[T <: Monoid[T]](l: Seq[T]): T = {
    val combine = l.head.combine
    val empty = l.head.empty
    l.fold(empty)(combine)
  }

  def myCombine_old2[T](l: Seq[T], m: Monoid[T]): T = {
    l.fold(m.empty)(m.combine)
  }

  val IntAddMonoid = new Monoid[Int] {
    override val empty: Int = 0

    override def combine(a: Int, b: Int): Int = a + b
  }

  myCombine_old2(Seq(1, 2, 3), IntAddMonoid)


  def myCombine1[T](using m: Monoid[T])(l: Seq[T]): T = {
    l.fold(m.empty)(m.combine)
  }

  given Monoid[Int] with {
    val empty: Int = 0
    def combine(a: Int, b: Int): Int = a + b

  }

  def myCombine2[T: Monoid](l: Seq[T]): T = {
    val m = summon[Monoid[T]]
    l.fold(m.empty)(m.combine)
  }

}