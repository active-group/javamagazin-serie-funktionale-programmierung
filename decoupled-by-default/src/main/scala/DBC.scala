object DBC {
  type Key = String;
  enum DBOp[A] {
    case Put[T](key: Key, value: T) extends DBOp[Unit]
    case Get[T](key: Key) extends DBOp[Option[T]]
  }

  import cats.free.Free
  type DB[A] = Free[DBOp, A]

  import DBOp.*
  import cats.free.Free.liftF

  // Put returns nothing (i.e. Unit).
  def put[T](key: Key, value: T): DB[Unit] =
    liftF[DBOp, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: Key): DB[Option[T]] =
    liftF[DBOp, Option[T]](Get[T](key))

  // Update composes get and set, and returns nothing.
  def update[T](key: Key, f: T => T): DB[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()


  def program: DB[Option[Int]] =
    for {
      _ <- put("dillo", 2)
      _ <- update[Int]("dillo", (_ + 12))
      _ <- put("parrot", 5)
      n <- get[Int]("dillo")
    } yield n


  import cats.{Id, ~>}
  import scala.collection.mutable

  val mutableMapRunner: DBOp ~> Id =
    new (DBOp ~> Id) {
      val kvs = mutable.Map.empty[Key, Any]
      def apply[A](fa: DBOp[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).asInstanceOf[A]
        }
    }

  def eval1[A](program : DB[A]): A =
    program.foldMap(mutableMapRunner)

  val result1: Option[Int] = eval1(program)


  import cats.data.State

  type DBState[A] = State[Map[Key, Any], A]
  val stateMapRunner: DBOp ~> DBState =
    new (DBOp ~> DBState) {
      def apply[A](fa: DBOp[A]): DBState[A] =
        fa match {
          case Put(key, value) =>
            State.modify(_.updated(key, value))
          case Get(key) =>
            State.inspect(_.get(key).asInstanceOf[A])
        }
    }

  def eval2[A](program : DB[A]): A =
    program.foldMap(stateMapRunner).run(Map.empty).value._2

  val result2: Option[Int] = eval2(program)


  import cats.effect.IO

  object Database {
    def insert[T](key : Key, value : T) : IO[Unit] = ???
    def select[T](key : Key) : IO[Option[T]] = ???
  }

  val databaseRunner: DBOp ~> IO =
    new (DBOp ~> IO) {
      def apply[A](fa: DBOp[A]): IO[A] =
        fa match {
          case Put(key, value) =>
            Database.insert(key, value)
          case Get(key) =>
            Database.select(key)
        }
    }

  def eval3[A](program : DB[A]): IO[A] =
    program.foldMap(databaseRunner)

  // implement ??? above first
  // val result3: Option[Int] = eval3(program).unsafeRunSync()
}
