case class Value(value: String)
enum BinarySearchTree{
  case Leaf()
  case Branch(left: BinarySearchTree, key: Int, value: Value, right: BinarySearchTree)

  def find(searchKey: Int): Option[Value] =
    this match {
      case Leaf() => None
      case Branch(left, key, value, right) =>
        if (searchKey < key) then
          left.find(searchKey)
        else if (searchKey > key) then
          right.find(searchKey)
        else
          Some(value)
    }

  def insert(newKey: Int, newValue: Value): BinarySearchTree =
    this match {
      case Leaf() => Branch(Leaf(), newKey, newValue, Leaf())
      case Branch(left, key, value, right) =>
        if (newKey < key) then
          Branch(left.insert(newKey, newValue), key, value, right)
        else if (newKey > key) then
          Branch(left, key, value, right.insert(newKey, newValue))
        else
          Branch(left, key, newValue, right)
    }

  def delete(delKey: Int): BinarySearchTree =
    this match {
      case Leaf() => this
      case Branch(left, key, value, right) =>
        if (delKey < key) then
          Branch(left.delete(delKey), key, value, right)
        else if (delKey > key) then
          Branch(left, key, value, right.delete(delKey))
        else
          (left, right) match {
            case (Leaf(), Leaf()) => Leaf()
            case (Leaf(), _)      => right
            case (_, Leaf())      => left
            case (_, _)           =>
              val (minDelKey, minValue) = right.findMin.get
              Branch(left, minDelKey, minValue, right.delete(minDelKey))
          }
    }

  private def findMin: Option[(Int, Value)] =
    this match {
      case Leaf() =>
        None
      case Branch(Leaf(), key, value, _) =>
        Some(key, value)
      case Branch(left, _, _, _) =>
        left.findMin
    }

  // Primarily for tests
  def toList: List[(Int, Value)] =
    this match {
      case Leaf() => List()
      case Branch(left, key, value, right) => left.toList ++ List((key, value)) ++ right.toList
    }
  def keys: List[Int] = toList.map(_._1)
}
