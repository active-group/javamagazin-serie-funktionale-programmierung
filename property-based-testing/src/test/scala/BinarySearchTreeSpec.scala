//
// All of the following is heavily inspired by John Hughes' talk
// "How to specify it! A guide to writing properties of pure functions"
// https://www.youtube.com/watch?v=zvRAyq5wj38
//

import org.scalacheck.Properties

object BinarySearchTreeSpec extends Properties("BinarySearchTree") {
  import BinarySearchTree._

  import org.scalacheck.{ Prop, Gen, Arbitrary, Shrink }
  import org.scalacheck.Prop._
  import org.scalacheck.Arbitrary.arbitrary

  // Generating and shrinking Values
  def isValid(v: Value): Boolean =
    v match { case Value(s) => s.nonEmpty && s.forall(_.isLetterOrDigit) }
  val genValue: Gen[Value] =
    Gen.nonEmptyStringOf(Gen.alphaNumChar).map(Value(_))
  given arbValue: Arbitrary[Value] = Arbitrary(genValue)
  given shrinkValue: Shrink[Value] =
    Shrink.xmap(Value(_), { case Value(s) => s }) suchThat isValid

  // Generating keys with a certain probability of getting two distinct keys or
  // the same one twice.
  case class KeyFrequency(same: Int, distinct: Int)
  def genDistinctKeys(implicit gen: Gen[Int]): Gen[(Int, Int)] =
    for {
      k1 <- gen
      k2 <- gen suchThat (_ != k1)
    } yield (k1, k2)
  def genKeys(implicit gen: Gen[Int], frequency: KeyFrequency = KeyFrequency(1, 1)): Gen[(Int, Int)] = {
    Gen.frequency(
      (frequency.same,     gen.map(i => (i, i))),
      (frequency.distinct, genDistinctKeys),
    )
  }

  // Generating random search trees
  def makeBST[V](nodes: List[(Int, Value)]): BinarySearchTree =
    nodes.foldLeft(Leaf()) { case (t, (k,v)) => t.insert(k, v) }
  val genBST: Gen[BinarySearchTree] =
    Gen.listOf(arbitrary[(Int, Value)]).map(makeBST)
  given arbBST: Arbitrary[BinarySearchTree] = Arbitrary(genBST)
  given shrinkBST: Shrink[BinarySearchTree] = Shrink.xmap(makeBST, _.toList)


  // (1) Properties from invariants
  // Question: Is there an invariant (i.e. a property that holds for any binary search tree)?
  // Yes, here it is:
  def isValid(tree: BinarySearchTree): Boolean =
    tree match {
      case Leaf() => true
      case Branch(left, key, _, right) =>
        left.keys.forall(_  < key) && isValid(left) &&
        right.keys.forall(_ > key) && isValid(right)
    }

  // Guaranteeing validity of test cases
  // We only ever generate valid test trees
  property("arbitrary valid") =
    forAll {(t: BinarySearchTree) => isValid(t)}
  // Skrinking preserves validity
  property("shrink valid") =
    forAll {(t: BinarySearchTree) => isValid(t) ==> shrinkBST.shrink(t).forall(isValid)}

  // The empty tree is valid.
  property("empty valid") = isValid(Leaf())
  // Inserting a key value pair yields a valid tree.
  property("insert valid") =
    forAll {(t: BinarySearchTree, k: Int, v: Value) => isValid(t.insert(k, v))}
  // Deleting a key preserves validity.
  property("delete valid") =
    forAll {(t: BinarySearchTree, k: Int) => isValid(t.delete(k))}


  // (2) Properties from postconditions
  // Question: Is there a postcondition for a given operation?

  // After calling insert, we should be able to find the key inserted, and any
  // other keys present beforehand.
  property("insert post") =
    forAll (genKeys(arbitrary, KeyFrequency(1, 2)),
            arbitrary[Value], arbBST.arbitrary) {
      (keys: (Int, Int), v: Value, t: BinarySearchTree) =>
        val (k, searchKey) = keys
        classify(searchKey == k,"lookup inserted") {
          (searchKey == k ==> (t.insert(k, v).find(searchKey) ?= Some(v))) ++
          (searchKey != k ==> (t.insert(k, v).find(searchKey) ?= t.find(searchKey)))
        }
    }

  // After calling delete, we should not find the key deleted, but any
  // other keys present beforehand.
  property("delete post") =
    forAll {(k: Int, t: BinarySearchTree, searchKey: Int) =>
      (searchKey == k ==> (t.delete(k).find(searchKey) ?= None)) ++
      (searchKey != k ==> (t.delete(k).find(searchKey) ?= t.find(searchKey)))
    }

  // After calling find,
  // - if the key is present in the tree, the result is Some(value)
  // - if the key is not present, the result is None
  property("find post present") =
    forAll {(k: Int, v: Value, t: BinarySearchTree) => t.insert(k, v).find(k) ?= Some(v) }
  property("find post absent") =
    forAll {(k: Int, v: Value, t: BinarySearchTree) => t.delete(k).find(k) ?= None }


  // Equivalence relation on binary search trees
  def equiv(t1: BinarySearchTree, t2: BinarySearchTree): Prop = t1.toList == t2.toList

  // (3) Metamorphic properties
  // Maybe it's hard to predict what the result of a function should be.

  // Maybe it's easier to predict how changing the input to a function will
  // change its result.

  // Example: insert + insert
  //
  //   insert(k', v')
  // t ----------------> ?
  // |                   |
  // | insert(k, v)      | insert(k', v')
  // |                   |
  // v                   v
  // ? ----------------> X <--- can we predict something about this "point"?
  //   insert(k', v')           how do the two paths relate to one another?
  //
  // We cannot predict the result of insert directly without inspecting t, k and
  // v and recreating insert itself.
  //
  // But what if we insert another key value pair (k', v') first?
  // We end up with two cases:
  //
  // Case #1: k == k'
  // If k equals k', the result of first inserting (k', v') and then (k, v)
  // should be the same as if we'd only inserted (k, v), because the second
  // insert overwrites the first.
  //
  // Case #2: k != k'
  // If k differs from k', the resulting trees may differ in shape but not
  // semantically: they are equivalent -- according to the equivalence relation
  // given above.
  //
  property("insert insert") =
    forAll {(k1: Int, k2: Int, v1: Value, v2: Value, t: BinarySearchTree) =>
      (k1 == k2 ==> ("insert same key twice" |:
        (t.insert(k1, v1).insert(k2, v2) ?= t.insert(k2, v2)))) ++
      (k1 != k2 ==> ("insert different keys" |:
        equiv(t.insert(k1, v1).insert(k2, v2), t.insert(k2, v2).insert(k1, v1))))
    }

  property("delete delete") =
    forAll {(k1: Int, k2: Int, t: BinarySearchTree) =>
      (k1 == k2 ==> ("delete same key" |:
        (t.delete(k1).delete(k2) ?= t.delete(k1)))) ++
      (k1 != k2 ==> ("delete different keys" |:
        (t.delete(k1).delete(k2) ?= t.delete(k1).delete(k2))))
    }

  property("delete insert same key") =
    forAll {(k: Int, v: Value, t: BinarySearchTree) =>
      equiv(t.delete(k).insert(k, v), t.insert(k, v))
    }
  property("insert delete same key") =
    forAll {(k: Int, v: Value, t: BinarySearchTree) =>
      equiv(t.insert(k, v).delete(k), t.delete(k))
    }
  property("insert delete distinct keys") =
    forAll {(k: Int, v: Value, delKey: Int, t: BinarySearchTree) =>
      k != delKey ==> equiv(t.insert(k, v).delete(delKey), t.delete(delKey).insert(k, v))
    }


  // (4) Model-based properties
  // Idea: Use an abstract model with an abstract operation to test the real
  // implementation.

  // What is an appropriate abstract model for binary search trees?
  // Ordered Lists of key value pairs are a good match with some limitations
  // with respect to insert (see below).

  // Example: insert
  //
  //   toList
  // t--------------->l
  // |                |
  // | insert(k, v)   | append(k, v)
  // |                |
  // v                v
  // ?--------------->X <--- What do we expect here, that is, how do the two
  //   toList                paths relate?
  //
  // append on lists is a model for insert on binary search trees, as long as we:
  // - re-establish the order implied by a binary search tree after inserting
  //   the new pair, that is, sorting the resulting list by key
  // - ensure that the inserted key is not present in the list before appending
  //   the new pair as insert on binary search trees replaces an existing key
  property("insert model") =
    forAll {(k: Int, v: Value, t: BinarySearchTree) =>
      t.insert(k, v).toList ?= (t.toList.filter(_._1 != k) :+ (k, v)).sortBy(_._1)
    }

  // filter on lists is a model for delete on binary search trees
  property("delete model") =
    forAll {(k: Int, t: BinarySearchTree) =>
      t.delete(k).toList ?= t.toList.filter(_._1 != k)
    }

  // find on lists is a model for find on binary search
  property("find model") =
    forAll {(k: Int, t: BinarySearchTree) =>
      t.find(k) ?= t.toList.find(_._1 == k).map(_._2)
    }
}
