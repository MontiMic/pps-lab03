package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence.concat

import scala.Console.println
import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case _ => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case _ => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (Cons(_, tail), n) if n > 0 => skip(tail)(n - 1)
      case (s, 0) => s
      case _ => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    /*def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))
      case (_, Cons(h2, t2)) => Cons(h2, concat(s1, t2))
      case _ => Nil()*/
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => Cons(h, concat(t, s2))
      case _ => s2

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */

    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def reverseHelper(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Nil() => acc
        case Cons(h, t) => reverseHelper(t, Cons(h, acc))
      reverseHelper(s, Nil())


    /* Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min_v1(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h, t) =>
        @tailrec
        def _search(min: Int, s: Sequence[Int]): Int = s match
          case Cons(h, t) if h < min => _search(h, t)
          case Cons(h, t) => _search(min, t)
          case _ => min
        Optional.Just(_search(h, t))
      case _ => Optional.Empty()

    def min(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h, t) =>
        min(t) match
          case Optional.Just(n) => Optional.Just(if h < n then h else n)
          case _ => Optional.Just(h)
      case _ => Optional.Empty()

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, Nil()) => Cons(h, Nil())
      case Cons(h, Cons(_, t)) => Cons(h, evenIndices(t))
      case _ => Nil()

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h == elem => true
      case Cons(_, t) => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _distinct(s: Sequence[A], acc: Sequence[A]) : Sequence[A] = s match
        case Cons(h, t) if contains(acc)(h) => _distinct(t, acc)
        case Cons(h, t) => _distinct(t, Cons(h, acc))
        case _ => reverse(acc)
      _distinct(s, Nil())  

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Cons(h, t) =>
        @tailrec
        def _subLister(s: Sequence[A], acc: Sequence[A], toSkip: Int): (Sequence[A], Int) = (s, acc) match
          case (Cons(sh, st), Cons(acch, _)) if sh == acch => _subLister(st, Cons(sh, acc), toSkip + 1)
          case _ => (reverse(acc), toSkip)

        val (subList, toSkip) = _subLister(t, Cons(h,Nil()), 0)
        Cons(subList, group(skip(t)(toSkip)))
      case _ => Nil()

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition_v1[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      def _append(s: Sequence[A], elem: A): Sequence[A] = s match
        case Cons(h, t) => Cons(h, _append(t, elem))
        case _ => Cons(elem, Nil())

      @tailrec
      def _partition(s: Sequence[A], acc1: Sequence[A], acc2: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
        case Cons(h, t) if pred(h) => _partition(t, _append(acc1, h), acc2)(pred)
        case Cons(h, t) => _partition(t, acc1, _append(acc2, h))(pred)
        case _ => (acc1, acc2)
      _partition(s, Nil(), Nil())(pred)

    /*
     * Alternative implementation of partition without the append method
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def _partition(s: Sequence[A], acc1: Sequence[A], acc2: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
        case Cons(h, t) if pred(h) => _partition(t, Cons(h, acc1), acc2)(pred)
        case Cons(h, t) => _partition(t, acc1, Cons(h, acc2))(pred)
        case _ => (reverse(acc1), reverse(acc2))

      _partition(s, Nil(), Nil())(pred)

  end Sequence
end Sequences

@main def trySequences(): Unit =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) //21+31 = 52