import fpinscala.datastructures._


val t1 = Branch(Leaf(5), Branch(Leaf(4), Leaf(1)))

// Exercise 3.25
def size[A](t: Tree[A]): Int = {
  def loop(tr: Tree[A], acc: Int): Int = tr match {
    case null => acc
    case Leaf(_) => acc + 1
    case Branch(l, r) => 1 + loop(l, acc) + loop(r, acc)
  }
  loop(t, 0)
}

def size2[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + size2(l) + size2(r)
}

size(t1)
size2(t1)

// Exercise 3.26
def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
}

maximum(t1)

// Exercise 3.27