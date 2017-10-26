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
def depth[A](t: Tree[A]): Int = {
  def depth_loop(tr: Tree[A], acc: Int): Int = tr match {
    case Leaf(_) => acc
    case Branch(l, r) => depth_loop(l, acc + 1) max depth_loop(r, acc + 1 )
  }

  depth_loop(t, 0)
}

def depth2[A](t: Tree[A]): Int =  t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth2(l) max depth2(r))
}


depth(t1)
depth2(t1)

// Exercise 3.28
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l, r) => Branch(map(l)(f),  map(r)(f))
}

map(t1)(_ + 10)

// Exercise 3.29
// Generalize size, maximum, depth, and map, writing a new function fold
def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B): B