Chapter 3
============
Functional data structures:
* pattern matching
* recursion
* data sharing, as immutable data != copy

```scala  
sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
}
```
`+A`: covariant type 


Variadic function
------------------

```scala  
def apply[A](as: A*): List[A] = // Variadic function syntax
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
```

`_*` : repetition of the same type element





Exercise 1
-------------

```scala  
val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
val example2 = List(1,2,3)
val total = sum(example)

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42 
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101 
}
```


Exercise 2
-------------

```scala  
def tail[A](l: List[A]): List[A] = 
  l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,t) => t
  }
```

Exercise 3
-------------

```scala  
def drop[A](l: List[A], n: Int): List[A] = 
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case Cons(_,t) => drop(t, n-1) 
  }
```

Exercise 4
-------------

```scala  
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
  l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f) 
    case _ => l
} 
```

Exercise 5
-------------

```scala  
def setHead[A](l: List[A])(h: A): List[A] = l match {
  case Nil => sys.error("setHead on empty list")
  case Cons(_,t) => Cons(h,t)
}
```

Exercise 6
-------------

  Notice we are copying the entire list up until the last element. Besides being inefficient, the natural recursive solution will use a stack frame for each element of the list, which can lead to stack overflows for large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams which we discuss in chapter 5, we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
  
  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which does not require even local mutation. We will write a reverse function later in this chapter.


```scala  
def init[A](l: List[A]): List[A] = 
  l match { 
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
def init2[A](l: List[A]): List[A] = {
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]
  @annotation.tailrec
  def go(cur: List[A]): List[A] = cur match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => List(buf.toList: _*)
    case Cons(h,t) => buf += h; go(t)
  }
  go(l)
}
```
Exercise 7-8
-------------
right fold: A*(B*(C*D))

```scala  
def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = 
  l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def sum2(l: List[Int]) = 
  foldRight(l, 0.0)(_ + _)

def product2(l: List[Double]) = 
  foldRight(l, 1.0)(_ * _)
```


 
```scala  
foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
Cons(1, Cons(2, Cons(3, Nil))) 
```


Exercise 9
-------------
```scala  
def length[A](l: List[A]): Int = 
  foldRight(l, 0)((_,acc) => acc + 1)
```

Exercise 10
-------------
tail-recursive foldLeft

```scala  
@annotation.tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
  case Nil => z
  case Cons(h,t) => foldLeft(t, f(z,h))(f)
}

```

Exercise 11
-------------
```scala  
def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)
```

Exercise 12
-------------
```scala  
def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
```

Exercise 13-14
-------------

```scala  
def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
  foldLeft(reverse(l), z)((b,a) => f(a,b))

def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
  foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = 
  foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
```


Exercise 15
-------------


```scala  
def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
  foldRight(l, r)(Cons(_,_))
```



