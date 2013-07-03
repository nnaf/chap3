Chapter 3
============


''' scala  
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
'''
  
Variadic function
------------------
'''  
def apply[A](as: A*): List[A] = // Variadic function syntax
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
'''

Exercise 1
-------------
	'''  
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
	'''

Exercise 1
-------------
'''scala  
def append[A](a1: List[A], a2: List[A]): List[A] =
  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
}
'''

Autre
-------------
'''scala  
def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
  l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def sum2(l: List[Int]) = 
  foldRight(l, 0.0)(_ + _)

def product2(l: List[Double]) = 
  foldRight(l, 1.0)(_ * _)
'''


Exercise 2
-------------
  /* 
  3. The third case is the first that matches, with `x` bound to 1 and `y` bound to 2. 
  */

  /*
  Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is a somewhat subjective choice. In our experience taking the tail of an empty list is often a bug, and silently returning a value just means this bug will be discovered later, further from the place where it was introduced. 
  
  It's generally good practice when pattern matching to use '_' for any variables you don't intend to use on the right hand side of a pattern. It makes it clear the value isn't relevant.  
  */
'''scala  
def tail[A](l: List[A]): List[A] = 
  l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,t) => t
  }
'''

Exercise 3
-------------
  /* 
  Again, it is somewhat subjective whether to throw an exception when asked to drop more elements than the list contains. The usual default for `drop` is not to throw an exception, since it is typically used in cases where this is not indicative of a programming error. If you pay attention to how you use `drop`, it is often in cases where the length of the input list is unknown, and the number of elements to be dropped is being computed from something else. If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.  
  */
'''scala  
def drop[A](l: List[A], n: Int): List[A] = 
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case Cons(_,t) => drop(t, n-1) 
  }
'''

Exercise 4
-------------
  /* 
  Somewhat overkill, but to illustrate the feature we are using a _pattern guard_, to only match a `Cons` whose head satisfies our predicate, `f`. The syntax is simply to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can use any of the variables introduced by the pattern.
  */
'''scala  
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
  l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f) 
    case _ => l
} 
'''

Exercise 5
-------------
  /*
  If a function body consists solely of a match expression, we'll often put the match on the same line as the function signature, rather than introducing another level of nesting.
  */
'''scala  
def setHead[A](l: List[A])(h: A): List[A] = l match {
  case Nil => sys.error("setHead on empty list")
  case Cons(_,t) => Cons(h,t)
}
'''

Exercise 6
-------------
  /*
  Notice we are copying the entire list up until the last element. Besides being inefficient, the natural recursive solution will use a stack frame for each element of the list, which can lead to stack overflows for large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams which we discuss in chapter 5, we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
  
  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which does not require even local mutation. We will write a reverse function later in this chapter.
  */
'''scala  
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
'''

  /* 
  No, this is not possible! The reason is that _before_ we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in chapter 5.
  */

Exercise 9
-------------
  /* 
  We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list. 
  
'''scala  
foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
Cons(1, Cons(2, Cons(3, Nil))) 
'''


Exercise 1
-------------
'''scala  
def length[A](l: List[A]): Int = 
  foldRight(l, 0)((_,acc) => acc + 1)
'''

Exercise 10
-------------
  /* 
  It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting in greater stack space usage at runtime. 
  */
'''scala  
@annotation.tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
  case Nil => z
  case Cons(h,t) => foldLeft(t, f(z,h))(f)
}

'''

Exercise 11
-------------
'''scala  
def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)
'''

Exercise 12
-------------
'''scala  
def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
'''

Exercise 13-14
-------------
  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows when implementing a strict `foldRight` function as we've done in this chapter. (We will revisit this in a later chapter, when we discuss laziness).
  
  The other implementations build up a chain of functions which, when called, results in the operations being performed with the correct associativity.
  */
'''scala  
def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
  foldLeft(reverse(l), z)((b,a) => f(a,b))

def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
  foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = 
  foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
'''


Exercise 15
-------------

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation performed by `foldRight`.
  */

'''scala  
def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
  foldRight(l, r)(Cons(_,_))
'''



