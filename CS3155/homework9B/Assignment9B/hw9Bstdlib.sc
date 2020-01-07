
// Nats

sealed trait Nat
case object Zero extends Nat
case class Succ(pred : Nat) extends Nat

val one = Succ(Zero)
val two = Succ(one)
val three = Succ(two)
val four = Succ(three)
val five = Succ(four)
val six = Succ(five)
val seven = Succ(six)
val eight = Succ(seven)
val nine = Succ(eight)
val ten = Succ(nine)

def nat_plus(x : Nat, y : Nat) : Nat = x match {
    case Zero    => y
    case Succ(x) => Succ(nat_plus(x, y))
}

def nat_to_int(x : Nat) : Int = x match {
    case Zero => 0
    case Succ( x ) => 1 + nat_to_int(x)
}

def nat_to_str(x : Nat) = nat_to_int(x).toString()

def print_nat(x : Nat) = println(nat_to_str(x))

def nat_mult(x : Nat, y : Nat) : Nat = x match {
    case Zero    => Zero
    case Succ(x) => nat_plus(nat_mult(x,y), y)
}


def nat_minus(x : Nat, y : Nat) : Nat = (x, y) match {
    case (Zero, _)          => Zero
    case (x, Zero)          => x
    case (Succ(x), Succ(y)) => nat_minus(x, y)
}

def nat_pow(x : Nat, y : Nat) : Nat = x match {
    case Zero       => one
    case Succ(Zero) => y 
    case Succ(x)    => nat_mult(y, nat_pow(x, y))
}

def nat_lte(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, y)          => True
    case (x, Zero)          => False
    case (Succ(x), Succ(y)) => nat_lte(x, y)
}

// Integers

sealed trait Integer
case class Positive(x : Nat) extends Integer
case class Negative(x : Nat) extends Integer

def int_to_str(x : Integer) : String = x match {
    case Positive(x) => nat_to_str(x)
    case Negative(x) => "-" + nat_to_str(x)
}

def print_integer(x : Integer) = println(int_to_str(x))

def abs(x : Integer) : Nat = x match {
    case Positive(x) => x
    case Negative(x) => x
}

def negate(x : Integer) : Integer = x match {
    case Positive(x) => Negative(x)
    case Negative(x) => Positive(x)
}

def plus(n : Integer, m : Integer) : Integer = (n, m) match {
    case (Positive(x), Positive(y)) => Positive(nat_plus(x, y))
    case (Negative(x), Negative(y)) => Negative(nat_plus(x,y))
    case (Negative(x), Positive(y)) => nat_lte(x, y) match {
        case True  => Positive(nat_minus(y, x))
        case False => Negative(nat_minus(x, y))
    }
    case (Positive(x), Negative(y)) => nat_lte(x, y) match {
        case True  => Negative(nat_minus(y, x))
        case False => Positive(nat_minus(x, y))
    }    
}

def minus(x : Integer, y : Integer) : Integer = plus(x, negate(y))

def mult(x : Integer, y : Integer) : Integer = (x,y) match {
    case (Positive(x), Positive(y)) => Positive(nat_mult(x, y))   
    case (Negative(x), Negative(y)) => Positive(nat_mult(x, y))
    case (Negative(x), Positive(y)) => Negative(nat_mult(x, y))
    case (Positive(x), Negative(y)) => Negative(nat_mult(x, y))
}

def pow(x : Integer, y : Nat) : Integer = y match {
    case Zero => Positive(Succ(Zero))
    case Succ(y) => mult(x, pow(x, y))
}

// Booleans

sealed trait Bool
case object True extends Bool
case object False extends Bool

def t = True
def f = False

def id(x : Bool) : Bool = x

def not(x : Bool) : Bool = x match {
    case True => False
    case False => True
}

def and(x : Bool, y : Bool) : Bool = (x,y) match {
    case (True, True) => True
    case _ => False
}

def or(x : Bool, y : Bool) : Bool = (x, y) match {
    case (False, False) => False
    case _              => True
}

def xor(x : Bool, y : Bool) : Bool = (x, y) match{
    case (True, False) => True
    case (False, True) => True
    case _ => False
}

def nand(x : Bool, y : Bool) : Bool = not(and(x,y))

def lte(x: Nat, y: Nat): Bool = (x, y) match {
    case (Zero, _) => True
    case (_, Zero) => False
    case (Succ(p1), Succ(p2)) => lte(p1, p2)
}

// Lists

sealed trait List[+A]
case object Empty extends List[Nothing]
case class Cons[A](x : A, xs : List[A]) extends List[A]

def singleton[A](x : A) : List[A] = Cons(x, Empty)

def length[A](xs : List[A]) : Nat = xs match {
    case Empty => Zero
    case Cons(_, xs) => Succ(length(xs))
}

def append[A](xs : List[A], ys : List[A]) : List[A] = xs match {
    case Empty => ys
    case Cons(x, xs) => Cons(x, append(xs, ys))
}

def fold[A, B](f : ((A, B) => B), acc : B, xs : List[A]) : B = xs match {
    case Empty       => acc
    case Cons(x, xs) => fold(f, f(x, acc), xs)
}

def map[A,B](f : (A => B), xs : List[A]) : List[B] = xs match {
    case Empty => Empty
    case Cons(x, xs) => Cons(f(x), map(f,xs))
}

def filter[A](p : (A => Bool), xs : List[A]) : List[A] = xs match {
    case Empty       => Empty
    case Cons(x, xs) => p(x) match {
        case True  => Cons(x, filter(p, xs))
        case False => filter(p, xs)
    }
}

def reverse[A](xs : List[A]) : List[A] =
    fold(Cons(_: A, _: List[A]), Empty, xs)
    
//concatMap               :: (a -> [b]) -> [a] -> [b]
//concatMap f             =  foldr ((++) . f) []
def concatMap[A, B]( f  : (A => List[B])
                   , xs : List[A]  
                   ) : List[B] = fold( ((x : A, ys : List[B]) => append(ys, f(x))  ) , Empty, xs)



// Maybe
sealed trait Maybe[+A]
case object None extends Maybe[Nothing]
case class Just[A](x : A) extends Maybe[A]

def map_maybe[A, B](f : (A => B), mx : Maybe[A]) : Maybe[B] = mx match {
    case None => None
    case Just(x) => Just(f(x))
}

// Products

sealed trait Pair[+A, +B]
case class MkPair[A, B](fst : A, snd : B) extends Pair[A, B]

// Sums

sealed trait Either[+A, +B]
case class Left[A, B](left : A) extends Either[A, B]
case class Right[A, B](right : B) extends Either[A, B]

// Equality
def bool_eq(x : Bool, y : Bool) : Bool = (x, y) match {
    case (True, True) => True
    case (False, False) => True
    case _ => False
}

def nat_eq(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, Zero)         => True
    case (Succ(px), Succ(py)) => nat_eq(px, py)
    case _                    => False
}

def int_eq(x : Integer, y : Integer) : Bool = (x, y) match {
    case (Positive(x), Positive(y))       => nat_eq(x, y)
    case (Negative(x), Negative(y))       => nat_eq(x, y)
    case (Positive(Zero), Negative(Zero)) => True
    case (Negative(Zero), Positive(Zero)) => True
    case _                                => False
}

def list_eq[A](l1: List[A], l2: List[A], eq: (A, A) => Bool): Bool =
    (l1, l2) match {
        case (Empty, Empty) => True
        case (Cons(x1, t1), Cons(x2, t2)) => and(eq(x1, x2), list_eq(t1, t2, eq))
        case _ => False
    }
    
def char_eq(c1 : Char, c2 : Char) : Bool = if (c1 == c2) True else False

def string_eq(s1 : String, s2 : String) : Bool = if (s1 == s2) True else False

// Dicts

sealed trait Dictionary[+A, +B] 
case object EmptyDict extends Dictionary[Nothing, Nothing]
case class Entry[A,B](key : A, value : B, xs : Dictionary[A, B]) extends Dictionary[A, B]

def is_in[A, B](eq : ((A, A) => Bool), dict : Dictionary[A, B], k : A ) : Bool = dict match {
    case EmptyDict         => False
    case Entry(k1, v, kvs) => eq(k,k1) match {
        case True  => True
        case False => is_in(eq, kvs, k)
    }
}

def lookup[A, B](eq : (A, A) => Bool, dict : Dictionary[A, B], k : A) : Maybe[B] = dict match {
    case EmptyDict         => None
    case Entry(k1, v, kvs) => eq(k,k1) match {
        case True  => Just(v)
        case False => lookup(eq, kvs, k)
    }
}

def get_or_default[A](m: Maybe[A], default: A): A = m match {
    case Just(x) => x
    case None    => default
}

// Strings

def string_to_list(s1 : String) : List[Char] = 
  if(s1 == "") {
    Empty
  }
  else {
    val x  = s1(0)
    val xs = s1.substring(1)
    Cons(x, string_to_list(xs))
  }

def list_to_string(l: List[Char]): String =
    fold[Char, String](_ + _, "", reverse(l))

// TEST HELPER
def passed(points: Int) {
    require(points >=0)
    if (points == 1) print(s"\n*** Tests Passed (1 point) ***\n")
    else print(s"\n*** Tests Passed ($points points) ***\n")
}