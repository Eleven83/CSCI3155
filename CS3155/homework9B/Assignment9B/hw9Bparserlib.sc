import $file.hw9Bstdlib
import hw9Bstdlib._

type Parser[S,D] = List[S] => List[(D, List[S])]

def char(c : Char) : Parser[Char,Char] = 
(ss : List[Char]) => ss match {
    case Empty      => Empty
    case Cons(s,ss) => char_eq(s,c) match {
        case True  => singleton((s, ss))
        case False => Empty
    }
}

def success[S, D](x : D) : Parser[S, D] = 
  (ss : List[S]) => singleton((x, ss))

def failure[S, D]() : Parser[S,D] = (ss : List[S]) => Empty

def choose[S, D](p : Parser[S, D], q : Parser[S, D]) : Parser[S, D] = (ss : List[S]) =>
{
    val p_res = p(ss)
    val q_res = q(ss)
    append(p_res, q_res)
} 

def runParser[D](p : Parser[Char, D], s : String) : Maybe[D] = p(string_to_list(s)) match {
    case Empty                   => None
    case Cons((x, Empty), Empty) => Just(x)
    case _                       => None
}

def satisfies[S](p : (S => Bool)) : Parser[S, S] = (xs : List[S]) => xs match {
    case Empty       => Empty
    case Cons(x, xs) => p(x) match{
        case True  => singleton((x,xs))
        case False => Empty
    } 
}

def bind[S, D, E](p : Parser[S,D], q : (D => Parser[S,E]) ) : Parser[S,E] = 
  (ss : List[S]) => {
      val join = (res : (D, List[S])) => res match {case (d, ss2) => q(d)(ss2)}
      concatMap(join, p(ss))
  } 

def stringL(ss : List[Char]) : Parser[Char, List[Char]] = ss match {
    case Empty      => success(Empty)
    case Cons(s,ss) =>        bind(char(s), 
         (c : Char)        => bind(stringL(ss),
         (cs : List[Char]) => success(Cons(c,cs)) ))
}

def string(str : String) : Parser[Char, List[Char]] = stringL(string_to_list(str))

def mapParser[S,A,B](f : (A => B), p : Parser[S,A]) : Parser[S,B] = bind(p, (result : A) => success(f(result)))

def option[S, D](p : Parser[S, D], q : Parser[S, D]) : Parser[S, D] = (ss : List[S]) => 
  p(ss) match {
      case Empty => q(ss)
      case res   => res
  }

// def option[S, D](p : Parser[S, D], q : Parser[S, D]) : Parser[S, D] = (ss : List[S]) => 
//   p(ss) match {
//       case Empty => q(ss)
//       case res   => res
//   }

def whitespaceChar : Parser[Char,Char] = choose(char(' '),
                                         choose(char('\n'), 
                                         choose(char('\r'), 
                                                char('\t'))))

def many[S,D](p : Parser[S,D]) : Parser[S, List[D]] = option(
                                                        bind(p, 
                                                            (x : D) => bind(many(p),
                                                            (xs : List[D]) => success(Cons(x,xs))))
                                                       , success(Empty)
                                                       )

// A whitespace parser which requires at least one whitespace character
// This is used to parse spaces between lexical elements
def whitespace : Parser[Char, List[Char]] = bind(whitespaceChar, 
                                                 (x : Char)  => bind(many(whitespaceChar),
                                                 (xs : List[Char]) => success(Cons(x,xs))))

// A whitespace parser for zero or more spaces
def maybeWhitespace : Parser[Char, List[Char]] = many(whitespaceChar)

def digit : Parser[Char, Char] = option(char('0'),
                                 option(char('1'),
                                 option(char('2'),
                                 option(char('3'),
                                 option(char('4'),
                                 option(char('5'),
                                 option(char('6'),
                                 option(char('7'),
                                 option(char('8'),
                                        char('9'))))))))))

def numString : Parser[Char, List[Char]] = many(digit)

def digitToNat(c : Char) : Nat = c match {
    case '0' => Zero
    case '1' => one
    case '2' => two
    case '3' => three
    case '4' => four
    case '5' => five
    case '6' => six
    case '7' => seven
    case '8' => eight
    case '9' => nine
}

def stringToNat(xs : List[Char]) : Nat = 
  fold( (x : Nat, acc : Nat) => nat_plus(x, nat_mult(acc, ten)) ,Zero, map(digitToNat, xs))

def number : Parser[Char, Nat] = bind(numString,
                                     (ss : List[Char]) => ss match {
                                         case Empty       => failure()
                                         case Cons(x, xs) => success(stringToNat(ss))
                                     })