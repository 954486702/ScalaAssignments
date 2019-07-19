// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// let- and right-associativity
abstract class Assoc
case object RA extends Assoc
case object LA extends Assoc

// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}

//val ops = List("+", "-", "*", "/", "^")


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")
def is_op(op: String) : Boolean = ops.contains(op)
def prec(op1: String, op2: String) : Boolean = if(op2=="^") precs(op1) > precs(op2) else precs(op1) >= precs(op2)


def collect(op: String, st: Toks, mv: Toks = Nil) : Toks =  {
  if( st != Nil && is_op(st.head) && prec(st.head, op) ) {/*println(st.tail);*/ collect(op,st.tail, mv ::: List(st.head)) }
  else mv
}

// (8) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

// def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = ...
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = /*toks.head match*/ {
  if(toks != Nil) toks.head match{
    case "+" => if (collect ("+", st) != Nil) syard(toks.tail, List("+") ::: st.takeRight (st.length - collect ("+", st).length), out ::: collect ("+", st) )
    else syard(toks.tail, List("+") ::: st, out) //st//placeholder for now

    case "-" => if (collect ("-", st) != Nil) syard(toks.tail, List("-") :::st.takeRight (st.length - collect ("-", st).length), out ::: collect ("-", st) )
    else syard(toks.tail, List("-") ::: st, out) //st//placeholder for now

    case "*" => if (collect ("*", st) != Nil) syard(toks.tail, List("*") :::st.takeRight (st.length - collect ("*", st).length), out ::: collect ("*", st) )
    else  syard(toks.tail, List("*") ::: st, out) //st//placeholder for now

    case "/" => if (collect ("/", st) != Nil) { syard(toks.tail, List("/") :::st.takeRight(st.length - collect ("/", st).length), out ::: collect ("/", st) ) }
    else  syard(toks.tail, List("/") ::: st, out) //st//placeholder for now

    case "^" => if (collect ("^", st) != Nil) { syard(toks.tail, List("^") :::st.takeRight(st.length - collect ("^", st).length), out ::: collect ("^", st) ) }
    else  syard(toks.tail, List("^") ::: st, out) //st//placeholder for now

    case "(" => syard (toks.tail, List ("(") ::: st, out)
    case ")" => syard (toks.tail, st.takeRight (st.length - (st.indexOf ("(") + 1) ), out ::: st.slice (0, st.indexOf ("(") ) )
    case num => if(toks.length==1) syard(Nil, Nil, out ::: List(num) ::: st)  else syard(toks.tail, st, out ::: List(num) )
  }
  else out ::: st
}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (9) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

//def compute(toks: Toks, st: List[Long] = Nil) : Long = ...
def compute(toks: Toks, st: List[Long] = Nil) : Long = {
  if(toks!= Nil) toks.head match{
    case "+" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)+st.last ) )
    case "-" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)-st.last ) )
    case "*" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)*st.last ) )
    case "/" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)/st.last ) )
    case "^" =>  compute(toks.tail,  st.take(st.length-2) ::: List(Math.pow( st(st.length-2).toDouble,st.last.toDouble ).toLong) )
    case num =>  compute(toks.tail, st ::: List( num.toLong ))
  }
  else st.head

}

// test cases
//println( compute(syard(split("3 + 4 * ( 2 - 1 )"))) )   // 7
//println( compute(syard(split("10 + 12 * 33")))       )// 406
//println( compute(syard(split("( 5 + 7 ) * 2")))      )// 24
//println( compute(syard(split("5 + 7 / 2")))          )// 8
//println( compute(syard(split("5 * 7 / 2")))  )        // 17
//println( compute(syard(split("9 + 24 / ( 7 - 3 )"))) )// 15
//println( compute(syard(split("4 ^ 3 ^ 2"))) )     // 262144
//println( compute(syard(split("4 ^ ( 3 ^ 2 )")))  )// 262144
//println( compute(syard(split("( 4 ^ 3 ) ^ 2"))) ) // 4096
//println( compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   )// 65536
//println( syard(split("4 ^ 3 ^ 2")) )
