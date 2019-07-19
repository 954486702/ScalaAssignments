import scala.annotation.tailrec
import scala.collection.immutable
// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================


// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (6) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 
 def is_op(op: String) : Boolean = ops.contains(op)
 def prec(op1: String, op2: String) : Boolean = precs(op1) >= precs(op2)

  def collect(op: String, st: Toks, mv: Toks = Nil) : Toks =  {
    if( st != Nil && is_op(st.head) && prec(st.head, op) ) {/*println(st.tail);*/ collect(op,st.tail, mv ::: List(st.head)) }
    else mv
  }

//println(collect( "*", List("*","/","+", "+", "+","-","/")  ))
//@tailrec
// def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = /*toks.head match*/ {
//  if(toks != Nil) toks.head match{
// case "+" => if (collect ("+", st) != Nil) syard(toks.tail, List("+") ::: st.takeRight (st.length - collect ("+", st).length), out ::: collect ("+", st) )
// else println(st); syard(toks.tail, List("+") ::: st, out) //st//placeholder for now
//
// case "-" => if (collect ("-", st) != Nil) syard(toks.tail, List("-") :::st.takeRight (st.length - collect ("-", st).length), out ::: collect ("-", st) )
// else println(st); syard(toks.tail, List("-") ::: st, out) //st//placeholder for now
//
// case "*" => if (collect ("*", st) != Nil) syard(toks.tail, List("*") :::st.takeRight (st.length - collect ("*", st).length), out ::: collect ("*", st) )
// else println("*: " +st); syard(toks.tail, List("*") ::: st, out) //st//placeholder for now
//
// case "/" => if (collect ("/", st) != Nil) { syard(toks.tail, List("/") :::st.takeRight(st.length - collect ("/", st).length), out ::: collect ("/", st) ) }
// else println("/: " +st); syard(toks.tail, List("/") ::: st, out) //st//placeholder for now
// case "(" => syard (toks.tail, List ("(") ::: st, out)
// case ")" => syard (toks.tail, st.takeRight (st.length - (st.indexOf ("(") + 1) ), out ::: st.slice (0, st.indexOf ("(") ) )
// case num => if(toks.length==1) { println(out ::: List(num) ::: st); syard(Nil, Nil, out ::: List(num) ::: st) } else println("num: "+num); println("st: " +st); println(out); println("toks: "+toks); println(); syard(toks.tail, st, out ::: List(num) )
// }
//     else out ::: st
// }

@tailrec
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
  case "(" => syard (toks.tail, List ("(") ::: st, out)
  case ")" => syard (toks.tail, st.takeRight (st.length - (st.indexOf ("(") + 1) ), out ::: st.slice (0, st.indexOf ("(") ) )
  case num => if(toks.length==1) syard(Nil, Nil, out ::: List(num) ::: st)  else syard(toks.tail, st, out ::: List(num) )
 }
 else out ::: st
}

//println(st.length - collect ("/", st).length);
// test cases
//println( syard(split("3 + 4 * ( 2 - 1 )"))  )// 3 4 2 1 - * +
//println( syard(split("10 + 12 * 33"))  )     // 10 12 33 * +
//println( syard(split("( 5 + 7 ) * 2"))      )// 5 7 + 2 *
//println( syard(split("5 + 7 / 2")) )         // 5 7 2 / +
//
//println( syard(split("5 * 7 / 2"))  )        // 5 7 * 2 /
//
//println( syard(split("9 + 24 / ( 7 - 3 )")) )// 9 24 7 3 - / +
//
//println( syard(split("3 + 4 + 5")) )          // 3 4 + 5 +
//println( syard(split("( ( 3 + 4 ) + 5 )")) )   // 3 4 + 5 +
//println( syard(split("( 3 + ( 4 + 5 ) )")) )   // 3 4 5 + +
//println( syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) )// 3 4 5 + +

 
// (7) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    
@tailrec
 def compute(toks: Toks, st: List[Int] = Nil) : Int = {
  if(toks!= Nil) toks.head match{
   case "+" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)+st.last ) )
   case "-" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)-(st.last) ) )
   case "*" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)*(st.last) ) )
   case "/" =>  compute(toks.tail,  st.take(st.length-2) ::: List(st(st.length-2)/(st.last) ) )
   case num =>  compute(toks.tail, st ::: List( num.toInt ))
  }
  else st.head

 }


// test cases
println( compute(syard(split("3 + 4 * ( 2 - 1 )")))  )// 7
println( compute(syard(split("10 + 12 * 33")))  )     // 406
println( compute(syard(split("( 5 + 7 ) * 2")))   )   // 24
 println( compute(syard(split("5 + 7 / 2")))   )       // 8
 println( compute(syard(split("5 * 7 / 2")))  )        // 17
 println( compute(syard(split("9 + 24 / ( 7 - 3 )"))) )// 15




