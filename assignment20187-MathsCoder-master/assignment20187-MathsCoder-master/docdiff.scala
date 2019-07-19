import scala.collection.JavaConverters._
import scala.util.Try
// Part 1 about Code Similarity
//==============================

//(1) Complete the clean function below. It should find
//    all words in a string using the regular expression
//    \w+  and the library function 
//
//         some_regex.findAllIn(some_string)
//
//    The words should be Returned as a list of strings.


def clean(s: String) : List[String] = {
  val words = """\w+""".r.findAllIn(s).toList
  //println(words)
  words
}
  clean("gh Hi, hi, Hi, Hi").distinct


//(2) The function occurrences calculates the number of times  
//    strings occur in a list of strings. These occurrences should 
//    be calculated as a Map from strings to integers.


def occurrences(xs: List[String]): Map[String, Int] = {
  val oMap = xs.map(s => (s, listOccurrences(s,xs)))
  oMap.toMap
}

def listOccurrences(s: String, xs: List[String]): Int = {
  val count = for (x <- xs;
                    if x == s) yield x
  count.length
}

//println( occurrences( clean("gh Hi, hi, Hi, Hi") ) )

//(3) This functions calculates the dot-product of two documents
//    (list of strings). For this it calculates the occurrence
//    maps from (2) and then multiplies the corresponding occurrences. 
//    If a string does not occur in a document, the product is zero.
//    The function finally sums up all products. 


def prod(lst1: List[String], lst2: List[String]) : Int = {
  val combined = lst1 ::: lst2
  val combinedU = combined.distinct
  //println(combinedU)
  val m1 = occurrences(lst1)
  val m2 = occurrences(lst2)

  val prodSum = for(key <- combinedU) yield m1.getOrElse(key,0) * m2.getOrElse(key, 0)

  prodSum.sum
}
//println( prod(List("3", "dg", "df", "rg"), List("fd", "3")) )

//(4) Complete the functions overlap and similarity. The overlap of
//    two documents is calculated by the formula given in the assignment
//    description. The similarity of two strings is given by the overlap
//    of the cleaned strings (see (1)).  


def overlap(lst1: List[String], lst2: List[String]) : Double = {
  val dProd = prod(lst1, lst2)
  val prodLst1 = prod(lst1, lst1)
  val prodLst2 = prod(lst2, lst2)
  val mProd = List(prodLst1, prodLst2).max
  val overlap = dProd/mProd.toDouble
  overlap

}

def similarity(s1: String, s2: String) : Double = {
  overlap( clean(s1), clean(s2) )
}

//println( similarity("dddddd ddddddf", "ddddddf" )  )


 //Test cases


val list1 = List("a", "b", "b", "c", "d") 
val list2 = List("d", "b", "d", "b", "d")

println( occurrences(List("a", "b", "b", "c", "d"))  ) // Map(a -> 1, b -> 2, c -> 1, d -> 1)
println( occurrences(List("d", "b", "d", "b", "d")) )   // Map(d -> 3, b -> 2)

println(prod(list1,list2)) // 7

  println(overlap(list1, list2) )  // 0.5384615384615384
  println(overlap(list2, list1)  ) // 0.5384615384615384
  println(overlap(list1, list1)   )// 1.0
  println(overlap(list2, list2)   )// 1.0

// Plagiarism examples from 
// https://desales.libguides.com/avoidingplagiarism/examples

val orig1 = """There is a strong market demand for eco-tourism in
Australia. Its rich and diverse natural heritage ensures Australia's
capacity to attract international ecotourists and gives Australia a
comparative advantage in the highly competitive tourism industry."""

val plag1 = """There is a high market demand for eco-tourism in
Australia. Australia has a comparative advantage in the highly
competitive tourism industry due to its rich and varied natural
heritage which ensures Australia's capacity to attract international
ecotourists."""

similarity(orig1, plag1) // 0.8679245283018868


// Plagiarism examples from 
// https://www.utc.edu/library/help/tutorials/plagiarism/examples-of-plagiarism.php

val orig2 = """No oil spill is entirely benign. Depending on timing and
location, even a relatively minor spill can cause significant harm to
individual organisms and entire populations. Oil spills can cause
impacts over a range of time scales, from days to years, or even
decades for certain spills. Impacts are typically divided into acute
(short-term) and chronic (long-term) effects. Both types are part of a
complicated and often controversial equation that is addressed after
an oil spill: ecosystem recovery."""

val plag2 = """There is no such thing as a "good" oil spill. If the
time and place are just right, even a small oil spill can cause damage
to sensitive ecosystems. Further, spills can cause harm days, months,
years, or even decades after they occur. Because of this, spills are
usually broken into short-term (acute) and long-term (chronic)
effects. Both of these types of harm must be addressed in ecosystem
recovery: a controversial tactic that is often implemented immediately
following an oil spill."""

overlap(clean(orig2), clean(plag2))  // 0.728
similarity(orig2, plag2)             // 0.728


 
// The punchline: everything above 0.6 looks suspicious and 
// should be investigated by staff.

//*/
