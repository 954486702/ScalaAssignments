// Part 1 about finding Knight's tours
//=====================================

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end are of any help.



type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if(x._1 >= dim || x._2 >= dim || x._2<0 || x._1<0 || path.contains(x) ) false
  else true
}



//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val Y = Array(2, 1, -1, -2, -2, -1, 1, 2)
  val X = Array(1, 2, 2, 1, -1, -2, -2, -1)
  val possibleMoves = for(n <- 0 to 7) yield ( x._1+X(n), x._2+Y(n) )
  val legalMoves = possibleMoves.filter(is_legal(dim,path,_))
  legalMoves.toList
}

//println( legal_moves(8, Nil, (2,2)) == List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)) )
//  println(  legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)) )
//  println(legal_moves(8, List((4,1), (1,0)), (2,2)) == List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)) )
//  println(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)) )
//  println(legal_moves(1, Nil, (0,0)) == Nil )
//  println(legal_moves(2, Nil, (0,0)) == Nil )
//  println(legal_moves(3, Nil, (0,0)) == List((1,2), (2,1)) )
//some test cases
//
//assert(legal_moves(8, Nil, (2,2)) ==
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(
 // println( legal_moves(8, List((4,1), (1,0)), (2,2)) ) // ==
    //List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

def count_tours(dim: Int, path: Path) : Int = {
  if(path.length == dim*dim ) {
    1
  }
  else {
    //val legalMoves = (for (move <- path) yield legal_moves(dim, path, move)).flatten
    //val legalMoves = legal_moves(dim, path, path.head)
    //val legalMoves = for( move <- legal_moves(dim, path,path.head)) yield move

    (for (move <- legal_moves(dim, path, path.head)) yield count_tours(dim, move :: path) ).sum
  }

}
//println( count_tours(55, List((0,0)) ) )
//println( enum_tours(8, List((0,0)) ) )

def enum_tours(dim: Int, path: Path) : List[Path] = {
  if(path.length == dim*dim ) {
    List(path)
  }
  else {
    //val legalMoves = (for (move <- path) yield legal_moves(dim, path, move)).flatten
    //val legalMoves = legal_moves(dim, path, path.head)
    //val legalMoves = for( move <- legal_moves(dim, path,path.head)) yield move

    val l = for (move <- legal_moves(dim, path, path.head)) yield enum_tours(dim, move :: path)
    //println(l)
    //println(l.flatten.length)

    l.flatten

  }
  //List( List((5,5),(4,4)),List((5,5),(3,3)))
}


//(5) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if( xs == Nil ) None
  else {
    val check = f(xs.head)
    if (check != None) check
    else first(xs.tail,f)
  }
}


// test cases
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None

//println( first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo) )  // Some(List((4,0)))
//println( first(List((1, 0),(2, 0),(3, 0)), foo) )         // None


//(6) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.

//println( first_tour(9, List((0,0))) )
//
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if(path.length == dim*dim ) {
//    println(Some(path))
    Some(path)

  }
  else {
    //for (move <- legal_moves(dim, path, path.head)) yield first_tour(dim, move :: path)
    first(legal_moves(dim, path, path.head), (x: (Int, Int)) => first_tour(dim, x :: path))
    //None


  }
}




/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//     time_needed(count_tours(dim, List((0, 0))))
// in order to print out the time that is needed for 
// running count_tours

// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println
  } 
}


*/
