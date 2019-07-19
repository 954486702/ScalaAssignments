// Part 2 about finding a single tour for a board using the Warnsdorf Rule
//=========================================================================

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if(x._1 >= dim || x._2 >= dim || x._2<0 || x._1<0 || path.contains(x) ) false
  else true
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val Y = Array(2, 1, -1, -2, -2, -1, 1, 2)
  val X = Array(1, 2, 2, 1, -1, -2, -2, -1)
  val possibleMoves = for(n <- 0 to 7) yield ( x._1+X(n), x._2+Y(n) )
  val legalMoves = possibleMoves.filter(is_legal(dim,path,_))
  legalMoves.toList
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val toOrder = legal_moves(dim, path, x)
  val order = for( move <- toOrder) yield (move,legal_moves(dim, move :: path, move).length)
  val sorted = order.sortBy(_._2)
  val warnsdoffOrder = for(s <- sorted) yield s._1
  warnsdoffOrder

}
//println( ordered_moves(80, List((0,0)), (40,40) ) )

//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if( xs == Nil ) None
  else {
    val check = f(xs.head)
    if (check != None) check
    else first(xs.tail,f)
  }
}

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  if(path.length == dim*dim && ( Math.abs(path.head._1 - path.last._1)*(path.head._2 - path.last._2) == 2 ) ){
    Some(path)
  }
  else{
    first( ordered_moves(dim,path,path.head), (x: (Int,Int)) => first_closed_tour_heuristic(dim, x :: path)  )
  }
}
//println( first_closed_tour_heuristic(6,List((3,3)) ) )


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  if(path.length == dim*dim ){
    Some(path)
  }
  else{
    first( ordered_moves(dim,path,path.head), (x: (Int,Int)) => first_tour_heuristic(dim, x :: path)  )
  }
}

//println( first_tour_heuristic(8,List((4,4)) ) )
