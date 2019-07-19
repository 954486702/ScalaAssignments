import scala.annotation.tailrec
// Finding a single tour on a "mega" board
//=========================================


// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

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

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if( xs == Nil ) None
  else {
    val check = f(xs.head)
    if (check != None) check
    else first(xs.tail,f)
  }
}

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
  tour_on_mega_boardH(dim, List(path) )

}


@tailrec
def tour_on_mega_boardH(dim: Int, paths: List[Path]): Option[Path] =paths match {
  case Nil => None
  case head :: tail => if( head.length == dim*dim) Some(head)
    //val a = for(move <- ordered_moves(dim, head, head.head )) yield move :: head
    else tour_on_mega_boardH(dim, for(move <- ordered_moves(dim, head, head.head )) yield move :: head)

}

