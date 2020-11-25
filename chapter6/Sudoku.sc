def isValidSudoku(grid: Array[Array[Int]]): Boolean = {
  !Range(0, 9).exists { i => 
    val notZero = (item: Int) => item != 0
    val row = Range(0,9).map(grid(i)(_)).filter(notZero)
    val col = Range(0,9).map(grid(_)(i)).filter(notZero)
    val square = Range(0,9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filter(notZero)

    row.distinct.length != row.length ||
    col.distinct.length != col.length ||
    square.distinct.length != square.length

  }
}

def solve(grid: Array[Array[Int]]): Boolean = {
  def rec(i: Int, j: Int): Boolean = {
    isValidSudoku(grid) match {
      case true => {
        if(grid(i)(j) == 0) {
          var solved = false
          for(candidate <- Range(1,10) if !solved) {
            grid(i)(j) = candidate
            solved = rec(i,j)
            if(!solved){
              grid(i)(j) = 0

            }
          }
          solved
        } else if(i + 1 < 9){
          rec(i + 1, j)
        } else if(j + 1 < 9) {
          rec(0, j + 1)
        } else {
          true
        }
      }
      case false => false
    }
  }

  rec(0,0)
}

// BOOK CODE
val puzzle = Array(
  Array(3, 0, 6,   5, 0, 8,   4, 0, 0),
  Array(5, 2, 0,   0, 0, 0,   0, 0, 0),
  Array(0, 8, 7,   0, 0, 0,   0, 3, 1),

  Array(0, 0, 3,   0, 1, 0,   0, 8, 0),
  Array(9, 0, 0,   8, 6, 3,   0, 0, 5),
  Array(0, 5, 0,   0, 9, 0,   6, 0, 0),

  Array(1, 3, 0,   0, 0, 0,   2, 5, 0),
  Array(0, 0, 0,   0, 0, 0,   0, 7, 4),
  Array(0, 0, 5,   2, 0, 6,   3, 0, 0)
)

val solution =  Array(
  Array(3, 1, 6,   5, 7, 8,   4, 9, 2),
  Array(5, 2, 9,   1, 3, 4,   7, 6, 8),
  Array(4, 8, 7,   6, 2, 9,   5, 3, 1),

  Array(2, 6, 3,   4, 1, 5,   9, 8, 7),
  Array(9, 7, 4,   8, 6, 3,   1, 2, 5),
  Array(8, 5, 1,   7, 9, 2,   6, 4, 3),

  Array(1, 3, 8,   9, 4, 7,   2, 5, 6),
  Array(6, 9, 2,   3, 5, 1,   8, 7, 4),
  Array(7, 4, 5,   2, 8, 6,   3, 1, 9)
)

val success = solve(puzzle)
pprint.log(success)
pprint.log(puzzle)
assert(success)
assert(isValidSudoku(puzzle))
assert(puzzle.flatten[Int].sameElements(solution.flatten[Int]))