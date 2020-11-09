def partialValidSudoku(grid: Array[Array[Int]]): Boolean = {
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