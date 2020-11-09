def renderSudoku(sudoku: Array[Array[Int]]) = {
  val separator = "\n" + ("+" + ("-".repeat(7))).repeat(3) + "+\n"
  def rowString(line: Array[Int]): String = {
    Range(0,3).toList.map { i =>
      Range(0,3).toList.map(j => if(line(i * 3 + j) > 0) line(i * 3 + j).toString() else " ").mkString(" ")
    }.mkString("| ", " | ", " |")
  }

  sudoku.grouped(3).map(row => row.map(rowString).mkString("\n")).mkString(separator, separator, separator)
}