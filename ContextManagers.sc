import java.io.BufferedWriter
import java.io.BufferedReader
import java.nio.file.Files.newBufferedReader
import java.nio.file.Files.newBufferedWriter
import java.nio.file.FileSystems

def withFileWriter[T](filename: String)(cb: BufferedWriter => T): Unit = {
  val writer = newBufferedWriter(FileSystems.getDefault().getPath(filename))
  try cb(writer)
  finally writer.close()
}

def withFileReader[T](filename: String)(cb: BufferedReader => T): T = {
  val reader = newBufferedReader(FileSystems.getDefault().getPath(filename))
  try cb(reader)
  finally reader.close()
}

withFileWriter("File.txt") { writer => 
  writer.write("Hello\n")
  writer.write("World\n")
}

val text = withFileReader("File.txt") { reader => 
  reader.readLine() + "\n" + reader.readLine()
}

println(text)