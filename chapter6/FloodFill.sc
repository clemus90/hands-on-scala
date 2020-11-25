import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import scala.collection.mutable.ArrayDeque

def pointsToFlood(raw: BufferedImage, origin: (Int, Int), comp: (Color, Color) => Boolean): Set[(Int, Int)] = {
  val queue = collection.mutable.ArrayDeque(origin)
  val seen = collection.mutable.Set[(Int, Int)]()
  val result = Set.newBuilder[(Int, Int)]
  result += origin

  val width = raw.getWidth
  val height = raw.getHeight

  while(queue.nonEmpty) {
    val (x, y) = queue.removeHead()
    val originColor = new Color(raw.getRGB(x, y))
    val adjacent = for {
      deltaX <- Range(-1, 2)
      deltaY <- Range(-1, 2)
      if (deltaX, deltaY) != (0,0)
      (x1, y1) = (x + deltaX, y + deltaY)
      if x1 >= 0 && x1 <= width && y1 >= 0 && y1 <= height && !seen.contains((x1, y1))
      destColor = new Color(raw.getRGB(x + deltaX, y + deltaY))
      if comp(originColor, destColor)
    } {
      queue.append((x1, y1))
      seen.add((x1, y1))
      result += ((x1, y1))
    }
  }
  result.result()
}

def floodFill(
  src: String,
  dest: String,
  startX: Int,
  startY: Int,
  compareColors: (Color, Color) => Boolean,
  fillColor: Color
): Unit = {
  val raw = ImageIO.read(new File(src))
  val toChange = pointsToFlood(raw, (startX, startY), compareColors)
  val colorInt = fillColor.getRGB
  for ((x, y) <- toChange) {
    raw.setRGB(x, y, colorInt)
  }
  ImageIO.write(raw, "jpg", new File(dest))
}