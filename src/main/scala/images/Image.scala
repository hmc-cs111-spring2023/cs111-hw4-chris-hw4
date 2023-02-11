import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
import java.awt.Color

/** *****************************************************************************
  * Flipping
  */

/** Flip an image along its horizontal axis
  */
def flipHorizontal(imageFilename: String, outputFilename: String): Unit = {
  val image = load(imageFilename)

  val width = image.getWidth
  val height = image.getHeight
  val flipped = new BufferedImage(width, height, image.getType)
  for (x <- 0 until width; y <- 0 until height) {
    flipped.setRGB(width - x - 1, y, image.getRGB(x, y))
  }

  save(flipped, outputFilename)
}

/** Flip an image along its vertical axis
  */
def flipVertical(imageFilename: String, outputFilename: String): Unit = {
  val image = load(imageFilename)

  val width = image.getWidth
  val height = image.getHeight
  val flipped = new BufferedImage(width, height, image.getType)
  for (x <- 0 until width; y <- 0 until height) {
    flipped.setRGB(x, height - y - 1, image.getRGB(x, y))
  }

  save(flipped, outputFilename)
}

/** *****************************************************************************
  * Rotating
  */

/** Rotate an image 90 degrees clockwise
  */
def rotateRight(imageFilename: String, outputFilename: String): Unit = {
  val image = load(imageFilename)

  val width = image.getWidth
  val height = image.getHeight
  val rotated = new BufferedImage(height, width, image.getType)
  for (x <- 0 until width; y <- 0 until height) {
    rotated.setRGB(height - y - 1, x, image.getRGB(x, y))
  }

  save(rotated, outputFilename)
}

/** Rotate an image 90 degrees counter-clockwise
  */
def rotateLeft(imageFilename: String, outputFilename: String): Unit = {
  val image = load(imageFilename)

  val width = image.getWidth
  val height = image.getHeight
  val rotated = new BufferedImage(height, width, image.getType)
  for (x <- 0 until width; y <- 0 until height) {
    rotated.setRGB(y, width - x - 1, image.getRGB(x, y))
  }

  save(rotated, outputFilename)
}

/** *****************************************************************************
  * Grayscale
  */

/** Convert an image to grayscale
  */
def grayscale(imageFilename: String, outputFilename: String): Unit = {
  val image = load(imageFilename)

  // create a new, empty image to copy pixels into
  val width = image.getWidth
  val height = image.getHeight
  val imageType = image.getType
  val result = new BufferedImage(width, height, imageType)

  // copy the pixels over, column-by-column, grayscaling each pixel
  for (column <- 0 until width)
    for (row <- 0 until height) {
      // we gray by averaging the red, green, and blue parts of the pixel
      val pixel = new Color(image.getRGB(column, row))
      val gray: Int =
        Math.round((pixel.getRed + pixel.getGreen + pixel.getBlue) / 3f)
      val newPixel = new Color(gray, gray, gray)
      result.setRGB(column, row, newPixel.getRGB)
    }

  save(result, outputFilename)
}

/** *****************************************************************************
  * Saving and loading
  */

/** Given a path to an image file, loads an image from that file
  * @param filename
  *   the path to the image file
  * @return
  *   a `BufferedImage` object that contains the image data loaded from the file
  * @throws FileNotFoundException
  *   if the file doesn't exist
  * @throws IllegalArgumentException
  *   if the file doesn't contain an image
  */
def load(filename: String): BufferedImage = {
  val inputStream = new FileInputStream(filename)
  val image = ImageIO.read(inputStream)

  // if the file is not an image file, then image is null (which is bad)
  require(image != null, "The file is not an image")

  // create a `BufferedImage` object from the image data
  val bufferedImage =
    new BufferedImage(image.getWidth, image.getHeight, image.getType)
  bufferedImage.setData(image.getData)

  bufferedImage
}

/** Given a `BufferedImage` object and path to a file, saves the image to the
  * `filename`. By default, the image format is PNG
  * @param format
  *   "png" by default
  * @return
  *   true if the file was successfully written
  * @throws IllegalArgumentException
  *   if any of the arguments are `null`
  * @throws IOException
  *   if there's an error writing the file
  */
def save(
    image: BufferedImage,
    filename: String,
    format: String = "png"
): Boolean = {
  ImageIO.write(image, format, new File(filename))
}
