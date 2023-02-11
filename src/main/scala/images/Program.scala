@main
def image_program() = {
  val imageFilePath = "src/resources/image.png"
  val outputFilePath = "output.png"

  save(
    rotateRight(
      grayscale(
        flipHorizontal(
          load(imageFilePath)
        )
      )
    ),
    outputFilePath
  )

}
