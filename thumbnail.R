# To add a thumbnail:
# 1. change the URL below
# 2. go to the blockbuilder page, for example:
# https://blockbuilder.org/jtr13/98bbc5b0d5fb6b5c07ec46e2bef9b706
# 3. Click thumbnail.png
# 4. Upload the image, save, and save on the top as well or it doesn't work


library(webshot2)
url <- "https://bl.ocks.org/jtr13/98bbc5b0d5fb6b5c07ec46e2bef9b706/16f1d144eeb8a3a45b9ca9497cef484b8ea16b7d"
webshot(url, cliprect = c(10, 150, 500, 400), file = "~/Downloads/thumbnail.png", zoom = 2)
