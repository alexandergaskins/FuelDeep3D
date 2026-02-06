library(lidR)
library(rgl)

LASfile <- "C:/Users/alexandergaskins/Documents/FuelDeep3D/inst/extdata/las/trees.laz"
rLAS <- readLAS(LASfile)

myColorRamp <- function(colors, values) {
  v <- (values - min(values)) / diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

zvals <- rLAS$Z
col <- myColorRamp(c("blue", "green", "yellow", "red"), zvals)

X <- rLAS$X - min(rLAS$X)
Y <- rLAS$Y - min(rLAS$Y)
Z <- rLAS$Z - min(rLAS$Z)

rgl.clear()
bg3d("white")

points3d(X, Y, Z, col = col, size = 0.5)

xpos <- max(X) + 0.05 * max(X)
ypos <- min(Y)

zseq <- seq(min(Z), max(Z), length.out = 200)
cols_bar <- myColorRamp(c("blue", "green", "yellow", "red"), zseq)

for (i in 1:(length(zseq)-1)) {
  quads3d(
    c(xpos, xpos + 0.02 * max(X), xpos + 0.02 * max(X), xpos),
    c(ypos, ypos, ypos, ypos),
    c(zseq[i], zseq[i], zseq[i+1], zseq[i+1]),
    col = cols_bar[i],
    lit = FALSE
  )
}

text3d(xpos + 0.04 * max(X), ypos, min(Z),
       texts = round(min(Z),2), adj = c(0,0))

text3d(xpos + 0.04 * max(X), ypos, max(Z),
       texts = round(max(Z),2), adj = c(0,1))

view3d(theta = 0, phi = -90, zoom = 0.55)