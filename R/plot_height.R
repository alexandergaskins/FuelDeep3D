#' Plot a 3D LAS point cloud with elevation color ramp and legend
#'
#' @param las A LAS object (from lidR).
#' @param colors A vector of colors for the elevation ramp.
#' @param point_size Numeric. Size of plotted points.
#' @param zoom Numeric. rgl zoom level.
#'
#' @return Invisibly returns NULL.
#' @export
plot_las_elevation_3d <- function(
    las,
    colors = c("blue", "green", "yellow", "red"),
    point_size = 0.5,
    zoom = 0.6
) {
  
  if (!inherits(las, "LAS")) {
    stop("Input must be a LAS object.")
  }
  
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' is required.")
  }
  
  # Normalize coordinates
  X <- las$X - min(las$X)
  Y <- las$Y - min(las$Y)
  Z <- las$Z - min(las$Z)
  
  # Color ramp helper
  myColorRamp <- function(colors, values) {
    v <- (values - min(values)) / diff(range(values))
    x <- grDevices::colorRamp(colors)(v)
    grDevices::rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  col <- myColorRamp(colors, Z)
  
  rgl::rgl.clear()
  rgl::bg3d("white")
  
  rgl::points3d(X, Y, Z, col = col, size = point_size)
  
  # Color bar
  xpos <- max(X) + 0.05 * max(X)
  ypos <- min(Y)
  
  zseq <- seq(min(Z), max(Z), length.out = 200)
  cols_bar <- myColorRamp(colors, zseq)
  
  for (i in seq_len(length(zseq) - 1)) {
    rgl::quads3d(
      c(xpos, xpos + 0.02 * max(X), xpos + 0.02 * max(X), xpos),
      c(ypos, ypos, ypos, ypos),
      c(zseq[i], zseq[i], zseq[i+1], zseq[i+1]),
      col = cols_bar[i],
      lit = FALSE
    )
  }
  
  rgl::text3d(
    xpos + 0.04 * max(X),
    ypos,
    min(Z),
    texts = round(min(Z), 2),
    adj = c(0,0)
  )
  
  rgl::text3d(
    xpos + 0.04 * max(X),
    ypos,
    max(Z),
    texts = round(max(Z), 2),
    adj = c(0,1)
  )
  
  rgl::view3d(theta = 0, phi = -90, zoom = zoom)
  
  invisible(NULL)
}