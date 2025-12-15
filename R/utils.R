# ============================================================
# Utility Functions for Visualization, Evaluation & Preprocessing
# FuelDeep3D — utils.R
# ============================================================

# ------------------------------------------------------------
# Default vegetation color palette
# ------------------------------------------------------------
#' Default vegetation color palette
#' @export
palette_veg <- function() {
  c(
    "0" = "yellow",
    "1" = "blue",
    "2" = "sienna",
    "3" = "gray40",
    "4" = "darkgreen"
  )
}

# ------------------------------------------------------------
# 3D LAS Visualization
# ------------------------------------------------------------
#' 3D Visualization of LAS Using RGL
#'
#' @param las LAS object
#' @param palette Color palette for classes
#' @param size Point size
#' @param title Plot title
#'
#' @export
plot_las_3d <- function(las,
                        palette = palette_veg(),
                        size = 2,
                        title = "LAS 3D View") {

  stopifnot(inherits(las, "LAS"))

  rgl::open3d()
  rgl::title3d(title)

  rgl::points3d(
    las@data[, c("X", "Y", "Z")],
    col  = palette[as.character(las$Classification)],
    size = size
  )
}

# ============================================================
# Noise Filtering Utilities
# ============================================================

#' Remove sparse outlier points using Statistical Outlier Removal (SOR)
#'
#' Applies a k-nearest-neighbor–based Statistical Outlier Removal (SOR)
#' filter to points above a user-defined height threshold. Points below
#' the threshold are preserved.
#'
#' @param las A LAS object.
#' @param height_thresh Numeric. Height (meters) above which filtering is applied.
#' @param k Integer. Number of nearest neighbors.
#' @param zscore Numeric. Standard deviation multiplier.
#'
#' @return A filtered LAS object.
#'
#' @examples
#' \dontrun{
#' las <- readLAS("trees.laz")
#' las_clean <- remove_noise_sor(las, height_thresh = 5, k = 20, zscore = 2.5)
#' }
#'
#' @export
remove_noise_sor <- function(las,
                             height_thresh = 5,
                             k = 20,
                             zscore = 2.5) {

  stopifnot(inherits(las, "LAS"))

  if (lidR::is.empty(las)) {
    return(las)
  }

  if (!is.numeric(height_thresh) || height_thresh < 0)
    stop("height_thresh must be a non-negative numeric value.")

  if (!is.numeric(k) || k < 1)
    stop("k must be a positive integer.")

  if (!is.numeric(zscore) || zscore <= 0)
    stop("zscore must be a positive numeric value.")

  # Split points
  high_pts <- las[las$Z > height_thresh]
  low_pts  <- las[las$Z <= height_thresh]

  if (lidR::npoints(high_pts) < k) {
    return(las)
  }

  # kNN distances
  xyz <- cbind(high_pts$X, high_pts$Y, high_pts$Z)
  knn_dist <- dbscan::kNNdist(xyz, k = k)

  d_mean <- rowMeans(knn_dist)

  mu <- mean(d_mean)
  sigma <- stats::sd(d_mean)

  if (is.na(sigma) || sigma == 0) {
    return(las)
  }

  keep <- d_mean < (mu + zscore * sigma)

  high_clean <- high_pts[keep]

  rbind(low_pts, high_clean)
}

# ============================================================
# Confusion Matrix + Evaluation Metrics
# ============================================================

#' Evaluate LAS Using Custom Field Names
#'
#' Computes confusion matrix, accuracy, per-class precision, recall, and F1 scores.
#'
#' @param las LAS object containing both truth and prediction fields
#' @param truth_col Name of the ground-truth label field (default = "label")
#' @param pred_col  Name of the predicted label field (default = "Classification")
#'
#' @return list with confusion matrix, accuracy, precision, recall, F1
#' @export
evaluate_single_las <- function(las,
                                truth_col = "label",
                                pred_col  = "Classification") {

  stopifnot(inherits(las, "LAS"))

  if (!(truth_col %in% names(las@data)))
    stop(paste("Truth column", truth_col, "not found in LAS."))

  if (!(pred_col %in% names(las@data)))
    stop(paste("Prediction column", pred_col, "not found in LAS."))

  true_labels <- as.integer(las@data[[truth_col]])
  pred_labels <- as.integer(las@data[[pred_col]])

  cm <- table(True = true_labels, Pred = pred_labels)

  accuracy  <- sum(diag(cm)) / sum(cm)
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)

  list(
    confusion = cm,
    accuracy  = accuracy,
    precision = precision,
    recall    = recall,
    f1        = f1
  )
}

# ------------------------------------------------------------
# Pretty-print confusion matrix
# ------------------------------------------------------------
#' Pretty-print confusion matrix
#'
#' @param cm Confusion matrix (table)
#' @export
print_confusion_matrix <- function(cm) {
  df <- as.data.frame.matrix(cm)
  print(df)
}

# ------------------------------------------------------------
# Pretty-print Precision / Recall / F1 table
# ------------------------------------------------------------
#' Pretty-print evaluation metrics table with summary row
#'
#' @param results List returned by evaluate_single_las()
#' @export
print_metrics_table <- function(results) {

  precision <- results$precision
  recall    <- results$recall
  f1        <- results$f1

  df <- data.frame(
    Class     = names(precision),
    Precision = round(precision, 4),
    Recall    = round(recall, 4),
    F1_Score  = round(f1, 4)
  )

  df <- rbind(
    df,
    data.frame(
      Class     = "Overall",
      Precision = round(mean(precision, na.rm = TRUE), 4),
      Recall    = round(mean(recall, na.rm = TRUE), 4),
      F1_Score  = round(mean(f1, na.rm = TRUE), 4)
    )
  )

  rownames(df) <- NULL
  print(df)
}
