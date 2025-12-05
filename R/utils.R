# ============================================================
# Utility Functions for Visualization & Evaluation
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
plot_las_3d <- function(las, palette = palette_veg(), size = 2, title = "LAS 3D View") {
  stopifnot("LAS" %in% class(las))
  
  rgl::open3d()
  rgl::title3d(title)
  rgl::points3d(
    las@data[, 1:3],
    col  = palette[as.character(las$Classification)],
    size = size
  )
}

# ------------------------------------------------------------
# Confusion Matrix + Evaluation Metrics
# ------------------------------------------------------------
#' Evaluate Predicted vs True LAS Labels
#'
#' @param pred LAS with predicted Classification
#' @param truth LAS with true labels (column "label")
#'
#' @return list of confusion matrix, accuracy, precision, recall, F1
#' @export
evaluate_las <- function(pred, truth) {
  stopifnot(nrow(pred@data) == nrow(truth@data))
  
  pred_labels  <- as.integer(pred$Classification)
  true_labels  <- as.integer(truth$label)
  
  cm <- table(True = true_labels, Pred = pred_labels)
  
  # Metrics
  accuracy <- sum(diag(cm)) / sum(cm)
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)
  
  list(
    confusion = cm,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1
  )
}

# ------------------------------------------------------------
# Quick class distribution summary
# ------------------------------------------------------------
#' Class Count Summary
#' @export
class_summary <- function(las) {
  table(las$Classification)
}
