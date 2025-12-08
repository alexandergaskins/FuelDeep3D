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


# ============================================================
# Confusion Matrix + Evaluation Metrics (Improved)
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
evaluate_single_las <- function(las, truth_col = "label", pred_col = "Classification") {
  stopifnot("LAS" %in% class(las))
  
  # Check fields
  if (!(truth_col %in% names(las@data)))
    stop(paste("Truth column", truth_col, "not found in LAS."))
  
  if (!(pred_col %in% names(las@data)))
    stop(paste("Prediction column", pred_col, "not found in LAS."))
  
  true_labels <- as.integer(las@data[[truth_col]])
  pred_labels <- as.integer(las@data[[pred_col]])
  
  # Confusion matrix
  cm <- table(True = true_labels, Pred = pred_labels)
  
  # Metrics
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
# Print confusion matrix in aligned table form
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
# Print per-class Precision / Recall / F1 in a table + summary row
# ------------------------------------------------------------
#' Pretty-print evaluation metrics table with summary row
#'
#' @param results List returned by evaluate_single_las()
#' @export
print_metrics_table <- function(results) {
  
  precision <- results$precision
  recall    <- results$recall
  f1        <- results$f1
  
  classes <- names(precision)
  
  # Per-class metrics table
  df <- data.frame(
    Class     = classes,
    Precision = round(precision, 4),
    Recall    = round(recall, 4),
    F1_Score  = round(f1, 4)
  )
  
  # Add summary row ("All" classes)
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
