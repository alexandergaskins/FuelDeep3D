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

  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').")
  }
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' is required for 3D plotting. Install it with install.packages('rgl').")
  }

  stopifnot(inherits(las, "LAS"))

  rgl::open3d()
  rgl::title3d(title)

  # Prefer standard LAS column names
  xyz <- las@data[, c("X", "Y", "Z"), drop = FALSE]

  rgl::points3d(
    xyz,
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
#' library(lidR)
#' las <- readLAS("trees.laz")
#' las_clean <- remove_noise_sor(las, height_thresh = 5, k = 20, zscore = 2.5)
#' }
#'
#' @export
remove_noise_sor <- function(las,
                             height_thresh = 5,
                             k = 20,
                             zscore = 2.5) {

  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').")
  }
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Install it with install.packages('dbscan').")
  }

  stopifnot(inherits(las, "LAS"))

  if (lidR::is.empty(las)) return(las)

  if (!is.numeric(height_thresh) || length(height_thresh) != 1 || height_thresh < 0)
    stop("height_thresh must be a single non-negative numeric value.")

  if (!is.numeric(k) || length(k) != 1 || k < 1)
    stop("k must be a single positive integer.")

  if (!is.numeric(zscore) || length(zscore) != 1 || zscore <= 0)
    stop("zscore must be a single positive numeric value.")

  # Split points
  high_pts <- las[las$Z > height_thresh]
  low_pts  <- las[las$Z <= height_thresh]

  if (lidR::npoints(high_pts) < k) return(las)

  # kNN distances
  xyz <- cbind(high_pts$X, high_pts$Y, high_pts$Z)
  knn_dist <- dbscan::kNNdist(xyz, k = k)
  d_mean <- rowMeans(knn_dist)

  mu <- mean(d_mean)
  sigma <- stats::sd(d_mean)

  if (!is.finite(sigma) || sigma == 0) return(las)

  keep <- d_mean < (mu + zscore * sigma)
  high_clean <- high_pts[keep]

  # Combine back
  if (exists("rbind_las", where = asNamespace("lidR"), inherits = FALSE)) {
    lidR::rbind_las(low_pts, high_clean)
  } else {
    rbind(low_pts, high_clean)
  }
}

# ============================================================
# Confusion Matrix + Evaluation Metrics
# ============================================================

# ------------------------------------------------------------
# Internal helper: compute metrics from confusion matrix
# ------------------------------------------------------------
metrics_from_cm <- function(cm) {
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)

  # Replace Inf/NaN due to division by zero
  precision[!is.finite(precision)] <- NA_real_
  recall[!is.finite(recall)]       <- NA_real_
  f1[!is.finite(f1)]               <- NA_real_

  list(precision = precision, recall = recall, f1 = f1)
}

#' Evaluate a single LAS using custom field names
#'
#' Computes confusion matrix, accuracy, and per-class precision/recall/F1
#' from one LAS file that contains both truth and prediction fields.
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

  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').")
  }

  stopifnot(inherits(las, "LAS"))

  if (!(truth_col %in% names(las@data)))
    stop(paste("Truth column", shQuote(truth_col), "not found in LAS."))

  if (!(pred_col %in% names(las@data)))
    stop(paste("Prediction column", shQuote(pred_col), "not found in LAS."))

  true_labels <- as.integer(las@data[[truth_col]])
  pred_labels <- as.integer(las@data[[pred_col]])

  if (length(true_labels) != length(pred_labels))
    stop("Truth and prediction vectors have different lengths.")

  cm <- table(True = true_labels, Pred = pred_labels)
  acc <- sum(diag(cm)) / sum(cm)

  m <- metrics_from_cm(cm)

  # Return BOTH names so your README can use confusion_matrix
  list(
    confusion = cm,
    confusion_matrix = cm,
    accuracy  = acc,
    precision = m$precision,
    recall    = m$recall,
    f1        = m$f1
  )
}

#' Evaluate two LAS files (truth vs prediction)
#'
#' Computes confusion matrix, accuracy, and per-class precision/recall/F1
#' using two separate LAS/LAZ files that are point-wise aligned.
#'
#' @param truth_las LAS containing ground-truth labels
#' @param pred_las  LAS containing predicted labels
#' @param truth_col Name of truth label field in truth_las (default = "label")
#' @param pred_col  Name of prediction field in pred_las (default = "Classification")
#'
#' @return list with confusion matrix, accuracy, precision, recall, F1
#' @export
evaluate_two_las <- function(truth_las,
                             pred_las,
                             truth_col = "label",
                             pred_col  = "Classification") {

  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').")
  }

  stopifnot(inherits(truth_las, "LAS"))
  stopifnot(inherits(pred_las, "LAS"))

  if (lidR::is.empty(truth_las) || lidR::is.empty(pred_las))
    stop("One of the LAS objects is empty.")

  if (!(truth_col %in% names(truth_las@data)))
    stop(paste("Truth column", shQuote(truth_col), "not found in truth_las."))

  if (!(pred_col %in% names(pred_las@data)))
    stop(paste("Prediction column", shQuote(pred_col), "not found in pred_las."))

  if (lidR::npoints(truth_las) != lidR::npoints(pred_las)) {
    stop("LAS files are not point-wise aligned: different number of points.")
  }

  true_labels <- as.integer(truth_las@data[[truth_col]])
  pred_labels <- as.integer(pred_las@data[[pred_col]])

  cm <- table(True = true_labels, Pred = pred_labels)
  acc <- sum(diag(cm)) / sum(cm)

  m <- metrics_from_cm(cm)

  list(
    confusion = cm,
    confusion_matrix = cm,
    accuracy  = acc,
    precision = m$precision,
    recall    = m$recall,
    f1        = m$f1
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
#' @param results List returned by evaluate_single_las() or evaluate_two_las()
#' @export
print_metrics_table <- function(results) {

  precision <- results$precision
  recall    <- results$recall
  f1        <- results$f1

  if (is.null(precision) || is.null(recall) || is.null(f1)) {
    stop("results must contain precision, recall, and f1 fields (output of evaluate_* functions).")
  }

  df <- data.frame(
    Class     = names(precision),
    Precision = round(as.numeric(precision), 4),
    Recall    = round(as.numeric(recall), 4),
    F1_Score  = round(as.numeric(f1), 4),
    stringsAsFactors = FALSE
  )

  df <- rbind(
    df,
    data.frame(
      Class     = "Overall (macro avg)",
      Precision = round(mean(precision, na.rm = TRUE), 4),
      Recall    = round(mean(recall, na.rm = TRUE), 4),
      F1_Score  = round(mean(f1, na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  )

  rownames(df) <- NULL
  print(df)
}


#' Plot confusion matrix as a heatmap (ggplot2)
#'
#' @param cm Confusion matrix (table or matrix)
#' @param title Plot title
#' @param row_normalize Logical. If TRUE, normalize each row to sum to 1 (proportions).
#' @param digits Integer. Rounding digits for displayed values when row_normalize=TRUE.
#' @param show_values Logical. If TRUE, print values inside each cell.
#'
#' @return A ggplot object
#' @export
plot_confusion_matrix <- function(cm,
                                  title = "Confusion Matrix",
                                  row_normalize = FALSE,
                                  digits = 3,
                                  show_values = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  }

  m <- as.matrix(cm)

  # Optional row-normalization (each true-label row sums to 1)
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }

  df <- as.data.frame(as.table(m))
  colnames(df) <- c("True", "Pred", "Value")

  # Text labels inside cells
  if (isTRUE(row_normalize)) {
    df$Label <- ifelse(is.na(df$Value), "", format(round(df$Value, digits = digits), nsmall = digits))
    fill_name <- "Proportion"
  } else {
    df$Label <- as.character(as.integer(round(df$Value)))
    fill_name <- "Count"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Pred, y = True, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = title,
      x = "Predicted Label",
      y = "True Label",
      fill = fill_name
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = Label), size = 4)
  }

  p
}
