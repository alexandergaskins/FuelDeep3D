# ============================================================
# Utility Functions for Visualization, Evaluation & Preprocessing
# FuelDeep3D — utils.R
# ============================================================

# ------------------------------------------------------------
# Default vegetation color palette
# ------------------------------------------------------------

#' Default vegetation color palette
#'
#' Returns a named character vector mapping class IDs to colors.
#' Useful for consistent coloring of point classes in plots.
#'
#' @return A named character vector of colors (names are class IDs as strings).
#'
#' @examples
#' pal <- palette_veg()
#' pal
#'
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

#' 3D visualization of a LAS object using rgl
#'
#' Opens an interactive 3D window and plots points colored by the LAS
#' \code{Classification} field.
#'
#' This function is intentionally skipped during \code{R CMD check} to
#' avoid interactive graphics in CRAN checks.
#'
#' @param las A \code{lidR::LAS} object.
#' @param palette Named color vector (names are class IDs as strings).
#' @param size Numeric. Point size passed to \code{rgl::points3d()}.
#' @param title Character. Title shown in the 3D window.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @examples
#' \donttest{
#' library(lidR)
#' las <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(las)) {
#'   plot_las_3d(las)
#' }
#' }
#'
#' @export
plot_las_3d <- function(las,
                        palette = palette_veg(),
                        size = 2,
                        title = "LAS 3D View") {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').",
         call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))

  # ---- HARD CRAN GUARD ----
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FuelDeep3D")) {
    warning("Skipping rgl visualization during R CMD check.")
    return(invisible(NULL))
  }

  if (!requireNamespace("rgl", quietly = TRUE)) {
    warning("Package 'rgl' is not installed. Install it to enable 3D plotting.")
    return(invisible(NULL))
  }

  rgl::open3d()
  rgl::title3d(title)

  xyz <- las@data[, c("X", "Y", "Z"), drop = FALSE]
  cls <- as.character(las@data$Classification)

  rgl::points3d(
    xyz,
    col  = palette[cls],
    size = size
  )

  invisible(NULL)
}

# ============================================================
# Noise Filtering Utilities
# ============================================================

#' Remove sparse outlier points using Statistical Outlier Removal (SOR)
#'
#' Applies a k-nearest-neighbor Statistical Outlier Removal (SOR) filter
#' to points above a user-defined height threshold. Points below the
#' threshold are preserved.
#'
#' @param las A \code{lidR::LAS} object.
#' @param height_thresh Numeric. Height (meters) above which filtering is applied.
#' @param k Integer. Number of nearest neighbors.
#' @param zscore Numeric. Standard deviation multiplier controlling outlier rejection.
#'
#' @return A filtered \code{lidR::LAS} object.
#'
#' @examples
#' \donttest{
#' library(lidR)
#' las <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(las)) {
#'   # keep the example fast by working on a small subset if the file is large
#'   n <- lidR::npoints(las)
#'   las_small <- las[seq_len(min(5000, n))]
#'   las_clean <- remove_noise_sor(las_small, height_thresh = 5, k = 10, zscore = 2.5)
#'   lidR::npoints(las_small)
#'   lidR::npoints(las_clean)
#' }
#' }
#'
#' @export
remove_noise_sor <- function(las,
                             height_thresh = 5,
                             k = 20,
                             zscore = 2.5) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').",
         call. = FALSE)
  }
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Install it with install.packages('dbscan').",
         call. = FALSE)
  }

  stopifnot(inherits(las, "LAS"))
  if (lidR::is.empty(las)) return(las)

  if (!is.numeric(height_thresh) || length(height_thresh) != 1 || height_thresh < 0) {
    stop("height_thresh must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1 || k < 1) {
    stop("k must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(zscore) || length(zscore) != 1 || zscore <= 0) {
    stop("zscore must be a single positive numeric value.", call. = FALSE)
  }

  # Split points
  high_pts <- las[las$Z > height_thresh]
  low_pts  <- las[las$Z <= height_thresh]

  if (lidR::npoints(high_pts) < k) return(las)

  # kNN distances in XYZ space
  xyz <- cbind(high_pts$X, high_pts$Y, high_pts$Z)
  knn_dist <- dbscan::kNNdist(xyz, k = k)
  d_mean <- rowMeans(knn_dist)

  mu <- mean(d_mean)
  sigma <- stats::sd(d_mean)

  if (!is.finite(sigma) || sigma == 0) return(las)

  keep <- d_mean < (mu + zscore * sigma)
  high_clean <- high_pts[keep]

  # Combine back
  out <- rbind(low_pts, high_clean)
  out
}

# ============================================================
# Confusion Matrix + Evaluation Metrics
# ============================================================

# ------------------------------------------------------------
# Internal helper: compute metrics from confusion matrix
# ------------------------------------------------------------

#' Compute precision/recall/F1 from a confusion matrix
#'
#' Internal helper used by evaluation functions.
#'
#' @param cm Confusion matrix as a table or matrix.
#'
#' @return A list with precision, recall, and f1 (each a numeric vector).
#'
#' @keywords internal
metrics_from_cm <- function(cm) {
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)

  precision[!is.finite(precision)] <- NA_real_
  recall[!is.finite(recall)]       <- NA_real_
  f1[!is.finite(f1)]               <- NA_real_

  list(precision = precision, recall = recall, f1 = f1)
}

#' Evaluate a single LAS using custom field names
#'
#' Computes confusion matrix, accuracy, and per-class precision/recall/F1
#' from one LAS object that contains both truth and prediction fields.
#'
#' @param las A \code{lidR::LAS} object containing both truth and prediction fields.
#' @param truth_col Character. Name of the ground-truth label field (default = "label").
#' @param pred_col Character. Name of the predicted label field (default = "Classification").
#'
#' @return A list with:
#' \itemize{
#'   \item \code{confusion} / \code{confusion_matrix}: confusion matrix
#'   \item \code{accuracy}: overall accuracy
#'   \item \code{precision}, \code{recall}, \code{f1}: per-class metrics
#' }
#'
#' @examples
#' \donttest{
#' library(lidR)
#' las <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(las)) {
#'   # Example-only: create a dummy truth column from Classification
#'   las@data$label <- as.integer(las@data$Classification)
#'   res <- evaluate_single_las(las, truth_col = "label", pred_col = "Classification")
#'   res$accuracy
#' }
#' }
#'
#' @export
evaluate_single_las <- function(las,
                                truth_col = "label",
                                pred_col  = "Classification") {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').",
         call. = FALSE)
  }

  stopifnot(inherits(las, "LAS"))

  if (!(truth_col %in% names(las@data))) {
    stop(paste("Truth column", shQuote(truth_col), "not found in LAS."), call. = FALSE)
  }
  if (!(pred_col %in% names(las@data))) {
    stop(paste("Prediction column", shQuote(pred_col), "not found in LAS."), call. = FALSE)
  }

  true_labels <- as.integer(las@data[[truth_col]])
  pred_labels <- as.integer(las@data[[pred_col]])

  if (length(true_labels) != length(pred_labels)) {
    stop("Truth and prediction vectors have different lengths.", call. = FALSE)
  }

  cm  <- table(True = true_labels, Pred = pred_labels)
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

#' Evaluate two LAS files (truth vs prediction)
#'
#' Computes confusion matrix, accuracy, and per-class precision/recall/F1
#' using two separate LAS objects that are point-wise aligned (same number of points).
#'
#' @param truth_las A \code{lidR::LAS} object containing ground-truth labels.
#' @param pred_las A \code{lidR::LAS} object containing predicted labels.
#' @param truth_col Character. Name of truth label field in \code{truth_las} (default = "label").
#' @param pred_col Character. Name of prediction field in \code{pred_las} (default = "Classification").
#'
#' @return A list with confusion matrix, accuracy, precision, recall, and F1.
#'
#' @examples
#' \donttest{
#' library(lidR)
#' truth <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' pred  <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(truth) && !lidR::is.empty(pred)) {
#'   truth@data$label <- as.integer(truth@data$Classification)
#'   res <- evaluate_two_las(truth, pred, truth_col = "label", pred_col = "Classification")
#'   res$accuracy
#' }
#' }
#'
#' @export
evaluate_two_las <- function(truth_las,
                             pred_las,
                             truth_col = "label",
                             pred_col  = "Classification") {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').",
         call. = FALSE)
  }

  stopifnot(inherits(truth_las, "LAS"))
  stopifnot(inherits(pred_las, "LAS"))

  if (lidR::is.empty(truth_las) || lidR::is.empty(pred_las)) {
    stop("One of the LAS objects is empty.", call. = FALSE)
  }

  if (!(truth_col %in% names(truth_las@data))) {
    stop(paste("Truth column", shQuote(truth_col), "not found in truth_las."), call. = FALSE)
  }
  if (!(pred_col %in% names(pred_las@data))) {
    stop(paste("Prediction column", shQuote(pred_col), "not found in pred_las."), call. = FALSE)
  }

  if (lidR::npoints(truth_las) != lidR::npoints(pred_las)) {
    stop("LAS files are not point-wise aligned: different number of points.", call. = FALSE)
  }

  true_labels <- as.integer(truth_las@data[[truth_col]])
  pred_labels <- as.integer(pred_las@data[[pred_col]])

  cm  <- table(True = true_labels, Pred = pred_labels)
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
#' Converts a confusion matrix to a data.frame and prints it.
#'
#' @param cm Confusion matrix (table or matrix).
#'
#' @return Invisibly returns the printed data.frame.
#'
#' @examples
#' cm <- table(True = c(0, 0, 1, 1), Pred = c(0, 1, 1, 1))
#' print_confusion_matrix(cm)
#'
#' @export
print_confusion_matrix <- function(cm) {
  df <- as.data.frame.matrix(cm)
  print(df)
  invisible(df)
}

# ------------------------------------------------------------
# Pretty-print Precision / Recall / F1 table
# ------------------------------------------------------------

#' Pretty-print evaluation metrics table with a macro-average summary row
#'
#' Prints a table with per-class precision/recall/F1 and an overall macro-average row.
#'
#' @param results List returned by \code{evaluate_single_las()} or \code{evaluate_two_las()}.
#'
#' @return Invisibly returns the metrics data.frame.
#'
#' @examples
#' cm <- table(True = c(0, 0, 1, 1), Pred = c(0, 1, 1, 1))
#' pr <- diag(cm) / colSums(cm)
#' rc <- diag(cm) / rowSums(cm)
#' f1 <- 2 * pr * rc / (pr + rc)
#' res <- list(precision = pr, recall = rc, f1 = f1)
#' print_metrics_table(res)
#'
#' @export
print_metrics_table <- function(results) {
  precision <- results$precision
  recall    <- results$recall
  f1        <- results$f1

  if (is.null(precision) || is.null(recall) || is.null(f1)) {
    stop("results must contain precision, recall, and f1 fields (output of evaluate_* functions).",
         call. = FALSE)
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
  invisible(df)
}

# ------------------------------------------------------------
# Confusion matrix heatmap
# ------------------------------------------------------------

#' Plot confusion matrix as a heatmap (ggplot2)
#'
#' Creates a heatmap visualization of a confusion matrix.
#' Optionally normalizes each row so that each true-label row sums to 1.
#'
#' @param cm Confusion matrix (table or matrix).
#' @param title Character. Plot title.
#' @param row_normalize Logical. If TRUE, normalize each row to sum to 1 (proportions).
#' @param digits Integer. Rounding digits for displayed values when row_normalize = TRUE.
#' @param show_values Logical. If TRUE, prints values inside each cell.
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' cm <- table(True = c(0, 0, 1, 1), Pred = c(0, 1, 1, 1))
#' p <- plot_confusion_matrix(cm, row_normalize = FALSE)
#' p
#' }
#'
#' @export
plot_confusion_matrix <- function(cm,
                                  title = "Confusion Matrix",
                                  row_normalize = FALSE,
                                  digits = 3,
                                  show_values = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').",
         call. = FALSE)
  }

  m <- as.matrix(cm)

  # Optional row-normalization
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }

  df <- as.data.frame(as.table(m))
  colnames(df) <- c("True", "Pred", "Value")

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

