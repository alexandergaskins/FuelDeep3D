# ============================================================
# FuelDeep3D - Evaluation utilities (Confusion matrix + metrics)
# ============================================================

# ------------------------------
# Internal helper: class labels
# ------------------------------
format_class_labels <- function(classes, class_names = NULL, show_codes = TRUE) {
  classes_chr <- as.character(classes)
  
  if (is.null(class_names)) {
    return(classes_chr)
  }
  
  # Allow either:
  # 1) named vector: c("0"="Ground","1"="Branch","2"="Leaves")
  # 2) unnamed vector matching length(classes): c("Ground","Branch","Leaves")
  if (!is.null(names(class_names)) && any(nzchar(names(class_names)))) {
    nm <- as.character(class_names)
    names(nm) <- as.character(names(class_names))
    
    mapped <- nm[classes_chr]
    mapped[is.na(mapped)] <- classes_chr[is.na(mapped)]
    
    if (isTRUE(show_codes)) {
      return(paste0(classes_chr, " (", mapped, ")"))
    }
    return(mapped)
  }
  
  # Unnamed vector
  if (length(class_names) != length(classes_chr)) {
    stop(
      "class_names must be either a named vector keyed by class code, ",
      "or an unnamed vector with the same length as 'classes'.",
      call. = FALSE
    )
  }
  
  if (isTRUE(show_codes)) {
    return(paste0(classes_chr, " (", as.character(class_names), ")"))
  }
  as.character(class_names)
}

# ------------------------------
# Internal helper: metrics from cm
# ------------------------------
metrics_from_cm <- function(cm) {
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)
  
  precision[!is.finite(precision)] <- NA_real_
  recall[!is.finite(recall)]       <- NA_real_
  f1[!is.finite(f1)]               <- NA_real_
  
  list(precision = precision, recall = recall, f1 = f1)
}

# ------------------------------
# Internal helper: build confusion matrix from LAS
# ------------------------------
.build_cm_from_las <- function(las,
                               truth_col = "label",
                               pred_col  = "Classification",
                               classes   = NULL,
                               drop_na   = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  df <- las@data
  if (!(truth_col %in% names(df))) stop("Truth column not found: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df))) stop("Prediction column not found: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  table(True = truth_f, Pred = pred_f)
}

# ------------------------------
# Internal helper: square/align cm
# ------------------------------
.as_square_cm <- function(cm, classes = NULL) {
  m <- as.matrix(cm)
  
  # Ensure row/col names exist (as codes)
  if (is.null(rownames(m))) rownames(m) <- as.character(seq_len(nrow(m)) - 1L)
  if (is.null(colnames(m))) colnames(m) <- as.character(seq_len(ncol(m)) - 1L)
  
  r <- rownames(m)
  c <- colnames(m)
  
  # Determine the class set to use (in a stable order)
  if (!is.null(classes)) {
    all_classes <- as.character(classes)
  } else {
    # Keep row order first, then add any new col classes not already present
    all_classes <- c(r, setdiff(c, r))
  }
  
  # Create square matrix filled with 0 and place existing cells
  out <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                dimnames = list(True = all_classes, Pred = all_classes))
  out[r, c] <- m
  
  out
}

# ============================================================
# Evaluate: single LAS
# ============================================================

#' Evaluate predictions stored in a single LAS/LAZ
#'
#' @description
#' Computes a confusion matrix and standard multi-class metrics when both the
#' ground-truth labels and predictions are stored as columns in the same
#' \code{lidR::LAS} object.
#'
#' @details
#' The function returns:
#' \itemize{
#'   \item \strong{overall_accuracy}: \eqn{\sum diag(cm) / \sum cm}
#'   \item \strong{class_accuracy}: per-class accuracy computed as \eqn{TP / (TP+FN)}
#'         (same as per-class recall / producer's accuracy).
#'   \item \strong{precision}, \strong{recall}, \strong{f1}: per-class metrics.
#'   \item \strong{balanced_accuracy}: mean of \code{class_accuracy} across classes (macro average).
#' }
#'
#' To keep the confusion matrix shape stable across files (even when some classes
#' are missing), pass \code{classes = 0:2} or \code{classes = 0:3}.
#'
#' @param las A \code{lidR::LAS} object containing truth and prediction fields.
#' @param truth_col Character. Name of the ground-truth label column (default \code{"label"}).
#' @param pred_col Character. Name of the prediction column (default \code{"Classification"}).
#' @param classes Optional integer vector of expected class IDs (e.g. \code{0:2}, \code{0:3}).
#'   If \code{NULL}, inferred from observed truth and pred values.
#' @param drop_na Logical. If \code{TRUE} (default), rows with NA/Inf in truth or pred are dropped.
#' @param class_names Optional class name mapping. Either:
#'   \itemize{
#'     \item Named: \code{c("0"="Ground","1"="Branch","2"="Leaves")}
#'     \item Unnamed length-K: \code{c("Ground","Branch","Leaves")} (must match \code{classes})
#'   }
#' @param show_codes Logical. If \code{TRUE} (default), class labels display like \code{"0 (Ground)"}.
#'
#' @return A list containing confusion matrix and metrics.
#'
#' @examples
#' \dontrun{
#' library(lidR)
#' las <- readLAS("your_file.laz")
#'
#' res <- evaluate_single_las(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' res$class_accuracy
#' cat(sprintf("accuracy = %.2f%%\n", 100 * res$accuracy))
#' }
#' @export
evaluate_single_las <- function(las,
                                truth_col = "label",
                                pred_col  = "Classification",
                                classes   = NULL,
                                drop_na   = TRUE,
                                class_names = NULL,
                                show_codes = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  df <- las@data
  if (!(truth_col %in% names(df))) stop("Truth column not found: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df))) stop("Prediction column not found: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  cm <- table(True = truth_f, Pred = pred_f)
  
  overall_acc <- sum(diag(cm)) / sum(cm)
  class_acc   <- diag(cm) / rowSums(cm)
  class_acc[!is.finite(class_acc)] <- NA_real_
  
  m <- metrics_from_cm(cm)
  support <- rowSums(cm)
  balanced_acc <- mean(class_acc, na.rm = TRUE)
  
  class_labels <- format_class_labels(classes, class_names = class_names, show_codes = show_codes)
  dimnames(cm) <- list(True = class_labels, Pred = class_labels)
  
  names(class_acc) <- class_labels
  names(m$precision) <- class_labels
  names(m$recall) <- class_labels
  names(m$f1) <- class_labels
  
  list(
    confusion_matrix = cm,
    confusion = cm,
    accuracy = overall_acc,          
    overall_accuracy = overall_acc,
    class_accuracy = class_acc,
    balanced_accuracy = balanced_acc,
    precision = m$precision,
    recall = m$recall,
    f1 = m$f1,
    support = support,
    classes = classes,
    class_labels = class_labels,
    n = sum(cm)
  )
}

# ============================================================
# Evaluate: two LAS
# ============================================================

#' Evaluate predictions stored in two LAS/LAZ objects
#'
#' @description
#' Computes confusion matrix and metrics when ground truth and predictions come from
#' two separate \code{lidR::LAS} objects.
#'
#' @details
#' \strong{Important:} This assumes the LAS objects are point-wise aligned (same points,
#' same order). If they are not aligned, metrics will be meaningless.
#'
#' @inheritParams evaluate_single_las
#' @param truth_las A \code{lidR::LAS} containing ground-truth labels.
#' @param pred_las A \code{lidR::LAS} containing predicted labels.
#'
#' @return A list containing confusion matrix and metrics.
#'
#' @examples
#' \dontrun{
#' library(lidR)
#' truth <- readLAS("truth.laz")
#' pred  <- readLAS("predicted.laz")
#'
#' res <- evaluate_two_las(
#'   truth, pred,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' res$class_accuracy
#' cat(sprintf("accuracy = %.2f%%\n", 100 * res$accuracy))
#' }
#' @export
evaluate_two_las <- function(truth_las,
                             pred_las,
                             truth_col = "label",
                             pred_col  = "Classification",
                             classes   = NULL,
                             drop_na   = TRUE,
                             class_names = NULL,
                             show_codes = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(truth_las, "LAS"))
  stopifnot(inherits(pred_las, "LAS"))
  
  if (lidR::is.empty(truth_las) || lidR::is.empty(pred_las)) {
    stop("One of the LAS objects is empty.", call. = FALSE)
  }
  if (lidR::npoints(truth_las) != lidR::npoints(pred_las)) {
    stop("LAS objects are not aligned: different number of points.", call. = FALSE)
  }
  
  df_t <- truth_las@data
  df_p <- pred_las@data
  
  if (!(truth_col %in% names(df_t))) stop("Truth column not found in truth_las: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df_p))) stop("Prediction column not found in pred_las: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df_t[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df_p[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  cm <- table(True = truth_f, Pred = pred_f)
  
  overall_acc <- sum(diag(cm)) / sum(cm)
  class_acc   <- diag(cm) / rowSums(cm)
  class_acc[!is.finite(class_acc)] <- NA_real_
  
  m <- metrics_from_cm(cm)
  support <- rowSums(cm)
  balanced_acc <- mean(class_acc, na.rm = TRUE)
  
  class_labels <- format_class_labels(classes, class_names = class_names, show_codes = show_codes)
  dimnames(cm) <- list(True = class_labels, Pred = class_labels)
  
  names(class_acc) <- class_labels
  names(m$precision) <- class_labels
  names(m$recall) <- class_labels
  names(m$f1) <- class_labels
  
  list(
    confusion_matrix = cm,
    confusion = cm,
    accuracy = overall_acc,          
    overall_accuracy = overall_acc,
    class_accuracy = class_acc,
    balanced_accuracy = balanced_acc,
    precision = m$precision,
    recall = m$recall,
    f1 = m$f1,
    support = support,
    classes = classes,
    class_labels = class_labels,
    n = sum(cm)
  )
}

# ------------------------------------------------------------
# 3D LAS Visualization (height-colored, configurable background)
# ------------------------------------------------------------

#' 3D visualization of a LAS object colored by height (Z) using rgl
#'
#' @description
#' Opens an interactive \pkg{rgl} 3D window and plots a \code{\link[lidR]{LAS}} point cloud,
#' coloring points by their height (\code{Z}). The background color and the height range used
#' for coloring can be customized. For large point clouds, \code{max_points} can be used to
#' subsample points for faster rendering.
#'
#' @details
#' Points are colored using their \code{Z} (height) values mapped to a continuous color gradient.
#'
#' \itemize{
#'   \item \strong{Height scaling (\code{zlim})}: If \code{zlim = c(zmin, zmax)} is provided,
#'   \code{Z} values are clamped to \code{[zmin, zmax]} before mapping to colors. This prevents
#'   extreme outliers from dominating the color scale and helps preserve contrast in the
#'   understory/canopy.
#'
#'   \item \strong{Automatic range}: If \code{zlim = NULL}, the observed \code{Z} range
#'   (after any optional subsampling) is used automatically.
#'
#'   \item \strong{Custom palettes (\code{height_palette})}: You can provide a vector of colors
#'   (e.g., \code{c("purple","blue","cyan","yellow","red")}) to define the height color ramp.
#'
#'   \item \strong{Performance (\code{max_points})}: Rendering millions of points can be slow.
#'   If \code{max_points} is set and the LAS has more points than this value, points are randomly
#'   subsampled (without modifying the original LAS object) to improve interactivity.
#' }
#'
#' This function is intentionally skipped during \code{R CMD check} to avoid interactive graphics
#' in CRAN checks.
#'
#' @param las A \code{\link[lidR]{LAS}} object.
#' @param bg Character. Background color passed to \code{rgl::bg3d()}. Common values include
#'   \code{"black"} and \code{"white"}.
#' @param zlim Numeric vector of length 2 (e.g., \code{c(0, 35)}) giving the height range used for
#'   color mapping. Values outside this range are clamped. If \code{NULL}, the observed range is used.
#' @param height_palette Character vector of colors used to build the height color ramp.
#'   If \code{NULL}, a default sequential palette is used.
#' @param size Numeric. Point size passed to \code{rgl::points3d()}.
#' @param max_points Integer. Maximum number of points to plot. If the LAS contains more than
#'   \code{max_points}, a random subset of size \code{max_points} is drawn for faster rendering.
#'   Use \code{NULL} to plot all points (may be slow for large clouds).
#' @param title Character. Title shown in the 3D window (via \code{rgl::title3d()}).
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @examples
#' \donttest{
#' library(lidR)
#' las <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(las)) {
#'   # Black background, color from 0 to 35 height units
#'   plot_las_3d(las, bg = "black", zlim = c(0, 35), size = 2, max_points = 200000)
#'
#'   # White background, automatic height range
#'   plot_las_3d(las, bg = "white", zlim = NULL, size = 2, max_points = 100000)
#'
#'   # Custom height palette
#'   plot_las_3d(
#'     las,
#'     bg = "black",
#'     zlim = c(0, 35),
#'     height_palette = c("purple", "blue", "cyan", "yellow", "red"),
#'     max_points = 150000
#'   )
#' }
#' }
#'
#' @export
plot_las_3d <- function(las,
                        bg = "black",
                        zlim = NULL,
                        height_palette = NULL,
                        size = 2,
                        max_points = 200000L,
                        title = "LAS 3D View") {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  # ---- HARD CRAN GUARD ----
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FuelDeep3D")) {
    warning("Skipping rgl visualization during R CMD check.", call. = FALSE)
    return(invisible(NULL))
  }
  
  if (!requireNamespace("rgl", quietly = TRUE)) {
    warning("Package 'rgl' is not installed. Install it to enable 3D plotting.", call. = FALSE)
    return(invisible(NULL))
  }
  
  if (lidR::is.empty(las)) return(invisible(NULL))
  
  # optional subsample for performance
  n <- lidR::npoints(las)
  if (!is.null(max_points) && is.finite(max_points) && max_points > 0 && n > max_points) {
    idx <- sample.int(n, size = as.integer(max_points))
    las_plot <- las[idx]
  } else {
    las_plot <- las
  }
  
  xyz <- las_plot@data[, c("X", "Y", "Z"), drop = FALSE]
  z <- xyz[["Z"]]
  
  # choose z-limits
  if (is.null(zlim)) {
    zmin <- min(z, na.rm = TRUE)
    zmax <- max(z, na.rm = TRUE)
  } else {
    if (!is.numeric(zlim) || length(zlim) != 2L || any(!is.finite(zlim))) {
      stop("zlim must be NULL or a numeric vector of length 2 with finite values.", call. = FALSE)
    }
    zmin <- min(zlim)
    zmax <- max(zlim)
  }
  if (!is.finite(zmin) || !is.finite(zmax) || zmin == zmax) {
    stop("Invalid z-range for coloring (zlim or data range).", call. = FALSE)
  }
  
  # clamp and normalize Z to [0,1]
  zc <- pmin(pmax(z, zmin), zmax)
  t <- (zc - zmin) / (zmax - zmin)
  
  # palette
  if (is.null(height_palette)) {
    height_palette <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF") # safe default (ASCII)
  }
  ramp <- grDevices::colorRampPalette(height_palette)
  cols <- ramp(256L)[pmax(1L, pmin(256L, as.integer(round(t * 255L)) + 1L))]
  
  rgl::open3d()
  rgl::bg3d(color = bg)
  rgl::title3d(title)
  
  rgl::points3d(
    x = xyz[["X"]],
    y = xyz[["Y"]],
    z = xyz[["Z"]],
    col = cols,
    size = size
  )
  
  invisible(NULL)
}

# ============================================================
# Print metrics table (includes macro/weighted; overall optional)
# ============================================================

#' Print per-class metrics and summary averages
#'
#' @description
#' Formats and prints a per-class metrics table from the output of
#' \code{\link{evaluate_single_las}} or \code{\link{evaluate_two_las}}.
#' The table includes per-class Support, Accuracy (Recall), Precision, Recall, and F1.
#' Optionally includes Macro and Weighted averages, and an Overall accuracy row.
#'
#' @details
#' \itemize{
#'   \item \strong{Support}: number of true points in each class (row sum of confusion matrix).
#'   \item \strong{Accuracy}: per-class accuracy here means \strong{Recall} (TP / (TP + FN)).
#'     This is sometimes called \emph{producer's accuracy}.
#'   \item \strong{Macro avg}: unweighted mean across classes (ignores class imbalance).
#'   \item \strong{Weighted avg}: mean across classes weighted by Support (reflects imbalance).
#'   \item \strong{Overall accuracy}: sum(diag(cm)) / sum(cm). This is optional because
#'     it often duplicates the weighted recall/accuracy in multi-class settings.
#' }
#'
#' @param results List returned by \code{evaluate_single_las()} or \code{evaluate_two_las()}.
#' @param digits Integer. Number of decimal places to round numeric metrics.
#' @param include_macro Logical. If TRUE, include a "Macro avg" row.
#' @param include_weighted Logical. If TRUE, include a "Weighted avg" row.
#' @param include_overall Logical. If TRUE, include an "Overall accuracy" row.
#'
#' @return
#' Invisibly returns a \code{data.frame} with per-class metrics and optional summary rows.
#'
#' @examples
#' \dontrun{
#' library(lidR)
#' las <- readLAS("C:/Users/vs.naga/Documents/Github/FuelDeep3D/inst/extdata/las/trees.laz")
#'
#' res <- evaluate_single_las(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' # print without overall accuracy row (recommended)
#' # print_metrics_table(res, include_overall = FALSE)
#'
#' # If you want overall accuracy too:
#' print_metrics_table(res, include_overall = TRUE)
#' }
#'
#' @export
print_metrics_table <- function(results,
                                digits = 4,
                                include_macro = TRUE,
                                include_weighted = TRUE,
                                include_overall = FALSE) {
  if (is.null(results$class_labels) || is.null(results$support)) {
    stop("results must come from evaluate_single_las() or evaluate_two_las().", call. = FALSE)
  }
  
  labs <- results$class_labels
  support <- as.numeric(results$support)
  names(support) <- labs
  
  acc <- as.numeric(results$class_accuracy); names(acc) <- labs
  pr  <- as.numeric(results$precision);      names(pr)  <- labs
  rc  <- as.numeric(results$recall);         names(rc)  <- labs
  f1  <- as.numeric(results$f1);             names(f1)  <- labs
  
  # per-class table
  df <- data.frame(
    Class     = labs,
    Support   = support,
    Accuracy  = round(acc, digits),
    Precision = round(pr,  digits),
    Recall    = round(rc,  digits),
    F1_Score  = round(f1,  digits),
    stringsAsFactors = FALSE
  )
  
  out_rows <- list(df)
  
  # weights for weighted averages
  w <- support / sum(support)
  
  if (isTRUE(include_macro)) {
    macro_row <- data.frame(
      Class     = "Macro avg",
      Support   = sum(support),
      Accuracy  = round(mean(acc, na.rm = TRUE), digits),
      Precision = round(mean(pr,  na.rm = TRUE), digits),
      Recall    = round(mean(rc,  na.rm = TRUE), digits),
      F1_Score  = round(mean(f1,  na.rm = TRUE), digits),
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(macro_row))
  }
  
  if (isTRUE(include_weighted)) {
    weighted_row <- data.frame(
      Class     = "Weighted avg",
      Support   = sum(support),
      Accuracy  = round(sum(w * acc, na.rm = TRUE), digits),
      Precision = round(sum(w * pr,  na.rm = TRUE), digits),
      Recall    = round(sum(w * rc,  na.rm = TRUE), digits),
      F1_Score  = round(sum(w * f1,  na.rm = TRUE), digits),
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(weighted_row))
  }
  
  if (isTRUE(include_overall)) {
    overall_row <- data.frame(
      Class     = "Overall accuracy",
      Support   = sum(support),
      Accuracy  = round(as.numeric(results$overall_accuracy), digits),
      Precision = NA_real_,
      Recall    = NA_real_,
      F1_Score  = NA_real_,
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(overall_row))
  }
  
  df_out <- do.call(rbind, out_rows)
  rownames(df_out) <- NULL
  print(df_out)
  invisible(df_out)
}


# ============================================================
# Noise Filtering Utilities
# ============================================================

#' Remove sparse outlier points using Statistical Outlier Removal (SOR)
#'
#' @description
#' Applies a k-nearest-neighbor Statistical Outlier Removal (SOR) filter to points
#' above a user-defined height threshold. Points at or below the threshold are
#' preserved unchanged.
#'
#' @details
#' The filter is applied only to points with \code{Z > height_thresh}.
#' For each of these points, the mean distance to its \code{k} nearest neighbors
#' is computed in 3D XYZ space. A point is kept if:
#' \deqn{d_i < mean(d) + zscore * sd(d)}
#' where \code{d_i} is the mean kNN distance for point i.
#'
#' To keep behavior stable and safe, \code{k} is automatically capped so that
#' \code{k < n}, where \code{n} is the number of points being filtered.
#'
#' This function preserves the original point order by computing a global keep
#' mask and subsetting once.
#'
#' @param las A \code{lidR::LAS} object.
#' @param height_thresh Numeric. Height (meters) above which filtering is applied.
#' @param k Integer. Number of nearest neighbors used by the SOR filter.
#' @param zscore Numeric. Standard deviation multiplier controlling outlier rejection.
#'
#' @return A filtered \code{lidR::LAS} object.
#'
#' @examples
#' \donttest{
#' library(lidR)
#' las <- readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' if (!lidR::is.empty(las)) {
#'   las_small <- las[seq_len(min(20000, lidR::npoints(las)))]
#'   las_clean <- remove_noise_sor(las_small, height_thresh = 1, k = 10, zscore = 2.5)
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
  
  k <- as.integer(k)
  
  # Global masks (preserve original order)
  is_high <- las$Z > height_thresh
  if (!any(is_high, na.rm = TRUE)) return(las)
  
  # Work only on high points
  high_idx <- which(is_high)
  high_pts <- las[high_idx]
  
  if (lidR::is.empty(high_pts)) return(las)
  
  # Build XYZ and drop non-finite rows (defensive, CRAN-safe)
  xyz <- cbind(high_pts$X, high_pts$Y, high_pts$Z)
  ok <- is.finite(xyz[, 1]) & is.finite(xyz[, 2]) & is.finite(xyz[, 3])
  
  if (!all(ok)) {
    xyz <- xyz[ok, , drop = FALSE]
    high_idx <- high_idx[ok]
    high_pts <- high_pts[ok]
  }
  
  n <- nrow(xyz)
  if (is.null(n) || n < 2L) return(las)
  
  # kNNdist requires k < n; cap safely
  k_use <- min(k, n - 1L)
  if (k_use < 1L) return(las)
  
  knn_dist <- dbscan::kNNdist(xyz, k = k_use)
  
  # kNNdist can return vector in edge cases -> handle both
  if (is.null(dim(knn_dist))) {
    d_mean <- as.numeric(knn_dist)
  } else {
    d_mean <- rowMeans(knn_dist)
  }
  
  if (length(d_mean) == 0L || any(!is.finite(d_mean))) return(las)
  
  mu <- mean(d_mean)
  sigma <- stats::sd(d_mean)
  
  if (!is.finite(sigma) || sigma <= 0) return(las)
  
  keep_high <- d_mean < (mu + zscore * sigma)
  if (length(keep_high) != length(high_idx)) return(las)
  
  # Build final keep mask for all points (preserve order)
  keep_all <- rep(TRUE, lidR::npoints(las))
  keep_all[high_idx] <- keep_high
  
  las[keep_all]
}


# ============================================================
# Print confusion matrix (LAS or cm)
# ============================================================

#' Print a confusion matrix (LAS/LAZ or precomputed cm)
#'
#' @description
#' Prints a confusion matrix as a readable table. You can pass either:
#' \itemize{
#'   \item a precomputed confusion matrix \code{table/matrix}, OR
#'   \item a \code{lidR::LAS} object (then \code{truth_col} and \code{pred_col} are used to build the matrix).
#' }
#'
#' @param x A confusion matrix (table/matrix) OR a \code{lidR::LAS} object.
#' @param truth_col Character. Truth label column name (used when \code{x} is LAS).
#' @param pred_col Character. Prediction column name (used when \code{x} is LAS).
#' @param classes Optional integer vector of expected class IDs (e.g. \code{0:2}).
#'   Keeps matrix shape stable even if some classes are missing.
#' @param class_names Optional mapping for nicer labels (named or unnamed).
#' @param show_codes Logical. If \code{TRUE} (default), labels display like \code{"0 (Ground)"}.
#' @param drop_na Logical. Drop rows where truth/pred is NA/Inf (used when \code{x} is LAS).
#' @param row_normalize Logical. If TRUE, each row is converted to proportions (rows sum to 1).
#' @param digits Integer. Number of decimal places to show when \code{row_normalize = TRUE}.
#'
#' @return Invisibly returns the printed data.frame.
#'
#' @examples
#' \dontrun{
#' library(lidR)
#' las <- readLAS("C:/Users/vs.naga/Documents/Github/FuelDeep3D/inst/extdata/las/trees.laz")
#'
#' print_confusion_matrix(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves"),
#'     row_normalize = TRUE,
#'     digits = 3
#' )
#' }
#' @export
print_confusion_matrix <- function(x,
                                   truth_col = "label",
                                   pred_col  = "Classification",
                                   row_normalize = FALSE,
                                   digits = 3,
                                   classes   = NULL,
                                   class_names = NULL,
                                   show_codes = TRUE,
                                   drop_na = TRUE) {
  
  if (inherits(x, "LAS")) {
    cm <- .build_cm_from_las(
      x,
      truth_col = truth_col,
      pred_col  = pred_col,
      classes   = classes,
      drop_na   = drop_na
    )
    if (is.null(classes)) {
      classes <- rownames(as.matrix(cm))
    }
  } else {
    cm <- x
    if (is.null(classes)) {
      m0 <- as.matrix(cm)
      if (!is.null(rownames(m0))) {
        classes <- rownames(m0)
      } else {
        classes <- as.character(seq_len(nrow(m0)) - 1L)
      }
    }
  }
  
  m <- .as_square_cm(cm, classes = classes)
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }
  if (isTRUE(row_normalize)) {
    m <- round(m, digits)
  }
  
  
  labs <- format_class_labels(rownames(m), class_names = class_names, show_codes = show_codes)
  dimnames(m) <- list(True = labs, Pred = labs)
  
  df <- as.data.frame.matrix(m)
  print(df)
  invisible(df)
}

# ============================================================
# Confusion matrix heatmap (cm input)
# ============================================================

#' Plot a confusion matrix heatmap (ggplot2)
#'
#' @description
#' Creates a heatmap visualization of a confusion matrix. If \code{row_normalize = TRUE},
#' each row is converted to proportions so rows sum to 1.
#'
#' @param cm Confusion matrix (table or matrix).
#' @param las_name Optional character. If provided, appended to the title.
#' @param title Character. Plot title.
#' @param row_normalize Logical. If \code{TRUE}, normalize each row to proportions.
#' @param digits Integer. Rounding digits for displayed values when \code{row_normalize = TRUE}.
#' @param show_values Logical. If \code{TRUE}, draw numbers in each cell.
#' @param class_names Optional class name mapping (named or unnamed).
#' @param show_codes Logical. If \code{TRUE}, axis labels include codes like \code{"0 (Ground)"}.
#' @param flip_y Logical. If \code{TRUE}, show first class at the top (common confusion-matrix style).
#'
#' @return A ggplot object (invisibly).
#'
#' @examples
#' \dontrun{
#' library(lidR)
#' las <- readLAS("C:/Users/vs.naga/Documents/Github/FuelDeep3D/inst/extdata/las/trees.laz")
#'
#' cm <- table(True = las@data$label, Pred = las@data$Classification)
#'
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = TRUE,
#'   las_name = "trees.laz",
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#' }
#' @export
plot_confusion_matrix <- function(cm,
                                  las_name = NULL,
                                  title = "Confusion Matrix",
                                  row_normalize = FALSE,
                                  digits = 3,
                                  show_values = TRUE,
                                  class_names = NULL,
                                  show_codes = FALSE,
                                  flip_y = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').", call. = FALSE)
  }
  
  # Make sure we plot a square matrix with aligned classes
  m <- .as_square_cm(cm, classes = NULL)
  true_classes <- rownames(m)
  pred_classes <- colnames(m)
  
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }
  
  df <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
  colnames(df) <- c("True", "Pred", "Value")
  
  if (isTRUE(row_normalize)) {
    df$Label <- ifelse(is.na(df$Value), "", formatC(df$Value, format = "f", digits = digits))
    fill_name <- "Proportion"
  } else {
    df$Label <- as.character(as.integer(round(df$Value)))
    fill_name <- "Count"
  }
  
  # Axis labels (class codes -> names)
  true_labels <- format_class_labels(true_classes, class_names = class_names, show_codes = show_codes)
  pred_labels <- format_class_labels(pred_classes, class_names = class_names, show_codes = show_codes)
  
  df$True <- factor(df$True, levels = true_classes, labels = true_labels)
  df$Pred <- factor(df$Pred, levels = pred_classes, labels = pred_labels)
  
  # lock y-axis levels for flipping
  df$True <- factor(df$True, levels = true_labels)
  
  if (!is.null(las_name) && nzchar(las_name)) {
    title <- paste0(title, " - ", las_name)
  }
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Pred, y = True, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = "Predicted", y = "True", fill = fill_name) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = Label), size = 4)
  }
  
  if (isTRUE(flip_y)) {
    p <- p + ggplot2::scale_y_discrete(limits = rev(levels(df$True)))
  }
  
  print(p)
  invisible(p)
}
