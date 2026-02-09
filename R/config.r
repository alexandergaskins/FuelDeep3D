#' Create a FuelDeep3D configuration
#'
#' @description
#' Constructs a named list of parameters used throughout FuelDeep3D for dataset
#' tiling, model training, and inference. The returned configuration is consumed
#' by \code{\link{train}} (to build NPZ tiles and train the Python model) and
#' \code{\link{predict}} (to run inference and write a predicted LAS/LAZ).
#'
#' @details
#' The configuration groups parameters into a few logical sections:
#'
#' \strong{I/O paths}
#' \itemize{
#'   \item \code{las_path}: input LAS/LAZ file used for preprocessing, training, or inference.
#'   \item \code{out_dir}: directory where NPZ tiles will be written (typically contains
#'   \code{train/}, \code{val/}, and \code{test/} subfolders).
#'   \item \code{out_pred_dir}: directory where predicted LAS/LAZ outputs are written.
#'   \item \code{model_path}: path to a \code{.pth} model checkpoint used by \code{\link{predict}}.
#' }
#'
#' \strong{Tiling / sampling (Python dataset builder)}
#' \itemize{
#'   \item \code{block_size} and \code{stride} control the spatial tiling grid (meters).
#'   \item \code{sample_n} sets the number of points sampled per tile.
#'   \item \code{repeat_per_tile} controls how many repeated samples/augmentations are generated per tile.
#'   \item \code{min_pts_tile} drops tiles with too few points.
#'   \item \code{val_split}, \code{test_split}, and \code{seed} control dataset splitting.
#'   \item \code{cell_size} and \code{quantile} are forwarded to the Python pipeline for
#'   height normalization / grid-based statistics and related thresholds.
#' }
#'
#' \strong{Training hyperparameters (Python trainer)}
#' \itemize{
#'   \item \code{batch_size}, \code{epochs}, \code{learning_rate}, and \code{weight_decay}
#'   configure the optimizer and training loop in the Python trainer.
#' }
#'
#' \strong{Device selection}
#' \itemize{
#'   \item \code{device} can be \code{"cpu"} or \code{"cuda"}. If \code{NULL}, the Python
#'   backend selects CUDA when available and otherwise falls back to CPU.
#' }
#'
#' \strong{Class handling: 3 vs 4 classes}
#' \itemize{
#'   \item \code{num_classes = 3}: produces the model's 3-class predictions.
#'   \item \code{num_classes = 4}: after 3-class prediction, FuelDeep3D can add a
#'   ground class via CSF post-processing (see \code{\link{add_ground_csf}}).
#' }
#'
#' \strong{Cleanup}
#' \itemize{
#'   \item If \code{delete_tiles_after_train = TRUE}, generated NPZ tiles under
#'   \code{out_dir/train}, \code{out_dir/val}, and \code{out_dir/test} may be removed
#'   after training (see \code{\link{train}}).
#' }
#'
#' @param las_path Path to an input LAS/LAZ file.
#' @param out_dir Directory where training tiles (NPZ) will be written.
#' @param out_pred_dir Directory where prediction outputs will be written.
#' @param model_path Path to a \code{.pth} model checkpoint.
#' @param device Device to use (\code{"cpu"} or \code{"cuda"}). If \code{NULL}, the Python
#'   backend will choose automatically.
#' @param block_size Tile size (meters).
#' @param stride Overlap stride (meters).
#' @param sample_n Number of points sampled per tile.
#' @param repeat_per_tile Number of repeated samples/augmentations per tile.
#' @param min_pts_tile Minimum number of points required to keep a tile.
#' @param val_split Fraction of tiles used for validation.
#' @param test_split Fraction of tiles used for testing.
#' @param seed Random seed used for splitting/sampling.
#' @param batch_size Batch size for training.
#' @param epochs Number of training epochs.
#' @param learning_rate Optimizer learning rate.
#' @param weight_decay L2 regularization strength (weight decay).
#' @param cell_size Grid cell size (meters) used for height normalization/statistics.
#' @param quantile Quantile of the threshold used in metrics/filters in the Python pipeline.
#' @param num_classes Number of output classes. Supported values are \code{3} or \code{4}.
#'   If \code{4}, ground can be added using CSF post-processing.
#' @param csf_args Named list of arguments forwarded to \code{RCSF::csf()} inside
#'   \code{\link{add_ground_csf}} (used when \code{num_classes = 4}).
#' @param delete_tiles_after_train Logical; if \code{TRUE}, delete generated NPZ tiles
#'   after training completes (see \code{\link{train}}).
#'
#' @return A named list containing all configuration parameters.
#'
#' @export
config <- function(
    las_path      = system.file("extdata", "trees.las", package = "FuelDeep3D"),
    out_dir       = getwd(),
    out_pred_dir  = getwd(),
    model_path    = system.file("extdata", "best_model.pth", package = "FuelDeep3D"),
    device        = NULL,
    block_size    = 6.0,
    stride        = 1.0,
    sample_n      = 4096,
    repeat_per_tile = 4,
    min_pts_tile  = 512,
    val_split     = 0.15,
    test_split    = 0.10,
    seed          = 42,
    batch_size    = 16,
    epochs        = 2,
    learning_rate = 1e-5,
    weight_decay  = 1e-4,
    cell_size     = 0.25,
    quantile      = 0.05,
    num_classes   = 3,
    csf_args = list(
      rigidness = 4,
      cloth_resolution = 0.25,
      time_step = 0.65,
      class_threshold = 0.05
    ),
    delete_tiles_after_train = TRUE
) {
  if (!num_classes %in% c(3, 4)) {
    stop(
      "num_classes must be 3 or 4.\n",
      "  3 = normal model prediction\n",
      "  4 = model prediction + CSF-based ground class",
      call. = FALSE
    )
  }
  as.list(environment())
}
