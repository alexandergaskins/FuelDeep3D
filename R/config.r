#' Create a FuelDeep3D configuration
#'
#' Create a list of configuration parameters used by FuelDeep3D functions
#' such as [train()] and [predict()].
#'
#' @param las_path Path to an input LAS/LAZ file.
#' @param out_dir Directory where training tiles will be written.
#' @param out_pred_dir Directory where prediction outputs will be written.
#' @param model_path Path to a pre-trained `.pth` model checkpoint.
#' @param device Device to use (`"cpu"` or `"cuda"`). If `NULL`, the Python
#'   backend will choose automatically.
#' @param block_size Tile size in meters.
#' @param stride Overlap stride in meters.
#' @param sample_n Number of points sampled per tile.
#' @param repeat_per_tile Number of augmentations per tile.
#' @param min_pts_tile Minimum number of points required to keep a tile.
#' @param val_split Fraction of tiles used for validation.
#' @param test_split Fraction of tiles used for testing.
#' @param seed Random seed.
#' @param batch_size Batch size for training.
#' @param epochs Number of training epochs.
#' @param learning_rate Optimizer learning rate.
#' @param weight_decay L2 regularization strength.
#' @param cell_size Grid cell size (meters) used for height normalization.
#' @param quantile Quantile threshold used in metrics.
#' @param num_classes Number of segmentation classes. Supported values are 3
#'   or 4. If 4, FuelDeep3D can add a ground class using CSF post-processing.
#' @param csf_args A named list of arguments passed to `RCSF::csf()` when
#'   `num_classes = 4`.
#' @param delete_tiles_after_train Logical; if `TRUE`, delete generated tiles
#'   after training completes.
#'
#' @return A named list containing all configuration parameters.
#'
#' @examples
#' # Minimal config (does not require any external files)
#' cfg <- config(
#'   las_path = "dummy.las",
#'   out_dir = tempdir(),
#'   out_pred_dir = tempdir(),
#'   model_path = "dummy.pth",
#'   num_classes = 3
#' )
#' names(cfg)
#' cfg$num_classes
#'
#' # Validate allowed values
#' try(config(las_path="a.las", model_path="m.pth", num_classes = 5))
#'
#' \dontrun{
#' # Typical usage with real files (requires LAS/LAZ and a model checkpoint)
#' cfg <- config(
#'   las_path = "path/to/input.laz",
#'   out_dir = "tiles_out",
#'   out_pred_dir = "pred_out",
#'   model_path = "path/to/best_model.pth",
#'   num_classes = 3
#' )
#' }
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
)  {

  if (!num_classes %in% c(3, 4)) {
    stop(
      "num_classes must be 3 or 4.\n",
      "  3 = normal model prediction\n",
      "  4 = model prediction + CSF-based ground class"
    )
  }

  as.list(environment())
}
