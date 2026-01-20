#' Train the FuelDeep3D model (auto-preprocess if NPZ tiles are missing)
#'
#' This function calls the Python training pipeline shipped in
#' `inst/extdata/python`. Training requires a working Python environment and
#' the required Python dependencies (e.g., torch).
#'
#' @param cfg A list created by [config()].
#' @param setup_env Logical; if `TRUE`, calls [py_setup()] to create/use a venv.
#'
#' @return A list with training outputs (e.g., best metrics and checkpoint path),
#'   returned from the Python trainer.
#'
#' @examples
#' \dontrun{
#' library(FuelDeep3D)
#'
#' cfg <- config(
#'   las_path  = system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"),
#'   out_dir   = file.path(tempdir(), "FuelDeep3D_tiles"),
#'   epochs    = 2,
#'   batch_size = 8,
#'   learning_rate = 1e-3
#' )
#'
#' # Train (requires Python + torch + extdata/python scripts)
#' res <- train(cfg, setup_env = FALSE)
#' res
#' }
#' @export
train <- function(cfg, setup_env = FALSE) {
  stopifnot(is.list(cfg))

  # Safety: never run training during R CMD check
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    stop("train() is disabled during R CMD check. See ?train for \\\\dontrun{} examples.",
         call. = FALSE)
  }

  if (isTRUE(setup_env)) {
    py_setup()
  }

  # locate inst/extdata/python
  py_dir <- system.file("extdata", "python", package = "FuelDeep3D")
  if (!nzchar(py_dir) || !dir.exists(py_dir)) {
    stop("Could not find 'inst/extdata/python' in the installed FuelDeep3D package.",
         call. = FALSE)
  }

  # ---- 1) Ensure NPZ tiles exist ----
  train_dir <- file.path(cfg$out_dir, "train")
  has_npz <- dir.exists(train_dir) &&
    length(list.files(train_dir, pattern = "\\.npz$", full.names = TRUE)) > 0

  if (!has_npz) {
    message(">> No NPZ tiles found in ", train_dir,
            " -> running build_dataset_from_las() ...")

    py_dataset <- reticulate::import_from_path("dataset", path = py_dir, delay_load = FALSE)

    py_dataset$build_dataset_from_las(
      LAS_PATH        = cfg$las_path,
      OUT_DIR         = cfg$out_dir,
      SAMPLE_N        = cfg$sample_n,
      BLOCK_SIZE      = cfg$block_size,
      STRIDE          = cfg$stride,
      VAL_SPLIT       = cfg$val_split,
      TEST_SPLIT      = cfg$test_split,
      SEED            = cfg$seed,
      REPEAT_PER_TILE = cfg$repeat_per_tile,
      MIN_PTS_TILE    = cfg$min_pts_tile,
      CELL_SIZE       = cfg$cell_size,
      QUANTILE        = cfg$quantile
    )
  } else {
    message(">> Found existing NPZ tiles in ", train_dir, " -> skipping preprocessing.")
  }

  # ---- 2) Import Python trainer ----
  py_train <- reticulate::import_from_path("train", path = py_dir, delay_load = FALSE)

  # ---- 3) Make sure numeric fields are not strings ----
  cfg$batch_size       <- as.integer(cfg$batch_size)
  cfg$epochs           <- as.integer(cfg$epochs)
  cfg$learning_rate    <- as.numeric(cfg$learning_rate)
  cfg$weight_decay     <- as.numeric(cfg$weight_decay)
  cfg$block_size       <- as.numeric(cfg$block_size)
  cfg$stride           <- as.numeric(cfg$stride)
  cfg$sample_n         <- as.integer(cfg$sample_n)
  cfg$repeat_per_tile  <- as.integer(cfg$repeat_per_tile)
  cfg$min_pts_tile     <- as.integer(cfg$min_pts_tile)
  cfg$cell_size        <- as.numeric(cfg$cell_size)
  cfg$quantile         <- as.numeric(cfg$quantile)

  message(">> Calling Python train.train_model(config)")
  res <- py_train$train_model(cfg)

  if (is.null(res)) {
    stop("Python returned NULL. Check console for Python errors.", call. = FALSE)
  }

  res <- reticulate::py_to_r(res)

  # ---- optional cleanup: delete NPZ tiles if requested ----
  if (isTRUE(cfg$delete_tiles_after_train)) {
    train_dir <- file.path(cfg$out_dir, "train")
    val_dir   <- file.path(cfg$out_dir, "val")
    test_dir  <- file.path(cfg$out_dir, "test")

    message(">> Deleting NPZ tiles in ", cfg$out_dir, " (train/val/test) ...")

    for (d in c(train_dir, val_dir, test_dir)) {
      if (dir.exists(d)) {
        unlink(d, recursive = TRUE, force = TRUE)
        message("   - Deleted directory: ", d)
      }
    }
  }

  res
}
