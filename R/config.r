#' Default config 
#' Create model configuration
#'
#' @param las_path Path to input LAS file
#' @param out_dir Directory where training tiles are saved
#' @param out_pred_dir Directory where predictions are written
#' @param model_path Path to pre-trained .pth model
#' @param device Device to use ("cpu" or "cuda")
#' @param block_size Tile size in meters
#' @param stride Overlap stride
#' @param sample_n Number of points sampled per tile
#' @param repeat_per_tile Number of augmentations per tile
#' @param min_pts_tile Minimum number of points to keep a tile
#' @param val_split Fraction for validation split
#' @param test_split Fraction for test split
#' @param seed Random seed
#' @param batch_size Batch size for training
#' @param epochs Number of training epochs
#' @param learning_rate Optimizer learning rate
#' @param weight_decay L2 regularization strength
#' @param cell_size Grid cell size for height normalization
#' @param quantile Quantile threshold used in metrics
#' @param num_classes Number of segmentation classes
#' @param csf_args Additional arguments passed to RCSF::csf()
#' @param delete_tiles_after_train Whether to delete generated tiles after training
#'
#' @return A list containing all configuration parameters
#' @export
config <- function(
    las_path      = system.file("extdata", "trees.las", package = "FuelDeep3D"),
    out_dir       = getwd(),
    out_pred_dir  = getwd(),
    model_path    = system.file("extdata", "best_model.pth", package = "FuelDeep3D"),
    device        = NULL,     # NULL => Python picks cuda/cpu
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
    cell_size     = 0.25,   # HAG grid size (m)
    quantile      = 0.05,
    num_classes  = 3,        # <--- NEW: default is 3
    
    # NEW: CSF parameters stored inside cfg
    csf_args = list(
      rigidness = 4,                   # stiffness of the cloth (2 = balanced, stable ground detection)
      cloth_resolution = 0.25,        # grid size of cloth in meters (0.5 = good detail for forests)
      time_step = 0.65,               # simulation step size (controls cloth drop speed/stability)
      class_threshold = 0.05          # max distance from cloth to classify a point as ground
    ),
    
    delete_tiles_after_train = TRUE   # <--- turn on deletion
    
)  {
  
  if (!num_classes %in% c(3, 4)) {
    stop("num_classes must be 3 or 4.
          3 = normal model prediction
          4 = model prediction + CSF-based ground class")
  }
  
  as.list(environment())
}
