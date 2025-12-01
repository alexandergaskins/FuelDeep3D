#' Default config 
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
      rigidness = 2,
      cloth_resolution = 0.5,
      time_step = 0.65,
      class_threshold = 0.2
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
