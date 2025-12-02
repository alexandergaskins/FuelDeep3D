#' Add ground as class 3 using CSF after FuelDeep3D prediction
#' @param in_las  LAS/LAZ with FuelDeep3D predictions in Classification (0,1,2)
#' @param out_las Output LAS/LAZ with ground set to class 3
#' @param csf_args list of arguments passed to lidR::csf()
#' @export
add_ground_csf <- function(in_las,
                           out_las,
                           csf_args = list()) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Please install it with install.packages('lidR').")
  }
  if (!requireNamespace("RCSF", quietly = TRUE)) {
    stop("Package 'RCSF' is required for csf(). Please install it with install.packages('RCSF').")
  }
  
  message(">> Reading LAS: ", in_las)
  las <- lidR::readLAS(in_las)
  if (lidR::is.empty(las)) stop("Input LAS is empty: ", in_las)
  
  # NEW — normalize heights before CSF
  las <- lidR::normalize_height(las, lidR::knnidw())
  
  # Save network predictions (0,1,2)
  orig_class <- las$Classification
  
  # Build CSF algorithm (user can tune via csf_args)
  alg <- do.call(lidR::csf, csf_args)
  
  message(">> Running CSF ground classification ...")
  las <- lidR::classify_ground(las, alg)
  
  # lidR ground class (LASGROUND == 2)
  is_ground <- las$Classification == lidR::LASGROUND
  
  # Assign ground = 3, keep original classes elsewhere
  las$Classification[is_ground]  <- 3L
  las$Classification[!is_ground] <- orig_class[!is_ground]
  
  message(">> Writing LAS with 4 classes (0,1,2 + ground=3) to: ", out_las)
  lidR::writeLAS(las, out_las)
  
  invisible(out_las)
}
