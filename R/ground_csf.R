#' Add a ground class using CSF after FuelDeep3D prediction
#'
#' After FuelDeep3D writes predicted labels into the LAS \code{Classification}
#' field (typically classes 0/1/2), this function runs Cloth Simulation Filtering
#' (CSF) ground detection and assigns detected ground points to class \code{3}.
#' Non-ground points keep their original predicted classes.
#'
#' Internally, heights are normalized (via \code{lidR::normalize_height()})
#' before running CSF.
#'
#' @param in_las Character. Path to an input \code{.las} or \code{.laz} file that
#'   already contains FuelDeep3D predictions in the \code{Classification} field.
#' @param out_las Character. Path to write the output \code{.las} or \code{.laz}
#'   file. Ground points will be set to class \code{3}.
#' @param csf_args List. Arguments forwarded to \code{lidR::csf()} to tune the
#'   CSF algorithm (e.g., \code{rigidness}, \code{cloth_resolution},
#'   \code{time_step}, \code{class_threshold}).
#'
#' @return Invisibly returns \code{out_las} (the output file path).
#'
#' @details
#' Requirements:
#' \itemize{
#' \item \pkg{lidR} for reading/writing LAS and running CSF ground classification.
#' \item \pkg{RCSF} provides the CSF implementation used by \code{lidR::csf()}.
#' }
#'
#' Output classes:
#' \itemize{
#' \item \code{0,1,2} are preserved from the original FuelDeep3D predictions.
#' \item \code{3} is assigned to points classified as ground by CSF.
#' }
#'
#' @examples
#' \dontrun{
#' library(FuelDeep3D)
#'
#' in_file <- system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D")
#' out_file <- file.path(tempdir(), "tree2_ground.laz")
#'
#' add_ground_csf(
#'   in_las  = in_file,
#'   out_las = out_file,
#'   csf_args = list(
#'     rigidness = 4,
#'     cloth_resolution = 0.25,
#'     time_step = 0.65,
#'     class_threshold = 0.05
#'   )
#' )
#' }
#'
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

  las <- lidR::normalize_height(las, lidR::knnidw())

  orig_class <- las$Classification

  alg <- do.call(lidR::csf, csf_args)

  message(">> Running CSF ground classification ...")
  las <- lidR::classify_ground(las, alg)

  is_ground <- las$Classification == lidR::LASGROUND

  las$Classification[is_ground]  <- 3L
  las$Classification[!is_ground] <- orig_class[!is_ground]

  message(">> Writing LAS with 4 classes (0,1,2 + ground=3) to: ", out_las)
  lidR::writeLAS(las, out_las)

  invisible(out_las)
}
