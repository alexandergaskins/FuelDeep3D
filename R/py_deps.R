#' Ensure a Conda environment and Python dependencies for FuelDeep3D
#'
#' @description
#' Ensures a Conda environment exists and installs the Python dependencies required
#' by FuelDeep3D into that environment. The environment is then activated for the
#' current R session using \code{reticulate}.
#'
#' @details
#' This helper performs the following steps:
#' \enumerate{
#'   \item Detects a Conda installation using \code{reticulate::conda_binary()}.
#'   \item Creates the requested environment (if missing) using the requested Python version.
#'   \item Installs Python packages via \code{\link{install_py_deps}}.
#'   \item Activates the environment for the current R session with
#'   \code{reticulate::use_condaenv(..., required = TRUE)}.
#' }
#'
#' Use \code{cpu_only = TRUE} on machines without an NVIDIA GPU / CUDA drivers.
#' If you change \code{cpu_only} after a previous install, use \code{reinstall = TRUE}
#' to force reinstalling dependencies.
#'
#' @param envname Name of the Conda environment to use or create.
#' @param python_version Python version used when creating a new environment.
#'   Ignored if the environment already exists.
#' @param reinstall Logical. If \code{TRUE}, forces dependency installation even if
#'   key modules appear to be installed. Default: \code{FALSE}.
#' @param cpu_only Logical. If \code{TRUE}, installs CPU-only PyTorch wheels.
#'   If \code{FALSE} (default), installs CUDA wheels (cu121) via the PyTorch extra index URL.
#'   CUDA wheels require a compatible NVIDIA GPU + drivers.
#'
#' @return Invisibly returns \code{TRUE} when the environment exists and has been
#'   activated for the current R session.
#'
#' @examples
#' \dontrun{
#' # Requires Conda and an internet connection
#' ensure_py_env(envname = "pointnext", python_version = "3.10", cpu_only = FALSE)
#'
#' # CPU-only install (recommended on most laptops / no NVIDIA GPU)
#' ensure_py_env(envname = "pointnext", python_version = "3.10", cpu_only = TRUE)
#'
#' # Confirm which Python reticulate is using:
#' reticulate::py_config()
#' }
#'
#' @export
ensure_py_env <- function(envname = "pointnext",
                          python_version = "3.10",
                          reinstall = FALSE,
                          cpu_only = FALSE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it first.", call. = FALSE)
  }
  
  conda_bin <- reticulate::conda_binary()
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    stop(
      "No Conda installation detected. Please install Anaconda or Miniconda and restart R.",
      call. = FALSE
    )
  }
  
  envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
  env_exists <- !is.null(envs) && envname %in% envs$name
  
  if (!env_exists) {
    message(">> Conda env '", envname, "' not found; creating it with Python ", python_version, " ...")
    reticulate::conda_create(envname = envname, packages = paste0("python=", python_version))
    message(">> Created Conda env '", envname, "'.")
  } else {
    message(">> Reusing existing Conda env '", envname, "'.")
  }
  
  install_py_deps(envname = envname, only_if_missing = !reinstall, cpu_only = cpu_only)
  
  reticulate::use_condaenv(envname, required = TRUE)
  message(">> Activated Conda env '", envname, "' for this R session.")
  
  invisible(TRUE)
}

#' Install Python dependencies into a Conda environment
#'
#' @description
#' Installs the Python packages required by FuelDeep3D into an existing Conda
#' environment using pip via \code{reticulate::conda_install(..., pip = TRUE)}.
#'
#' @details
#' When \code{only_if_missing = TRUE} (default), the function checks whether key
#' modules are importable (\code{torch}, \code{numpy}, \code{sklearn}, \code{laspy}, \code{tqdm}).
#' If all are available, installation is skipped and the function returns \code{FALSE}.
#'
#' Use \code{cpu_only = TRUE} to install CPU-only PyTorch wheels. Use
#' \code{cpu_only = FALSE} to install CUDA 12.1 wheels (cu121), which requires a
#' compatible NVIDIA GPU and drivers.
#'
#' @param envname Name of the Conda environment. The environment must already exist.
#' @param only_if_missing Logical. If \code{TRUE}, skips installation when all key
#'   modules appear to be installed. If \code{FALSE}, forces installation/update.
#' @param cpu_only Logical. If \code{TRUE}, installs CPU-only PyTorch wheels.
#'   If \code{FALSE} (default), installs CUDA wheels (cu121).
#'
#' @return Invisibly returns \code{TRUE} if installation was executed, or
#'   \code{FALSE} if installation was skipped because required modules were already present.
#'
#' @export
install_py_deps <- function(envname = "pointnext",
                            only_if_missing = TRUE,
                            cpu_only = FALSE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it first.", call. = FALSE)
  }
  
  # Confirm env exists (helps if user calls install_py_deps() directly)
  envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
  if (is.null(envs) || !(envname %in% envs$name)) {
    stop("Conda env '", envname, "' not found. Create it first (e.g., with ensure_py_env()).",
         call. = FALSE)
  }
  
  if (only_if_missing) {
    message(">> Checking key Python modules in env '", envname, "' ...")
    reticulate::use_condaenv(envname, required = TRUE)
    
    key_modules <- c("torch", "numpy", "sklearn", "laspy", "tqdm")
    missing <- key_modules[
      !vapply(key_modules, function(mod) reticulate::py_module_available(mod), logical(1))
    ]
    
    if (length(missing) == 0) {
      message(">> All key modules already installed in '", envname, "'. Skipping Python deps install.")
      return(invisible(FALSE))
    } else {
      message(">> Missing modules detected in '", envname, "': ",
              paste(missing, collapse = ", "),
              ". Installing full dependency set ...")
    }
  } else {
    message(">> Installing (or updating) Python deps in env '", envname, "' ...")
    reticulate::use_condaenv(envname, required = TRUE)
  }
  
  # ---- PyTorch: CPU vs CUDA ----
  if (isTRUE(cpu_only)) {
    torch_args <- c(
      "torch==2.5.1",
      "torchvision==0.20.1",
      "torchaudio==2.5.1"
    )
  } else {
    torch_args <- c(
      "--extra-index-url", "https://download.pytorch.org/whl/cu121",
      "torch==2.5.1+cu121",
      "torchvision==0.20.1+cu121",
      "torchaudio==2.5.1+cu121"
    )
  }
  
  pip_args <- c(
    torch_args,
    "numpy~=2.2",
    "scipy~=1.15",
    "scikit-learn~=1.7",
    "tqdm>=4.66",
    "laspy~=2.6",
    "lazrs~=0.7",
    "matplotlib~=3.10",
    "seaborn~=0.13"
  )
  
  message(">> Installing Python deps into Conda env '", envname, "' using pip via reticulate::conda_install() ...")
  message(">> Mode: ", if (cpu_only) "CPU-only PyTorch wheels" else "CUDA cu121 PyTorch wheels (requires compatible NVIDIA setup)")
  
  reticulate::conda_install(envname = envname, packages = pip_args, pip = TRUE)
  
  message(">> Finished installing Python deps into '", envname, "'.")
  invisible(TRUE)
}
