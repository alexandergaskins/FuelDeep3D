#' Ensure a Conda environment and Python deps for FuelDeep3D
#'
#' This helper will:
#' 1. Check if the Conda env exists; if not, create it with the requested
#'    Python version.
#' 2. Check whether key Python modules are importable from that env.
#'    If any are missing, it installs the full dependency set via pip.
#' 3. Activate the env for the current R session via `reticulate::use_condaenv()`.
#'
#' @param envname Name of the Conda environment to use/create.
#'   Defaults to `"pointnext"`.
#' @param python_version Python version to use when creating the env
#'   (ignored if env already exists). Default: `"3.10"`.
#' @param reinstall Logical; if `TRUE`, always reinstall Python deps
#'   even if key modules are already present. Default: `FALSE`.
#'
#' @return Invisibly returns `TRUE` if the environment exists/was created
#'   and is ready to use.
#' @export
ensure_py_env <- function(envname = "pointnext",
                          python_version = "3.10",
                          reinstall = FALSE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it first.",
         call. = FALSE)
  }

  # Make sure Conda is available
  conda_bin <- reticulate::conda_binary()
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    stop(
      "No Conda installation detected.\n",
      "Please install Anaconda or Miniconda first, then restart R.\n",
      "Examples:\n",
      "  - https://docs.anaconda.com/anaconda/install/\n",
      "  - https://docs.conda.io/en/latest/miniconda.html",
      call. = FALSE
    )
  }

  # Does the env already exist?
  envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
  env_exists <- !is.null(envs) && envname %in% envs$name

  if (!env_exists) {
    message(">> Conda env '", envname, "' not found; creating it with Python ", python_version, " ...")
    reticulate::conda_create(
      envname  = envname,
      packages = paste0("python=", python_version)
    )
    message(">> Created Conda env '", envname, "'.")
  } else {
    message(">> Reusing existing Conda env '", envname, "'.")
  }

  # Install deps (conditionally or forced)
  install_py_deps(envname = envname, only_if_missing = !reinstall)

  # Activate env for this R session
  reticulate::use_condaenv(envname, required = TRUE)
  message(">> Activated Conda env '", envname, "' for this R session.")

  invisible(TRUE)
}


#' Install Python dependencies into a Conda environment
#'
#' This helper uses `reticulate::conda_install()` with pip to install
#' all Python packages needed by FuelDeep3D into a given Conda env.
#'
#' If `only_if_missing = TRUE`, it first checks for a few key modules
#' (`torch`, `numpy`, `sklearn`, `laspy`, `tqdm`). If all of these are
#' importable, it skips installation.
#'
#' @param envname Name of the Conda environment (default: `"pointnext"`).
#'   The environment must already exist (e.g. created with `conda_create()`
#'   or via `ensure_py_env()`).
#' @param only_if_missing Logical; if `TRUE` (default), it will skip
#'   installation when all key modules are already present.
#'
#' @return Invisibly returns `TRUE` if installation was run, or `FALSE`
#'   if it was skipped because everything was already present.
#' @export
install_py_deps <- function(envname = "pointnext",
                            only_if_missing = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it first.",
         call. = FALSE)
  }

  # Optional: check for missing modules first
  if (only_if_missing) {
    message(">> Checking key Python modules in env '", envname, "' ...")

    # Temporarily point reticulate to that env so py_module_available()
    # checks the right interpreter.
    reticulate::use_condaenv(envname, required = TRUE)

    key_modules <- c("torch", "numpy", "sklearn", "laspy", "tqdm")
    missing <- key_modules[
      !vapply(
        key_modules,
        function(mod) reticulate::py_module_available(mod),
        logical(1)
      )
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
  }

  # Requirements from your requirements.txt as pip arguments
  pip_args <- c(
    # Deep learning (CUDA 12.1 builds) – only works on a CUDA 12.1-capable GPU machine
    "--extra-index-url", "https://download.pytorch.org/whl/cu121",
    "torch==2.5.1+cu121",
    "torchvision==0.20.1+cu121",
    "torchaudio==2.5.1+cu121",

    # Core numerics
    "numpy~=2.2",
    "scipy~=1.15",
    "scikit-learn~=1.7",
    "tqdm>=4.66",

    # Point cloud IO
    "laspy~=2.6",
    "lazrs~=0.7",

    # Metrics / plots
    "matplotlib~=3.10",
    "seaborn~=0.13"
  )

  message(">> Installing Python deps into Conda env '", envname, "' using pip via reticulate::conda_install() ...")

  reticulate::conda_install(
    envname  = envname,
    packages = pip_args,
    pip      = TRUE
  )

  message(">> Finished installing Python deps into '", envname, "'.")
  invisible(TRUE)
}
