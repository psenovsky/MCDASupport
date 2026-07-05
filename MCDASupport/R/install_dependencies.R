#' Install dependencies for MCDASupport package
#'
#' @description
#' installs all required dependencies for usage of MCDASupport package.
install_dependencies <- function() {
  print("Checking for missing dependencies.")
  packages <- c(
    "data.tree",
    "diagram",
    "dplyr",
    "graphics",
    "grDevices",
    "igraph",
    "lpSolve",
    "mathjaxr",
    "plotly",
    "quadprog",
    "stats",
    "R6",
    "tidyr",
    "visNetwork")
  dep <- setdiff(packages, rownames(installed.packages()))
  if (length(dep) > 0) {
    cat(paste0("packages: ", dep, " are missing, trying to install them\n"))
    install.packages(dep)
    print("\nInstallation of missing packages completed. Please review the console to check if the installation finished without errors.")
  } else {
    print("No missing dependency detected, everything seems to be prepared for MCDASupport package usage.")
  }

}
