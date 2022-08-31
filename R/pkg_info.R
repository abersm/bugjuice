#' Silently load 1 or more packages
#'
#' @param ... Enter comma separated list of unquoted or quoted package names without `c()`
#' @export
lib <- function(...) {
  pkgs <- dots_as_quoted(...)
  not_installed <- pkgs[pkgs %!in% installed_packages()]
  if (any(idx_github <- grepl("/", not_installed, fixed = TRUE))) {
    pkg_required("devtools")
    lapply(not_installed[idx_github], function(x) invisible(devtools::install_github(x)))
    not_installed <- not_installed[!idx_github]
  }
  if (length(not_installed) > 0) {
    z <- tryCatch(utils::install.packages(not_installed, dependencies = TRUE), warning = function(e) TRUE, error = function(e) TRUE)
    if (!is.null(z) && z) {
      pkg_required("BiocManager")
      tryCatch(suppress(BiocManager::install(not_installed)), error = function(e) TRUE)
    }
  }
  lapply(pkgs, function(x) tryNULL(suppressWarnings(suppressPackageStartupMessages(library(x, character.only = TRUE)))))
  if (any(idx <- pkgs %!in% installed_packages())) {
    message("Unable to install packages ", paste(shQuote(pkgs[idx]), collapse = ", "))
  } else {
    message("All packages successfully installed")
  }
  invisible(pkgs)
}

#' Extract names of functions in package containing a specified pattern
#'
#' @param pkg Package. Enter as quoted or unquoted package name
#' @param pattern Pattern in name of object
#' @param internal If `TRUE` (default), internal objects are included in output
#' @param external If `TRUE` (default), external objects are included in output
#' @returns Character vector of function names
#' @export
pkg_fns <- function(pkg, pattern = "", internal = TRUE, external = TRUE) {
  pkg <- get_input(pkg)
  pkg_env <- getNamespace(pkg)
  fns <- unlist(eapply(pkg_env, function(x) is.function(x) || inherits(x, "ggproto"), all.names = internal, USE.NAMES = TRUE))
  fns <- names(fns)[fns]
  if (!external) {
    exports <- if (pkg == "base") names(.BaseNamespaceEnv) else names(pkg_env[[".__NAMESPACE__."]][["exports"]])
    fns <- Setdiff(fns, exports)
  }
  sort.int(grep(pattern = pattern, x = fns, value = TRUE))
}

#' Stop function if package is unavailable
#'
#' @param x Name of package. Enter as quoted package name
#' @noRd
pkg_required <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Must install package ", x)
  }
}

#' List all installed packages
#'
#' Functionality from Hadley Wickham's excellent package helpr
#' @returns Character vector of all installed packages (whether loaded currently or not)
#' @noRd
installed_packages <- function() {
  user_libPaths <- normalizePath(.libPaths())
  uniqueLibPaths <- subset(user_libPaths, !duplicated(user_libPaths))
  paths <- unlist(lapply(uniqueLibPaths, dir, full.names = TRUE))
  desc <- file.path(paths, "DESCRIPTION")
  desc <- desc[file.exists(desc)]
  vapply(desc, read.dcf, character(1), fields = c("Package"), USE.NAMES = FALSE)
}
