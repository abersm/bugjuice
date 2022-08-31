# Test paths --------------------------------------------------------------

#' Determine whether input is a file path (and not a directory)
#'
#' @param x Character vector of file paths
#' @returns Logical with length equal to input length
#' @export
file_exists <- function(x) file.exists(x) & !dir.exists(x)

#' Alias for `file_exists`
#'
#' @rdname file_exists
#' @export
is_file <- file_exists

#' Determine whether input is a directory (and not a file path)
#'
#' @param x Character vector of directory paths
#' @returns Logical with length equal to input length
#' @export
dir_exists <- function(x) dir.exists(x)

#' Alias for `dir_exists`
#'
#' @rdname dir_exists
#' @export
is_dir <- dir_exists

# Create paths ------------------------------------------------------------

#' Build a file path by successive folder names
#'
#' @param ... Either path to directory or comma separated list of quoted or unquoted folder names following desktop not wrapped in `c()`
#' @param start Beginning of file path up to desktop
#' @returns File path without terminal "/"
#' @export
paste_path <- function(..., start = desktop_path()) {
  paste0(sub(pattern = "\\/$", replacement = "", x = start), if (n_dots(...) > 0L) "/" else "", paste(dots_as_quoted(...), collapse = "/"))
}

#' Path to desktop
#'
#' @param include_slash If `TRUE` (default), path ends with "Desktop/". If `FALSE`, path ends with Desktop
#' @returns Path to desktop location
#' @export
path_to_desktop <- function(include_slash = TRUE) {
  paste0(path.expand("~/"), "Desktop", if (include_slash) "/" else "")
}

#' Alias for path_to_desktop
#'
#' @rdname path_to_desktop
#' @export
desktop_path <- path_to_desktop

# Search paths ------------------------------------------------------------

#' List files in folders
#'
#' @param ... Arguments passed to `paste_path()`. Enter comma separated list of quoted or unquoted folder names following a starting path not wrapped in `c()`. Starting path is specified using start argument. Default uses desktop path as start argument. Do not include file name in `...`
#' @param pattern Pattern in file name to search
#' @param ext File extension. Enter as quoted file extension with or without "."
#' @param pattern_exclude Pattern in file name to exclude from search
#' @param all_terms If `TRUE` (default) and multiple patterns entered, result is files containing all patterns
#' @param ignore_case If `FALSE` (default), case of `pattern` is ignored in search. If `TRUE`, search for `pattern` is case sensitive
#' @param recursive If `FALSE` (default), only files in specified path returned
#' @param exact_match If `FALSE` (default), entry for `...` doesn't need to be exact match of file name
#' @param start_path Directory used as prefix to the path built using dots
#' @returns Character vector of file paths
#' @export
list_files <- function(..., pattern = NULL, ext = NULL, pattern_exclude = NULL, all_terms = TRUE, ignore_case = TRUE, exact_match = FALSE, recursive = TRUE, start_path = desktop_path(include_slash = FALSE)) {
  n <- n_dots(...)
  file_path <- if (n == 1L && dir.exists(...)) c(...) else paste_path(..., start = start_path)
  files <- if (length(pattern) < 2L || !all_terms) {
    list.files(path = file_path, pattern = paste(pattern, collapse = "|"), recursive = recursive, ignore.case = ignore_case, full.names = TRUE)
  } else {
    files <- lapply(pattern, function(x) list.files(path = file_path, pattern = x, full.names = TRUE, recursive = recursive, ignore.case = ignore_case))
    # Reduce(base::intersect, files)
    unlist(files, use.names = FALSE)
  }
  if (!is.null(pattern_exclude)) {
    files <- files[!grepl(pattern_exclude, files, ignore.case = ignore_case)]
  }
  if (exact_match) {
    file_name <- basename(files)
    file_name_no_ext <- str_remove_ext(file_name)
    if (ignore_case) {
      file_name_lower <- tolower(file_name)
      file_name_no_ext_lower <- tolower(file_name_no_ext)
      files <- files[endsWith(file_name_no_ext_lower, file_name_lower)]
    } else {
      files <- files[endsWith(file_name_no_ext, file_name)]
    }
  }
  if (length(files) == 0L) {
    dots <- dots_as_quoted(...)
    dots <- dots[1L]
    files <- list.files(path = start_path, pattern = dots, recursive = recursive, ignore.case = ignore_case, full.names = TRUE)
  }
  if (is.null(ext)) return(files)
  if (!grepl(pattern = "\\.", x = ext)) {
    ext <- paste0(".", ext)
  }
  if (ignore_case) {
    files[endsWith(tolower(files), tolower(ext))]
  } else {
    files[endsWith(files, ext)]
  }
}

#' Identify files/folder with paths that contain a specified pattern
#'
#' If multiple search terms are entered, output includes paths containing all search terms
#' @param ... Search terms in file name. Enter as comma separated list of quoted or unquoted terms or regex without `c()`
#' @param ext File extension. Enter as quoted extension with or without "."
#' @param ignore_case If `FALSE` (default), case of search terms entered in `...` are ignored. If `TRUE`, search for terms entered as `...` is case sensitive
#' @param start_path Directory to limit results of search
#' @param recursive If `TRUE` (default), all folders and subfolders in `start_path` are searched
#' @param files_only If `TRUE`, output limited to paths containing an extension (i.e. not folders)
#' @returns Character vector of paths
#' @noRd
path_which_contains <- function(..., ext = NULL, ignore_case = TRUE, start_path = desktop_path(include_slash = FALSE), recursive = TRUE, files_only = FALSE) {
  pattern <- dots_as_quoted(...)
  n_patterns <- n_dots(...)
  ext <- if (!is.null(ext)) {
    sprintf("\\.%s$", sub("^\\.", "", ext))
  } else if (files_only) {
    "\\.[^.]+$"
  } else {
    NULL
  }
  all_files <- list.files(path = start_path, pattern = ext, full.names = TRUE, ignore.case = TRUE, recursive = recursive)
  if (length(all_files) == 0L) return(character(0))
  if (n_patterns > 1L) {
    idx <- Reduce(`+`, lapply(pattern, grepl, x = all_files, ignore.case = ignore_case)) == n_patterns
    all_files[idx]
  } else {
    grep(pattern = pattern, x = all_files, ignore.case = ignore_case, value = TRUE)
  }
}

# Search for folders ------------------------------------------------------

#' Search for folders
#'
#' @param ... Arguments passed to `paste_path()`. Enter comma separated list of quoted or unquoted folder names following a starting path not wrapped in `c()`. Starting path is specified using start argument. Default uses desktop path as start argument
#' @param pattern Pattern in folder name to search
#' @param pattern_exclude Pattern in folder name to exclude from search
#' @param all_terms If `TRUE` (default) and multiple patterns entered, result is folders containing all patterns
#' @param ignore_case If `FALSE` (default), case of `pattern` is ignored in search. If `TRUE`, search for `pattern` is case sensitive
#' @param recursive If `TRUE` (default), all subfolders are searched
#' @param exact_match If `FALSE` (default), entry for `...` doesn't need to be exact match of folder name
#' @param start_path Directory used as prefix to the path built using dots
#' @param incl_hidden If `TRUE` (default), hidden folders included in output
#' @returns Character vector of folder paths
#' @export
list_dirs <- function(..., pattern = NULL, pattern_exclude = NULL, all_terms = TRUE, ignore_case = TRUE,  recursive = TRUE, exact_match = FALSE, start_path = desktop_path(include_slash = FALSE), incl_hidden = TRUE) {
  path <- if (n_dots(...) == 1L && dir.exists(...)) c(...) else paste_path(..., start = start_path)
  folders <- list.dirs(path, full.names = TRUE, recursive = recursive)
  if (!is.null(pattern_exclude)) {
    folders <- folders[!grepl(pattern_exclude, basename(folders), ignore.case = ignore_case)]
  }
  if (!incl_hidden) {
    folders <- folders[!grepl("/\\.", folders)]
  }
  n_patterns <- length(pattern)
  if (n_patterns > 0L && length(folders) != 0L) {
    if (exact_match) {
      if (n_patterns > 1L) {
        if (all_terms) {
          stop("Conflicting input to list_dirs(): when length('pattern') > 1L and 'exact_match' = TRUE, 'all_terms' must be FALSE", call. = FALSE)
        } else {
          idx <- if (ignore_case) {
            basename(folders) %in% pattern
          } else {
            tolower(basename(folders)) %in% tolower(pattern)
          }
          folders <- folders[idx]
        }
      } else {
        idx <- if (ignore_case) {
          basename(folders) == pattern
        } else {
          tolower(basename(folders)) == tolower(pattern)
        }
        folders <- folders[idx]
      }
    } else {
      pattern <- if (all_terms) pattern else paste(pattern, collapse = "|")
      for (i in pattern) {
        folders <- folders[grepl(i, basename(folders), ignore.case = ignore_case)]
      }
    }
  }
  folders
}

#' List folders in top level directory
#'
#' @returns Character vector of folder names in top level directory
#' @noRd
top_level_dirs <- function() list.files("/")

# Open files --------------------------------------------------------------

#' Open file
#'
#' @param file_name Either file path or name of file with or without file extension. Input can be quoted or unquoted
#' @param ... Comma separated list without of folders (in order) to open in order to arrive at file location not wrapped in `c()`. Folder names can be entered as quoted or unquoted if there are no spaces. If there are spaces, must quote file name
#' @param ext File extension with or without ".". Default is `"R"`
#' @param desktop If `TRUE` (default), files can be located on desktop
#' @param recursive If `FALSE` (default), only files in specified path returned
#' @param ignore_case If `TRUE` (default), case of file name is ignored
#' @export
open_file <- function(file_name, ..., ext = "R", desktop = TRUE, recursive = TRUE, ignore_case = TRUE) {
  file_name <- get_input(file_name)
  dots <- dots_as_quoted(...)
  # Line below changed from file.exists to file_exists
  if (all(str_has_ext(file_name)) && any(idx <- file_exists(file_name)) && is.null(dots)) {
    file_full_name <- file_name[idx]
    #file_name <- file_name[!idx]
    lapply(file_full_name, function(x) system(paste("'open'", shQuote(x))))
    invisible(file_full_name)
    file_name <- file_name[!idx]
    if (length(file_name) > 0L) suppress(open_file(file_name)) else return(invisible(TRUE))
  }
  file_name <- .get_file_path(x = file_name, ext = ext, dots = dots, desktop = desktop, ignore_case = ignore_case, recursive = recursive, silent = TRUE)
  n <- length(file_name)
  if (n == 0L || !any(idx <- file_exists(file_name))) return(NULL)
  lapply(file_name[idx], function(x) system(paste("'open'", shQuote(x))))
  invisible()
}

# Open folders ------------------------------------------------------------

#' Open finder window for folder
#'
#' @param ... Either a vector (or comma separated list) of full folder paths or path created by passing arguments to `paste_path`. Enter comma separated list of quoted or unquoted folder names following a starting path not wrapped in `c()`. Starting path is specified using start argument. Default uses desktop path as start argument
#' @param start Beginning of file path up to desktop
#' @param silent If `TRUE`, messages will be suppressed
#' @export
open_folder <- function(..., start = desktop_path(), silent = FALSE) {
  paths <- dots_as_quoted(...)
  paths <- tryCatch(c(...), error = function(e) paths)
  idx <- dir.exists(paths)
  if (all(idx)) {
    system(paste("'open'", shQuote(paths)))
  } else {
    paths <- paste0(sub(pattern = "\\/$", replacement = "", x = start), if (n_dots(...) > 0L) "/" else "", paste(paths, collapse = "/"))
    idx <- dir.exists(paths)
    if (any(idx)) {
      if (all(idx)) {
        system(paste("'open'", shQuote(paths)))
      } else {
        if (silent) {
          message("In open_folder(), the following directions do not exist:\n\n", paste(shQuote(paths[!idx]), collapse = "\n"))
        }
        system(paste("'open'", shQuote(paths[idx])))
      }
    }
  }
}

#' Open working directory folder or R file located in working directory
#'
#' @param file_name File name. Enter as quoted or unquoted file name, with or without extension. If empty, working directory folder will be opened
#' @param ignore_case If `TRUE` (default), case of `file_name` is ignored
#' @param recursive If `TRUE` (default), subfolders in working directory are searched
#' @export
open_wd <- function(file_name = NULL, ignore_case = TRUE, recursive = TRUE) {
  pkg_required("rstudioapi")
  file_name <- get_input(file_name)
  if (is.null(file_name)) return(system("open ."))
  file_name <- str_force_ext(file_name, ext = "R")
  if (file.exists(file_name)) {
    rstudioapi::navigateToFile(file_name)
  } else {
    files <- list.files(path = getwd(), pattern = str_remove_ext(file_name), full.names = TRUE, recursive = recursive, ignore.case = ignore_case)
    idx <- grepl("\\.R$", files, ignore.case = TRUE)
    n_files <- sum(idx)
    if (n_files == 1L) {
      rstudioapi::navigateToFile(files[idx])
    } else if (n_files > 1L) {
      files <- files[idx]
      exact_match <- basename(files) == file_name
      n_exact_match <- sum(exact_match)
      if (n_exact_match != 0L) {
        if (n_exact_match == 1L) return(rstudioapi::navigateToFile(files[exact_match]))
        files <- files[exact_match]
        n_files <- length(files)
      }
      z <- sub(pattern = getwd(), replacement = "", x = files)
      message(sprintf("\n%s files with the pattern '%s' identified:\n%s\n\nWill use first file:\n%s\n", n_files, file_name, paste(z, collapse = "\n"), z[1L]))
      rstudioapi::navigateToFile(files[1L])
    } else {
      stop(sprintf("\nNo working directory files with the pattern '%s' identified", file_name))
    }
  }
}

#' Open finder window for desktop folder
#'
#' @export
open_desktop <- function() system(paste("'open'", shQuote(desktop_path(include_slash = FALSE))))

# File extensions ---------------------------------------------------------

#' Extract file extension
#'
#' Functionality from `file_ext` function in tools package
#' @param x Character vector of file paths
#' @returns Character vector of file extension not including "."
#' @export
str_file_ext <- function(x) {
  idx <- regexpr("\\.[^.]+$", x)
  gsub(" copy", "", ifelse(idx > -1L, substr(x, idx + 1L, nchar(x)), ""))
}

#' Alias for str_file_ext
#'
#' @rdname str_file_ext
#' @export
file_ext <- str_file_ext

#' Set file extension
#'
#' @param x File name or file path. Enter as character vector. Length can be > 1
#' @param value File extension. Enter as quoted file extension without ".". Must have length of 1 or length equal to `length(x)`
#' @returns Input with new file extension specified by `value`
#' @export
`file_ext<-` <- function(x, value) sprintf("%s.%s", sub(pattern = "\\.[^.]+$", replacement = "", x = x), value)

#' Remove file extension from string
#'
#' @param x File name or file path
#' @returns File name or file path without extension
#' @export
str_remove_ext <- function(x) sub(pattern = "\\.[^.]+$", replacement = "", x = x)

#' Alias for str_remove_ext
#'
#' @rdname str_remove_ext
#' @export
remove_ext <- str_remove_ext

#' Determine whether a file path contains an extension
#'
#' @param x Character vector of file paths
#' @param ext Character vector of length 1 containing file extension with or without ".". If `NULL` (default), function will test whether `x` contains any file extension
#' @param ignore_case If `TRUE` (default), search for `ext` will be case insensitive
#' @returns Logical vector of length equal to input
#' @export
has_ext <- function(x, ext = NULL, ignore_case = TRUE) {
  ext <- if (is.null(ext)) "\\.[^.]+$" else sprintf("\\.%s$", sub(".", "", ext, fixed = TRUE))
  grepl(ext, x, ignore.case = ignore_case)
}

#' Alias for has_ext
#'
#' @rdname has_ext
#' @export
str_has_ext <- has_ext

#' Ensure that string has specific file extension
#'
#' @param x Character vector of file paths with or without extension included
#' @param ext File extension with or without "."
#' @returns Character vector of file paths with same length as `x` containing `ext` as extension
#' @export
str_force_ext <- function(x, ext) {
  sprintf("%s.%s", sub(pattern = "\\.[^.]+$", replacement = "", x = x), sub(pattern = "\\.", replacement = "", x = ext))
}

#' Alias for str_force_ext
#'
#' @rdname str_force_ext
#' @export
str_add_ext <- str_force_ext

#' Alias for str_force_ext
#'
#' @rdname str_force_ext
#' @export
str_replace_ext <- str_force_ext

# Internal ----------------------------------------------------------------

#' Identify potential matching file
#'
#' @param x Base name of file. Doesn't matter if extension is included, it will be ignored
#' @param ext File extension without ".". Enter as quoted extension
#' @param dots Character vector containing sequential folders in path to file
#' @param desktop If `TRUE` (default), files can be located on desktop
#' @param recursive If `FALSE` (default), subfolders are not searched
#' @param ignore_case If `FALSE` (default), case of `x` is considered in search. If `TRUE`, case of `x` is not considered in search
#' @param silent If `FALSE` (default) a message will be printed if 0 or > 1 files identified
#' @returns File path including extension
#' @noRd
.get_file_path <- function(x, ext, dots = NULL, desktop = TRUE, recursive = TRUE, ignore_case = TRUE, silent = FALSE) {
  # If file exists, return input
  if (all(file.exists(x), na.rm = TRUE)) return(x)
  x_regex <- if (grepl("\\(|\\)", x)) {
    gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", x))
  } else {
    x
  }
  desktop_dir <- desktop_path(FALSE)

  if (length(dots) == 0L) {
    # If dots are empty, x is a file name. Build search path for file with extension
    ext <- ext %||% str_file_ext(x)
    potential_matches <- .list_files_with_ext(x_regex, ext = ext, starting_directory = desktop_dir, recursive = recursive, ignore_case = ignore_case)
    potential_matches <- .prioritize_file_paths(pattern = x, files = potential_matches, ignore_case = ignore_case)
    n_files <- length(potential_matches)
    if (n_files > 1L) {
      potential_dirs <- dirname(potential_matches)
      wd <- getwd()
      files <- if (desktop && any(idx <- potential_dirs == desktop_dir)) {
        potential_matches[idx]
      } else if (any(idx <- potential_dirs == wd)) {
        potential_matches[idx]
      } else if (any(idx <- grepl(wd, potential_dirs))) {
        potential_matches[idx]
      } else {
        n_folders <- str_count(potential_dirs, "/")
        idx <- n_folders == min(n_folders)
        potential_matches[idx]
      }
      if (length(files) > 1L) {
        if (!silent) {
          z <- sub(pattern = desktop_path(), replacement = "", x = files)
          message(sprintf("\n%s files with the pattern '%s' identified:\n%s\n\nWill use first file:\n%s\n", n_files, x, paste(z, collapse = "\n"), z[1L]))
        }
        files[1L]
      } else {
        files
      }
    } else if (n_files == 1L) {
      potential_matches
    } else {
      if (!silent) {
        message(sprintf("\nNo %s files with the pattern '%s' identified\n", ext, x))
      }
      character(0)
    }
  } else {
    # If dots are entered, determine whether x is a file
    x_original <- x
    x <- str_force_ext(x = x, ext = ext %||% str_file_ext(x))
    dots <- paste(dots, collapse = "/")
    folder <- paste0(desktop_path(), dots)

    if (dir.exists(folder)) {
      # If x is a file, dots should be vector of folders to open from desktop
      files <- list.files(path = folder, pattern = x_regex, recursive = recursive, full.names = TRUE, ignore.case = ignore_case)
      if (length(files) == 0L) {
        files <- list.files(path = folder, pattern = str_remove_ext(x_regex), recursive = recursive, full.names = TRUE, ignore.case = ignore_case)
        if (length(files) == 0L) {
          files <- if (!is.null(ext)) {
            list.files(path = folder, pattern = sprintf("\\.%s|\\.%s", tolower(ext), toupper(ext)), recursive = recursive, full.names = TRUE, ignore.case = ignore_case)
          } else {
            list.files(path = folder, pattern = sprintf("\\.%s|\\.%s", x_original, toupper(ext)), recursive = recursive, full.names = TRUE, ignore.case = ignore_case)
          }
        }
      }
    } else {
      # If folder doesn't exist, try using x as the first folder from desktop
      files <- list.files(path = paste0(desktop_dir, "/", x_original, "/", dots, pattern = sprintf("\\.%s|\\.%s", tolower(ext), toupper(ext)), recursive = recursive, full.names = TRUE, ignore.case = ignore_case))
    }
    # Limit search to those with indicated file extension
    files <- files[grepl(pattern = sprintf("\\.%s$", ext), x = files, ignore.case = TRUE)]
    n_files <- length(files)
    if (n_files == 1L) {
      # If results include 1 file, return the file path
      files
    } else if (n_files > 1L) {
      # If results include > 1 file, search for identical match
      exact_match <- str_remove_ext(basename(files)) == str_remove_ext(x)
      n_exact_match <- sum(exact_match)
      if (n_exact_match != 0L) {
        if (n_exact_match == 1L) return(files[exact_match])
        files <- files[exact_match]
        n_files <- length(files)
      }
      if (!silent) {
        z <- sub(pattern = desktop_path(), replacement = "", x = files)
        message(sprintf("\n%s files with the pattern '%s' identified:\n%s\n\nWill use first file:\n%s\n", n_files, x, paste(z, collapse = "\n"), z[1L]))
      }
      files[1L]
    } else {
      if (!silent) {
        message(sprintf("\nNo %s files with the pattern '%s' identified\n", ext, x))
      }
      character(0)
    }
  }
}

#' Create file path that does not overwrite current files
#'
#' @param x File name. Enter as string with or without extension
#' @param ext File extension. Enter as string without "."
#' @param directory Path to directory where file should be saved. Default is `desktop_path()`
#' @returns File path
#' @noRd
.safe_file_path <- function(x, ext, directory = desktop_path()) {
  x <- str_force_ext(x, ext)
  x <- .safe_name(x, list.files(path = directory, pattern = sprintf("\\.%s$", ext), ignore.case = TRUE))
  paste(gsub(pattern = "/$", replacement = "", x = directory), x, sep = "/")
}

#' List files with known extension (independent of extension case)
#'
#' @param x Base name of file. Doesn't matter if extension is included, it will be ignored
#' @param ext File extension without ".". Enter as quoted extension
#' @param starting_directory Highest level directory to conduct search. Default is desktop
#' @param recursive If `FALSE` (default), subfolders are not searched
#' @param ignore_case If `FALSE` (default), case of `x` is considered in search. If `TRUE`, case of `x` is not considered in search. Regardless, search for ext is case insensitive
#' @returns Full file paths matching matching search criteria
#' @noRd
.list_files_with_ext <- function(x, ext, starting_directory = desktop_path(FALSE), recursive = TRUE, ignore_case = TRUE) {
  x <- str_remove_ext(x)
  x_regex <- if (grepl("\\(|\\)", x)) {
    gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", x))
  } else {
    x
  }
  x_regex <- paste0(x_regex, ".", tolower(ext), "$|", x_regex, ".", toupper(ext), "$")
  list.files(path = starting_directory, pattern = x_regex, full.names = TRUE, recursive = recursive, ignore.case = ignore_case)
}

#' Prioritize a list of file paths returned from search
#'
#' Search for files with exact match, then partial match if no exact matches
#' @param pattern Character vector of length 1 containing file name without extension
#' @param files Character vector of full file paths to evaluate
#' @param ignore_case If `TRUE`, case of `x` is ignored. Default is `TRUE`
#' @returns Character vector containing 0 or more file paths
#' @noRd
.prioritize_file_paths <- function(pattern, files, ignore_case = TRUE) {
  file_names <- str_remove_ext(basename(files))
  idx <- file_names == pattern
  idx <- if (sum(idx) > 0L) {
    idx
  } else if (ignore_case) {
    file_names <- tolower(file_names)
    pattern <- tolower(pattern)
    if (sum(idx <- file_names == pattern) > 0L) {
      idx
    } else {
      grep(pattern, file_names, ignore.case = TRUE)
    }
  } else {
    grep(pattern, file_names, ignore.case = FALSE)
  }
  files[idx]
}
