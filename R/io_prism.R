#' Import prism file
#'
#' Functionality from Yue Jiang's excellent package pzfx
#' @param file_name Name of Prism file. Enter as quoted or unquoted name (later only if no spaces in name)
#' @param ... Arguments passed to `paste_path()`
#' @param table Table in Prism file to import. Enter quoted table name or integer indicating position of table. Default imports all tables
#' @param desktop If `TRUE` (default), files can be uploaded from desktop. If `FALSE`, files can't be uploaded from desktop. Use `FALSE` to upload file from working directory when a file with the same name also exists on the desktop
#' @export
read_prism <- function(file_name, ..., table = NULL, desktop = TRUE) {
  file_name <- get_input(file_name)
  if (file.exists(file_name)) {
    file_location <- file_name
  } else {
    dots <- dots_as_quoted(...)
    file_location <- .get_file_path(x = file_name, ext = "pzfx", dots = dots, desktop = desktop)
    if (is.null(file_location)) return(invisible())
  }
  tables <- table %||% prism_sheets(file_location)
  lapply(set_names(tables), function(x) .read_pzfx(file_location, table = x))
}

#' Get names of sheets in prism file
#'
#' Functionality from Yue Jiang's excellent package pzfx
#' @param file_path Path to file
#' @export
prism_sheets <- function(file_path) {
  pkg_required("xml2")
  table_nodes <- xml2::xml_find_all(xml2::read_xml(file_path), ".//*[name() = 'Table' or name() = 'HugeTable']")
  vapply(table_nodes, function(x) xml2::xml_text(xml2::xml_child(x, ".//*[name() = 'Title']")), character(1))
}

#' Export prism file
#'
#' Functionality from Yue Jiang's excellent package pzfx
#' @param df Data frame. Can enter multiple data frames using list("name df1" = df1, etc.)
#' @param directory Path to directory where file should be saved. Default is `desktop_path()`
#' @param row_names If `TRUE` (default), row names included in 1st column Prism table
#' @export
write_prism <- function(df, directory = desktop_path(), row_names = TRUE) {
  pkg_required("xml2")
  .write_pzfx <- function(x, path, row_names = TRUE, x_col_name = NA) {
    table_lst <- function(x_lst, row_names, x_col_name) {
      h <- function(v) lapply(as.vector(v), function(e) list(d = list(as.character(e))))
      n <- length(x_lst)
      ret <- lapply(seq_len(n), function(i) {
        this_df <- x_lst[[i]]
        cols <- lapply(seq_len(ncol(this_df)), function(z) {
          structure(list(Title = list(colnames(this_df)[z]), Subcolumn = h(this_df[, z, drop = TRUE])), Width = "89", Decimals = "0", Subcolumns = "1")
        })
        names(cols) <- rep("YColumn", ncol(this_df))
        names(cols)[x_col_name[i]] <- "XColumn"
        one_table_lst <- list(Title = list(names(x_lst)[i]))
        if (row_names[i]) {
          one_table_lst[["RowTitlesColumn"]] <- structure(list(Subcolumn = h(row.names(this_df))), Width = "39")
        }
        one_table <- structure(append(one_table_lst, cols), ID = sprintf("Table%d", i - 1), XFormat = "none", TableType = "OneWay", EVFormat = "AsteriskAfterNumber")
        if (x_col_name[i] > 0) {
          attr(one_table, "XFormat") <- "numbers"
          attr(one_table, "YFormat") <- "replicates"
          attr(one_table, "Replicates") <- "1"
          attr(one_table, "TableType") <- "XY"
        }
        one_table
      })
      names(ret) <- rep("Table", n)
      ret
    }
    if (inherits(x, c("data.frame", "matrix"))) {
      x_lst <- list(`Data 1` = x)
    } else if (inherits(x, "list")) {
      x_lst <- x
      if (is.null(names(x_lst))) {
        names(x_lst) <- paste("Data", seq_len(length(x_lst)))
      }
      are_dfs <- sapply(x_lst, function(x) inherits(x, c("data.frame", "matrix")))
      if (any(!are_dfs)) {
        stop(sprintf("The following elements are neither data frames or matrices: %s", paste(names(x_lst)[!are_dfs], collapse = ", ")), call. = FALSE)
      }
    } else {
      stop(sprintf("Cannot process 'x' of class %s", paste(class(x), collapse = ", ")), call. = FALSE)
    }
    are_nums <- vapply(x_lst, function(x) all(vapply(x, is.numeric, logical(1))), logical(1))
    if (any(!are_nums)) {
      stop(paste0("The following tables are not all numeric: ", paste(names(x_lst)[!are_nums], collapse = ", "), ". Such tables are not supported by Prism GraphPad. Consider converting the data by non-numeric columns into a wide format, where table elements are all numeric"), call. = FALSE)
    }
    if (length(row_names) == 1) {
      row_names <- rep(row_names, length(x_lst))
    }
    if (length(row_names) != length(x_lst)) {
      stop("Argument 'row_names' must have length 1 or length(x)", call. = FALSE)
    }
    if (length(x_col_name) == 1) {
      x_col_name <- rep(x_col_name, length(x_lst))
    }
    if (length(x_col_name) != length(x_lst)) {
      stop("Argument 'x_col_name' must have length 1 or length(x)", call. = FALSE)
    }
    x_col_name[is.na(x_col_name)] <- 0
    x_col_name <- as.integer(x_col_name)
    lst <- list(GraphPadPrismFile = list(Created = list(OriginalVersion = structure(list(), CreatedByProgram = "GraphPad Prism", CreatedByVersion = "6.0f.254", Login = "", DateTime = strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S+00:00"))), InfoSequence = list(Ref = structure(list(), ID = "Info0", Selected = "1")), Info = structure(list(Title = list("Project info 1"), Notes = list(""), Constant = list(Name = list("Experiment Date"), Value = list("")), Constant = list(Name = list("Experiment ID"), Value = list("")), Constant = list(Name = list("Notebook ID"), Value = list("")), Constant = list(Name = list("Project"), Value = list("")), Constant = list(Name = list("Experimenter"), Value = list("")), Constant = list(Name = list("Protocol"), Value = list(""))), ID = "Info0")))
    ret <- lapply(seq_len(n <- length(x_lst)), function(i) {
      ref <- structure(list(), ID = sprintf("Table%d", i - 1))
      if (i == 1) {
        attr(ref, "Selected") <- "1"
      }
      ref
    })
    names(ret) <- rep("Ref", n)
    lst$GraphPadPrismFile$TableSequence <- ret
    lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, table_lst(x_lst, row_names, x_col_name))
    attr(lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
    xml2::write_xml(xml2::as_xml_document(lst), path)
    invisible(x)
  }
  file_name <- if (is.data.frame(df)) as.character(substitute(df)) else "file_names"
  file_name <- .safe_name(paste0(file_name, ".pzfx"), list.files(path = directory, pattern = "\\.$pzfx", ignore.case = TRUE))
  .write_pzfx(x = df, path = paste0(gsub("/$", "", directory), file_name, sep = "/"), row_names = row_names)
}

# Internal ----------------------------------------------------------------

#' Read 1 column for prism file
#'
#' @param col Column
#' @param format Format for column
#' @param col_name Name of column
#' @noRd
.read_col <- function(col, format = "", col_name = "") {
  if (any(names(col) == "Title")) {
    col_name <- paste(unlist(col[["Title"]], use.names = FALSE), collapse = "")
  }
  subcol_lst <- list()
  n_col <- length(col)
  for (i in seq_len(n_col)) {
    if (names(col)[i] == "Subcolumn") {
      z <- col[[i]]
      n_subcol <- length(z)
      vals <- rep(NA, n_subcol)
      for (j in seq_len(n_subcol)) {
        val <- unlist(z[[j]])
        if (is.null(val)) {
          val <- NA
        }
        if (any(names(attributes(z[[j]])) == "Excluded")) {
          if (attr(z[[j]], "Excluded") == "1") {
            val <- NA
          }
        }
        vals[j] <- val
      }
      suppressWarnings(new_vals <- as.numeric(vals))
      if (all(is.na(new_vals) == is.na(vals))) {
        vals <- new_vals
      }
      subcol_lst[[length(subcol_lst) + 1]] <- vals
    }
  }
  n <- length(subcol_lst)
  col_names <- if (n == 1) {
    col_name
  } else {
    switch(format,
           error = paste0(col_name, c("_X", "_ERROR")),
           replicates = paste(col_name, seq_len(n), sep = "_"),
           SDN =  paste0(col_name, c("_MEAN", "_SD", "_N")),
           SEN = paste0(col_name, c("_MEAN", "_SEM", "_N")),
           CVN = paste0(col_name, c("_MEAN", "_CV", "_N")),
           SD = paste0(col_name, c("_MEAN", "_SD")),
           SE = paste0(col_name, c("_MEAN", "_SE")),
           CV = paste0(col_name, c("_MEAN", "_CV")),
           low_high = paste0(col_name, c("_MEAN", "_PLUSERROR", "_MINUSERROR")),
           upper_lower_limits = paste0(col_name, c("_MEAN", "_UPPERLIMIT", "_LOWERLIMIT")),
           paste("V", seq_len(n)))
  }
  names(subcol_lst) <- col_names
  max_len <- max(lengths(subcol_lst))
  long_subcol_lst <- lapply(subcol_lst, function(s) {
    length(s) <- max_len
    s
  })
  ret <- vec_to_df(long_subcol_lst)
  names(ret) <- col_names
  ret
}

#' Upload prism file
#'
#' @param path Path to file
#' @param table Table in Prism file to import. Enter quoted table name or integer indicating position of table. Default imports all tables
#' @noRd
.read_pzfx <- function(path, table = 1) {
  pkg_required("xml2")
  table_names <- prism_sheets(path)
  if (is.numeric(table)) {
    idx <- table
  } else {
    if (table %!in% table_names) stop(sprintf("Can't find %s in prism file", table), call. = FALSE)
    idx <- which(table_names == table)
    if (length(idx) > 1) {
      warning(sprintf("Multiple tables named %s, returning the first one only", table))
      idx <- idx[1]
    }
  }
  xml <- xml2::read_xml(path)
  this_table <- xml2::xml_find_all(xml, ".//*[name() = 'Table' or name() = 'HugeTable']")
  this_table <- xml2::as_list(this_table[[idx]])
  x_format <- if (any(names(attributes(this_table)) == "XFormat")) {
    gsub(pattern = "-", replacement = "_", x = attributes(this_table)$XFormat)
  } else {
    ""
  }
  y_format <- if (any(names(attributes(this_table)) == "YFormat")) {
    gsub(pattern = "-", replacement = "_", x = attributes(this_table)$YFormat)
  } else {
    ""
  }
  col_lst <- list()
  for (i in seq_along(this_table)) {
    if (names(this_table)[i] == "XColumn") {
      if (x_format == "date" &  !any(names(this_table) == "XAdvancedColumn")) {
        this_col <- .read_col(this_table[[i]], col_name = "X", format = "")
      } else if (x_format == "date") {
        next
      } else {
        this_col <- .read_col(this_table[[i]], col_name = "X", format = x_format)
      }
      if (nrow(this_col) > 0) {
        col_lst[[length(col_lst) + 1]] <- this_col
      }
    } else if (names(this_table)[i] == "XAdvancedColumn") {
      if (x_format == "date") {
        this_col <- .read_col(this_table[[i]], col_name = "X", format = "")
        if (nrow(this_col) > 0) {
          col_lst[[length(col_lst) + 1]] <- this_col
        }
      } else {
        next
      }
    } else if (names(this_table)[i] == "RowTitlesColumn") {
      this_col <- .read_col(this_table[[i]], col_name = "ROWTITLE", format = "")
      if (nrow(this_col) > 0) {
        col_lst[[length(col_lst) + 1]] <- this_col
      }
    } else if (names(this_table)[i] == "YColumn") {
      this_col <- .read_col(this_table[[i]], format = y_format)
      col_lst[[length(col_lst) + 1]] <- this_col
    }
  }
  if (length(col_lst) == 0) return(empty_df())
  max_len <- max(vapply(col_lst, nrow, integer(1)))
  Reduce("cbind", lapply(col_lst, function(z) {
    while (nrow(z) < max_len) {
      col_names <- names(z)
      z <- rbind(z, NA)
      names(z) <- col_names
    }
    z
  }))
}
