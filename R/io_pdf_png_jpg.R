# pdf ---------------------------------------------------------------------

#' Export plot as df (1 plot per page)
#'
#' @inheritParams ppt
#' @param plot Plot object or list of plot objects
#' @param device Plot generating function. Options: `grDevices::pdf` (default) or `grDevices::cairo_pdf`
#' @param ... Arguments passed to `device`
#' @export
pdf_plot <- function(
    plot,
    file_name = NULL,
    directory = getwd(),
    height = 5, width = 5,
    device = grDevices::pdf,
    ...) {
  args <- list(height = height, width = width, ...)
  on.exit(grDevices::dev.off(), add = TRUE)
  file_name <- file_name %||% str_alphanum(get_input(plot))
  if (startsWith(file_name, ".")) {
    file_name <- "plot"
  }
  file_name <- paste(gsub("/$", "", directory), .safe_name(str_force_ext(file_name, "pdf"), list.files(path = directory, pattern = "\\.pdf$", ignore.case = TRUE)), sep = "/")
  if (identical(device, grDevices::cairo_pdf)) {
    args$filename <- file_name
  } else {
    args$file <- file_name
  }
  do.call(device, args)

  if (inherits(plot, "ggplot")) {
    print(plot)
  } else {
    lapply(plot, print)
    invisible(plot)
  }
}

# Preview -----------------------------------------------------------------

#' Open plot in temporary file to preview output
#'
#' @inheritParams ppt
#' @param ext Extension for output file. Default is `"pptx"`
#' @param cairo If `FALSE` (default), cairo device not used. If `TRUE`, cairo device is used
#' @export
preview <- function(
  plot_object,
  height = 5, width = 5,
  y = 1.67, top = y,
  x = 1, left = x,
  font_family = "Helvetica",
  ext = "pptx",
  slide_layout = "Plot_center",
  theme = "default_helvetica",
  fullsize = FALSE,
  cairo = FALSE) {
  pkg_required("officer")
  pkg_required("rvg")
  temp_file <- tempfile(fileext = paste0(".", ext))
  ph_location_fn <- if (fullsize) officer::ph_location_fullsize else officer::ph_location_template
  coord <- .ppt_plot_coord(1)
  if (startsWith(ext, "ppt")) {
    blank <- .add_blank_slide(slide_layout = slide_layout)
    if (inherits(plot_object, "ggplot")) {
      coord <- if (missing(top) && missing(left) && missing(x) && missing(y)) {
        .ppt_plot_coord(1)
      } else {
        list(x1 = left, y1 = top)
      }
      officer::ph_with(blank, rvg::dml(ggobj = plot_object), location = ph_location_fn(top = coord$y1, left = coord$x1, height = height, width = width, type = "body"), fonts = list(sans = font_family), editable = TRUE, bg = NULL) |> print(target = temp_file)
    } else {
      blank <- officer::ph_with(blank, rvg::dml(ggobj = plot_object[[1L]]), location = ph_location_fn(top = coord$y1, left = coord$x1, height = height, width = width, type = "body"), fonts = list(sans = font_family), editable = TRUE, bg = NULL)
      slides <- function(j) {
        blank <- officer::add_slide(blank, layout = slide_layout, master = theme) |>
          officer::ph_with(rvg::dml(ggobj = plot_object[[j]]), location = ph_location_fn(top = coord$y1, left = coord$x1, height = height, width = width, type = "body"), fonts = list(sans = font_family), editable = TRUE, bg = NULL)
      }
      n_plots <- length(plot_object)
      lapply(2:n_plots, slides)
      print(blank, target = temp_file)
    }
  } else if (cairo & ext == "pdf") {
    ggplot2::ggsave(filename = temp_file, device = grDevices::cairo_pdf, plot = plot_object, width = width, height = height)
  } else if (cairo & ext == "png") {
    ggplot2::ggsave(filename = temp_file, device = ext, type = "cairo", plot = plot_object, width = width, height = height)
  } else {
    ggplot2::ggsave(filename = temp_file, device = ext, plot = plot_object, width = width, height = height)
  }
  system2("open", temp_file)
  invisible(NULL)
}

# Other -------------------------------------------------------------------

#' Merge multiple pdf files
#'
#' @param ... Complete paths of pdf files to merge. Enter as comma separated list of quoted file paths
#' @param new_file_name File name for new pdf. Enter as length 1 character vector
#' @param directory Directory where new file should be saved. Default uses directory of 1st file entered into `...`
#' @returns New pdf file silently generated
#' @export
merge_pdf <- function(..., new_file_name = "combined", directory = NULL) {
  pkg_required("qpdf")
  paths <- c(...)
  qpdf::pdf_combine(input = paths, output = .safe_file_path(new_file_name, "pdf", directory %||% dirname(paths[1L])))
}

#' Convert colors in file to greyscale
#'
#' @param file Path to file. Enter full path to file
#' @param file_name Name for new file. If `NULL` (default), `"_grey"` added to input file name
#' @param directory Path to save new file. If `NULL` (default), file saved to same directory as `file`
#' @param ext File extension for output. If `NULL` (default), input file extension is used
#' @noRd
write_greyscale <- function(file, file_name = NULL, directory = NULL, ext = NULL) {
  pkg_required("magick")
  if (!file.exists(file)) {
    z <- paste0(desktop_path(), file)
    if (!file.exists(z)) {
      stop(sprintf("File '%s' does not exist", file), call. = FALSE)
    } else {
      file <- z
    }
  }
  directory <- directory %||% dirname(file)
  old_file_name <- basename(file)
  file_name <- file_name %||% paste0(str_remove_ext(old_file_name), "_grey")
  file_name <- .safe_file_path(file_name, ext %||% str_file_ext(old_file_name), directory)
  x <- magick::image_read(file)
  x <- magick::image_convert(x, colorspace = "gray")
  magick::image_write(x, file_name)
}
