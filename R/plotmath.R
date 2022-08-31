#' Subscript notation
#'
#' @param base,subscript Text for base and subscript respectively. Enter as quoted or unquoted text
#' @param font_face_base,font_face_subscript Font face for base and subscript text respectively. Options: `"plain"` (default), `"italic"`, `"bold"`, `"bolditalic"`. Enter as quoted type
#' @returns Expression that can be entered as ggplot2 title argument
#' @export
subscript <- function(base, subscript, font_face_base = c("plain", "italic", "bold", "bolditalic"), font_face_subscript = c("plain", "italic", "bold", "bolditalic")) {
  ff_x <- match.arg(font_face_base, choices = c("plain", "italic", "bold", "bolditalic"))
  ff_y <- match.arg(font_face_subscript, choices = c("plain", "italic", "bold", "bolditalic"))
  b <- get_input(base)
  ss <- get_input(subscript)
  .build_plotmath_expression(x = b, y = ss, type = "subscript", font_face_x = ff_x, font_face_y = ff_y)
}

#' Superscript notation
#'
#' @param base,superscript Text for base and superscript respectively. Enter as quoted or unquoted text
#' @param font_face_base,font_face_superscript Font face for base and superscript text respectively. Options: `"plain"` (default), `"italic"`, `"bold"`, `"bolditalic"`. Enter as quoted type
#' @returns Expression that can be entered as ggplot2 title argument
#' @export
superscript <- function(base, superscript, font_face_base = c("plain", "italic", "bold", "bolditalic"), font_face_superscript = c("plain", "italic", "bold", "bolditalic")) {
  ff_x <- match.arg(font_face_base, choices = c("plain", "italic", "bold", "bolditalic"))
  ff_y <- match.arg(font_face_superscript, choices = c("plain", "italic", "bold", "bolditalic"))
  b <- get_input(base)
  ss <- get_input(superscript)
  .build_plotmath_expression(x = b, y = ss, type = "superscript", font_face_x = ff_x, font_face_y = ff_y)
}

#' Create genotype labels using plotmath notation
#'
#' @param genes,superscripts Genes and superscripts respectively. Enter as character vector
#' @param font_face_genes,font_face_superscripts Fontface for genes and superscripts respectively. Options: `"italic" `(default), `"plain"`, `"bold"`, `"bolditalic"`. Enter as quoted type
#' @returns Expression that can be used for ggplot2
#' @export
label_genotype <- function(genes, superscripts, font_face_genes = "italic", font_face_superscripts = "italic") {
  n_genes <- length(genes)
  n_superscripts <- length(superscripts)
  if (n_genes != n_superscripts) {
    stop("In label_genotype(), length of 'genes' must match length of 'superscripts'", call. = FALSE)
  }
  ff_x <- rep(font_face_genes, length.out = n_genes)
  ff_y <- rep(font_face_superscripts, length.out = n_superscripts)
  x <- paste0(".(genes[[", seq_along(genes), "]])")
  y <- paste0(".(superscripts[[", seq_along(superscripts), "]])")
  genes <- paste0(ff_x, "('", genes, "')")
  superscripts <- paste0(ff_y, "('", superscripts, "')")
  genes <- lapply(genes, str2lang)
  superscripts <- lapply(superscripts, str2lang)
  z <- paste(x, y, sep = "^", collapse = "~")
  as.expression(eval(str2expression(sprintf("bquote(%s)", z))))
}

# Helpers -----------------------------------------------------------------

#' Create subscript or superscript plotmath expression
#'
#' @param x,y Base and either subscript or superscript text respectively. Enter as numeric or character string
#' @param type Options: `"subscript" `(default) or `"superscript"`
#' @param font_face_x,font_face_y Fontface for base and either subscript or superscript text respectively. Options: `"plain"` (default), `"italic"`, `"bold"`, `"bolditalic"`. Enter as quoted type
#' @noRd
.build_plotmath_expression <- function(x, y, type, font_face_x = "plain", font_face_y = "plain") {
  x <- sprintf("%s(%s)", font_face_x, x)
  y <- sprintf("%s(%s)", font_face_y, y)
  y <- switch(type, superscript = sprintf("^%s", y), subscript = sprintf("[%s]", y))
  str2expression(paste0(x, y))
}
