#' Round up for 5 or higher
#'
#' Functionality from Tyler Rinker's excellent package numform
#' @param x Numeric vector
#' @param digits Number of digits after decimal place to include in output
#' @export
round_up <- function(x, digits = 2) {
  posneg <- sign(x)
  z <- abs(x)*10^digits + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  posneg*z/10^digits
}

#' Format numbers for printing
#'
#' @param x Numeric vector (can also be character or factor)
#' @param digits Number of digits to include after decimal. Default is `2`
#' @param max_width Maximum number of characters (including decimal places) allowed in output (including digits before decimal, decimal, and digits after decimal). Default is `10`
#' @param big_mark Character to separate thousands. Default is `""`
#' @returns Number as character rounded to specified number of digits after decimal
#' @export
format_number <- function(x, digits = 2, max_width = 10, big_mark = "") {
  if (inherits(x, "factor")) {
    x <- as.character(x)
  }
  .format_number <- function(.x) {
    if (is.na(.x)) return(NA_character_)
    if (!is.numeric(.x)) {
      if (can_be_numeric(.x)) {
        .x <- as.numeric(.x)
      } else if (grepl(pattern = "[a-z]", x = .x, ignore.case = TRUE)) {
        .x <- gsub(pattern = "[a-z]", replacement = "", x = .x, ignore.case = TRUE)
        if (can_be_numeric(.x)) {
          .x <- as.numeric(.x)
        } else {
          return(NA_character_)
        }
      } else {
        return(NA_character_)
      }
    }
    x_rounded <- round_up(.x, digits = digits)
    x_char <- sprintf(paste0("%.", digits, "f"), x_rounded)
    if ((nchar(x_char) > max_width) || (as.numeric(x_char) == 0 && .x != 0)) {
      .format_number_extreme(.x, digits = digits)
    } else {
      x_char
    }
  }
  vapply(x, .format_number, character(1), USE.NAMES = FALSE)
}

#' Add thousands commas to numbers
#'
#' @param x Numeric or character vector
#' @param big_mark Mark for thousands. Default is `","`
#' @param include_decimal If `FALSE` (default), number is rounded and decimal removed
#' @export
add_comma <- function(x, big_mark = ",", include_decimal = FALSE) {
  x_numeric <- is.numeric(x)
  if (!include_decimal || x_numeric) {
    if (!x_numeric) {
      x <- sub(pattern = ",", replacement = "", x = x)
      x <- as.numeric(x)
    }
    return(format(round_up(x = x, digits = 0), big.mark = big_mark, scientific = FALSE))
  }
  x_decimal_split <- strsplit(x, ".", fixed = TRUE)
  pre_decimal <- vapply(x_decimal_split, `[`, "", 1L)
  post_decimal <- vapply(x_decimal_split, `[`, "", 2L)
  if (any(idx_no_decimal <- is.na(post_decimal))) {
    post_decimal[idx_no_decimal] <- ""
  }
  post_decimal[!idx_no_decimal] <- paste0(".", post_decimal[!idx_no_decimal])
  if (nzchar(big_mark) && length(idx_big_mark <- grep("[0-9]{4,}", pre_decimal))) {
    pre_decimal[idx_big_mark] <- str_reverse(gsub("([0-9]{3})\\B", paste0("\\1", big_mark), str_reverse(pre_decimal[idx_big_mark])))
  }
  paste0(pre_decimal, post_decimal)
}

#' Format percentages
#'
#' @param x Numeric vector of proportions (i.e. 0.95, not 95)
#' @param digits Number of digits to include after decimal. Default is `1`
#' @param big_mark Character to separate thousands. Default is `""`
#' @export
format_percent <- function(x, digits = 1, big_mark = "") {
  if (length(x) == 0) return(character())
  accuracy <- 10^-digits
  z <- accuracy/100
  x <- round(x/z)*z
  nsmall <- min(max(-floor(log10(accuracy)), 0), 20)
  z <- format(x*100, big_mark = big_mark, decimal.mark = ".", trim = TRUE, nsmall = nsmall, scientific = FALSE)
  z <- paste0(z, "%")
  z[is.infinite(x)] <- as.character(x[is.infinite(x)])
  z[is.na(x)] <- NA
  names(z) <- names(x)
  z
}

#' Format n (or n/total) and %
#'
#' @param n Numerator for percentage. Enter as numeric or character
#' @param total Denominator for percentage. Enter as numeric or character
#' @param digits Number of digits after decimal. Only used for percentage. Default is `1`
#' @param incl_denominator If `TRUE` (default), format for count is n/total. If `FALSE`, format is n
#' @param perc_first If `TRUE` (default), format is % (n) or % (n/n_total). If `FALSE`, format is n (%) or n/total (%)
#' @param bracket_type Type of bracket to surround range. Default is `"("`
#' @export
format_n_perc <- function(n, total, digits = 1, incl_denominator = TRUE, perc_first = TRUE, bracket_type = "(") {
  brackets <- if (bracket_type %in% c("(", ")")) c("(", ")") else c("[", "]")
  perc <- paste0(format_number(n/total*100, digits = digits), "%")
  n <- if (incl_denominator) paste0(n, "/", total) else n
  if (perc_first) {
    part1 <- perc
    part2 <- paste0(brackets[1], n, brackets[2])
  } else {
    part1 <- n
    part2 <- paste0(brackets[1], perc, brackets[2])
  }
  paste(part1, part2)
}

#' Format number and range
#'
#' @param x Risk ratio. Enter as numeric
#' @param x_lower,x_upper Lower and upper confidence intervals of risk ratio. Enter as numeric
#' @param sep Separator character between `x_lower` and `x_upper.` Default is `"-"`
#' @param bracket_type Type of bracket to surround range. Default is `"("`
#' @param digits Number of digits to display after decimal point. Default is `2`
#' @export
format_num_range <- function(x, x_lower, x_upper, sep = "-", bracket_type = "(", digits = 2) {
  brackets <- if (bracket_type %in% c("(", ")")) c("(", ")") else c("[", "]")
  z <- format_number(c(x, x_lower, x_upper), digits = digits)
  paste(z[1], paste0(brackets[1], z[2], sep, z[3], brackets[2]))
}

#' Convert number from scientific notation to custom style scientific notation
#'
#' @param x Numeric or character containing number
#' @param digits Number of digits to include after decimal. Default is `1`
#' @param sep Separator between base and 10^exponent. Default is multiplication symbol
#' @param as_exp If `FALSE` (default), output is character. If `TRUE`, character is expression
#' @param trim_ws If `FALSE` (default), white space is allowed in output. If `TRUE`, white space is removed from output
#' @export
format_scientific <- function(x, digits = 1, sep = "\u00d7", as_exp = FALSE, trim_ws = FALSE) {
  x <- as.numeric(x)
  x_sci <- format(x, scientific = TRUE)
  if (!grepl(pattern = "e", x = x_sci, fixed = TRUE)) {
    return(x)
  }
  x_sci <- strsplit(x_sci, split = "e")[[1]]
  x_base <- sprintf(paste0("%.", digits, "f"), round_up(as.numeric(x_sci[1]), digits = digits))
  x_sci <- paste0(x_base, " ", sep, " 10^", as.integer(x_sci[2]))
  if (!as_exp) {
    if (!trim_ws) {
      x_sci
    } else {
      gsub(pattern = " ", replacement = "", x = x_sci)
    }
  } else {
    x_sci <- gsub(pattern = " ", replacement = if (!trim_ws) "~" else "*", x = x_sci)
    parse(text = x_sci)
  }
}

#' Alternative version of format_scientific
#'
#' @rdname format_scientific
#' @export
format_scientific2 <- function(x) {
  x <- as.character(as.numeric(x))
  idx <- grepl("e\\+", x)
  x[idx] <- unlist(mapply(function(y, z) {
    y_sub <- nchar(gsub("^.*\\.", "", y))
    paste0(gsub("[.]", "", y), paste(rep("0", z - y_sub), collapse = ""))
  }, gsub("e\\+.+", "", x[idx]), as.integer(gsub("^.+?e\\+", "", x[idx])), SIMPLIFY = FALSE), use.names = FALSE)
  x
}

# Helpers -----------------------------------------------------------------

#' Helper function for processing very small or very large numbers
#'
#' @param x Numeric
#' @param digits Number of digits to include after decimal
#' @returns Number formatted in scientific notation with correct number of decimal places
#' @noRd
.format_number_extreme <- function(x, digits) {
  x_sci <- strsplit(format(x, scientific = TRUE), split = "e")[[1]]
  x_base <- sprintf(paste0("%.", digits, "f"), round_up(as.numeric(x_sci[1]), digits = digits))
  paste0(x_base, "e", as.integer(x_sci[2]))
}

#' Determine number of digits before decimal
#'
#' @param x Numeric or character vector
#' @returns Number of digits before decimal
#' @noRd
.n_digits_before_decimal <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  if (x > 1) {
    floor(log10(x)) + 1
  } else if (x < -1) {
    floor(log10(abs(x))) + 1
  } else {
    1
  }
}

#' Determine number of digits after decimal
#'
#' @param x Numeric or character vector
#' @returns Number of digits after decimal
#' @noRd
.n_digits_after_decimal <- function(x) {
  x <- as.character(x)
  if (!grepl("\\.", x)) return(0)
  pieces <- unlist(strsplit(x = x, split = "\\.", perl = TRUE), use.names = FALSE)
  if (length(pieces) > 2L) stop("Input contains multiple decimals", call. = FALSE)
  nchar(pieces[2L])
}
