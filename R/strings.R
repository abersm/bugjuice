#' Substitute multiple characters over multiple patterns
#'
#' @param x Character vector
#' @param pattern Character vector containing patterns to be replaced
#' @param replacement Character vector containing replacement patterns. Must have length of 1 or `length(pattern)`
#' @param ... Arguments passed to `gsub`
#' @returns Character vector with length same as input
#' @export
mgsub <- function(x, pattern, replacement, ...) {
  if (length(replacement) == 1L) {
    replacement <- rep(replacement, length(pattern))
  }
  for (i in seq_along(pattern)) {
    x <- gsub(pattern = pattern[i], replacement = replacement[i], x = x, ...)
  }
  x
}

#' Substitute multiple characters over multiple patterns (1st matching pattern only)
#'
#' @rdname mgsub
#' @export
msub <- function(x, pattern, replacement, ...) {
  if (length(replacement) == 1L) {
    replacement <- rep(replacement, length(pattern))
  }
  for (i in seq_along(pattern)) {
    x <- msub(pattern = pattern[i], replacement = replacement[i], x = x, ...)
  }
  x
}

#' Replace or remove numbers in string
#'
#' @param x String
#' @param replacement Replacement for numbers in `x`. Enter as numeric or string. Default removes all numbers from `x`
#' @returns New string containing replacement wherever numbers occur in `x`
#' @export
str_replace_numerics <- function(x, replacement = "") gsub("[0-9.]", replacement, as.character(x))

#' Replace text inside parentheses
#'
#' @param x String
#' @param replacement Text to be inserted between parenthesis
#' @param sub_fn Function used to substitute strings. Default is `sub`
#' @param ... Arguments passed to `sub_fn`
#' @export
str_replace_between_parens <- function(x, replacement = "", sub_fn = sub, ...) {
  sub_fn("\\(.*\\)", sprintf("(%s)", replacement), x = x, ...)
}

#' List all characters in a string
#'
#' @param x Character vector
#' @param simplify If `FALSE` (default), output will be list of length equal to length of input. Each component of list will include all characters in each string. If `TRUE` and the length of `x` is > 1, then only the first letter of each component of `x` will be returned as a character vector
#' @export
str_explode <- function(x, simplify = FALSE) {
  # alternative: rawToChar(charToRaw(x), multiple = TRUE)
  x <- strsplit(x, NULL)
  if (simplify) x[[1L]] else x
}

#' Wrap strings with prefix and/or suffix
#'
#' @param x Character vector
#' @param prefix,suffix String to attach to beginning (`prefix`) or end (`suffix`) of each value in `x`. Each must be length 1
#' @returns Character vector with length same as input
#' @export
str_wrap <- function(x, prefix = "", suffix = "") paste0(prefix, x, suffix)

# Info --------------------------------------------------------------------

#' Detect pattern in string
#'
#' @param x Character vector
#' @param pattern Substring to detect in `x.` Enter as character vector
#' @param invert If `TRUE`, search will be performed for values in `x` that do not contain `pattern`. Equivalent of `!str_contains(x)`
#' @param ignore_case If `FALSE` (default), case of both `x` and `pattern` are ignored in search. If `TRUE`, case used in `x` and `pattern` must match
#' @param ... Arguments passed to `grepl`
#' @export
str_contains <- function(x, pattern, invert = FALSE, ignore_case = FALSE, ...) {
  if (invert) {
    !grepl(pattern = pattern, x = x, ignore.case = ignore_case, ...)
  } else {
    grepl(pattern = pattern, x = x, ignore.case = ignore_case, ...)
  }
}

#' Detect multiple patterns in string
#'
#' @rdname str_contains
#' @export
str_contains_all <- function(x, pattern, ignore_case = FALSE, ...) {
  Reduce(`+`, lapply(pattern, grepl, x = x, ignore.case = ignore_case, ...)) == length(pattern)
}

#' Detect 1 or multiple patterns in string
#'
#' @rdname str_contains
#' @export
str_contains_any <- function(x, pattern, ignore_case = FALSE, ...) {
  grepl(pattern = paste(pattern, collapse = "|"), x = x, ignore.case = ignore_case, ...)
}

#' Detect which strings contain none of several patterns
#'
#' Equivalent of `!str_contains_any(x, pattern)`
#' @rdname str_contains
#' @export
str_contains_none <- function(x, pattern, ignore_case = FALSE, ...) {
  !grepl(pattern = paste(pattern, collapse = "|"), x = x, ignore.case = ignore_case, ...)
}

#' Is string coercible to numeric
#'
#' @param x Vector
#' @export
can_be_numeric <- function(x) {
  if (is.logical(x)) return(FALSE)
  is.na(x) | !is.na(suppressWarnings(as.numeric(x)))
}

#' For each element of character vector, determine whether string is empty
#'
#' @rdname str_contains
#' @returns Logical vector with length same as input. `NA` values return `FALSE`
#' @export
str_empty <- function(x) !nzchar(x)

#' Count occurrence of substring
#'
#' @param x Character vector
#' @param pattern Pattern to count per element of `x`
#' @param ignore_case If `FALSE` (default), case of both `x` and `pattern` are ignored in search. If `TRUE`, case used in `x` and `pattern` must match
#' @param ... Arguments passed to `gregexpr`
#' @returns Integer
#' @export
str_count <- function(x, pattern, ignore_case = FALSE, ...) {
  vapply(gregexpr(pattern, x, ignore.case = ignore_case, ...), function(y) sum(y > 0, na.rm = TRUE), integer(1))
}

#' Wrapper for grepl, grep, agrep
#'
#' @param x Character vector
#' @param pattern Substring to detect in `x.` Enter as character vector
#' @param invert If `TRUE`, search will be performed for values in `x` that do not contain `pattern`. Equivalent of `!str_contains(x)`
#' @param output Options: `"logical"` (default), `"value"`, `"index"`, `"all"` (list of length 3 with components "logical", "value", "index")
#' @param ignore_case If `FALSE` (default), case of both `x` and `pattern` are ignored in search. If `TRUE`, case used in `x` and `pattern` must match
#' @param value If `TRUE`, value of `x`
#' @param fixed If `TRUE`, exact search is performed for `pattern` in `x`
#' @param perl If `TRUE`, perl-style regular expressions can be entered as `pattern`
#' @param fuzzy If `TRUE`, fuzzy matching is performed
#' @param use_bytes If `TRUE`, search is performed byte-by-byte. If `FALSE` (default), search is performed character-by-character
#' @returns Output is a vector with length equal to input and class determined by `output` (same class as input if `output = "value"`, logical if `output = "logical"`, integer vector of indices in `x` if `output = "index"`) of output determined by `output` argument. Length identical to input
#' @export
str_info <- function(x, pattern, output = c("logical", "value", "index", "all"), invert = FALSE, ignore_case = FALSE, fixed = FALSE, perl = FALSE, use_bytes = FALSE, fuzzy = FALSE) {
  output <- match.arg(output, choices = c("logical", "value", "index", "all"))
  args <- list(pattern = pattern, x = x, ignore.case = ignore_case, fixed = fixed, useBytes = use_bytes)
  if (fuzzy) {
    fn <- agrepl
  } else {
    fn <- grepl
    args$perl <- perl
  }
  out <- do.call(fn, args)
  out <- if (invert) !out else out
  switch(output,
         logical = out,
         value = x[out],
         index = which(out),
         all = ,
         list(logical = out, value = x[out], index = which(out)))
}

#' Identify general pattern in strings
#'
#' Functionality from Brandon Greenwell's excellent package bpa
#' @param x Character vector or data frame
#' @param shorten_letters,shorten_numbers If `TRUE` (default), sequences of letter cases or numbers are shortened to a single symbol
#' @param summary If `TRUE` (default), number of occurrences of each pattern in `x` is included in output
#' @returns Patterns identified in `x`. "A" represents upper case letters, "a" represents lower case letters, "#" represents numeric digit 0-9
#' @export
str_pattern <- function(x, shorten_letters = TRUE, shorten_numbers = TRUE, summary = TRUE) {
  x <- as.character(x)
  x <- gsub(paste0("[A-Z]", if (shorten_letters) "+"), "A", x)
  x <- gsub(paste0("[a-z]", if (shorten_letters) "+"), "a", x)
  x <- gsub(paste0("[0-9]", if (shorten_numbers) "+"), "#", x)
  if (summary) {
    x <- x[!is.na(x)]
    x_unique <- unique.default(x)
    out <- tabulate(match(x, x_unique))
    names(out) <- x_unique
    sort.int(out, method = "quick", decreasing = TRUE)
  } else {
    x
  }
}

# Position ----------------------------------------------------------------

#' Determine the position of a pattern within a string
#'
#' Functionality from Hadley Wickham's excellent package stringb
#' @param x String
#' @param pattern Pattern to identify in string
#' @param fixed If `FALSE` (default), pattern is not used as is
#' @param ignore_case If `FALSE` (default), case of both `x` and `pattern` are ignored in search. If `TRUE`, case used in `x` and `pattern` must match
#' @param ... Arguments passed to `gregexpr`
#' @returns List of matrices with columns for start and end position of pattern
#' @export
str_position <- function(x, pattern, fixed = FALSE, ignore_case = FALSE, ...) {
  idx <- gregexpr(pattern = pattern, text = x, fixed = fixed, ignore.case = ignore_case, ...)
  lapply(idx, function(z) {
    start <- as.vector(z)
    if (identical(start, -1L)) {
      return(cbind(start = integer(), end = integer()))
    }
    end <- as.vector(z) + attr(z, "match.length") - 1
    no_match <- start == -1L
    start[no_match] <- NA
    end[no_match] <- NA
    cbind(start = start, end = end)
  })
}

# Remove ------------------------------------------------------------------

#' Remove a pattern from a string
#'
#' @param x String
#' @param pattern Pattern to remove from string. Enter as quoted regular expression or exact text
#' @param all If `TRUE` (default), all matches for `pattern` will be removed. If `FALSE`, only the first match for `pattern` will be removed
#' @param ... Arguments passed to `sub` or `gsub`
#' @export
str_remove <- function(x, pattern, all = TRUE, ...) {
  if (all) {
    gsub(pattern = pattern, replacement = "", x = x, ...)
  } else {
    sub(pattern = pattern, replacement = "", x = x, ...)
  }
}

#' Remove numbers from a string
#'
#' @rdname str_remove
#' @export
str_remove_numerics <- function(x, all = TRUE) str_remove(x, "[0-9.]", all = all)

#' Remove letters from string
#'
#' @param x String
#' @param case Case of letters to be removed. Options: `"both"` (default), `"lower"`, `"upper"`
#' @param all If `TRUE` (default), all matches for `pattern` will be removed. If `FALSE`, only the first match for `pattern` will be removed
#' @export
str_remove_letters <- function(x, case = "both", all = TRUE) {
  pattern <- switch(case, lower = "[a-z]", upper = "[A-Z]", all = , "[[:alpha:]]")
  str_remove(x, pattern = pattern, all = all)
}

#' Remove all non-number/non-text characters from string
#'
#' @rdname str_remove
#' @export
str_alphanum <- function(x) gsub("[^[:alnum:][:space:]\"]", "", x)

#' Alias for str_alphanum
#'
#' @rdname str_remove
#' @export
str_remove_punc <- str_alphanum

# Extract -----------------------------------------------------------------

#' Extract text following a particular pattern (1st occurrence only)
#'
#' @param x String
#' @param pattern Pattern to search for in `x`
#' @param all If `TRUE` (default), all matches for `pattern` will be extracted. If `FALSE`, only the first match for `pattern` will be extracted
#' @param fixed If `TRUE`, `pattern` is matched "as is"
#' @returns Substring
#' @export
str_extract <- function(x, pattern, all = TRUE, fixed = FALSE) {
  if (all) {
    regmatches(x, gregexpr(pattern, x, fixed = fixed, perl = FALSE))
  } else {
    regmatches(x, regexpr(pattern, x, fixed = fixed, perl = FALSE))
  }
}

#' Extract text before a pattern
#'
#' @param x String
#' @param pattern Pattern in `x`. All text before (and not including) this pattern will be extracted
#' @param all If `FALSE` (default), only the first match for `pattern` will be removed. If `TRUE`, all matches for `pattern` will be removed
#' @returns Text in `x` before `pattern`
#' @export
str_extract_before <- function(x, pattern, all = FALSE) {
  str_remove(x, pattern = paste0(pattern, ".*"), all = all)
}

#' Alias for str_extract_before
#'
#' @rdname str_extract_before
#' @export
str_remove_after <- str_extract_before

#' Extract text after a pattern
#'
#' @param x String
#' @param pattern Pattern in `x`. All text after (but not including) this pattern will be extracted
#' @param all If `FALSE` (default), only the first match for `pattern` will be removed. If `TRUE`, all matches for `pattern` will be removed
#' @returns Text in `x` after `pattern`
#' @export
str_extract_after <- function(x, pattern, all = FALSE) {
  str_remove(x, pattern = paste0(pattern, "*."), all = all)
}

#' Alias for str_extract_after
#'
#' @rdname str_extract_after
#' @export
str_remove_before <- str_extract_after

#' Extract first n characters from string
#'
#' @param x String
#' @param n Number of characters to extract
#' @export
str_first_n <- function(x, n) substr(as.character(x), 1, n)

#' Extract last n characters from string
#'
#' @rdname str_first_n
#' @export
str_last_n <- function(x, n) {
  x <- as.character(x)
  n_char <- nchar(x)
  substr(x, n_char - n + 1, n_char)
}

#' Extract all numbers from string
#'
#' @param x String
#' @param single_number If `FALSE` (default), output is a list of numbers. If `TRUE`, output is concatenated numbers that appear throughout string
#' @param incl_decimal If `TRUE` (default), decimals can be included in numbers. If `FALSE`, values will be split at decimals. Only relevant if `single_number` is `FALSE`
#' @param incl_comma If `FALSE` (default), values will be split at commas. If `TRUE`, commas can be included in numbers. Only relevant if `single_number` is `FALSE`
#' @param incl_neg If `FALSE` (default), negative sign not included in output. Only relevant if single_number is `FALSE`
#' @param as_numeric If `FALSE` (default), output is a character. If `TRUE`, output is numeric
#' @returns 1 or more numbers. If `single_number` is `FALSE`, output is a list of numbers (as a character if `as_numeric` is `FALSE`)
#' @export
str_numerics <- function(x, single_number = FALSE, incl_decimal = TRUE, incl_comma = FALSE, incl_neg = FALSE, as_numeric = FALSE) {
  if (single_number) {
    x <- gsub("[^0-9.-]+", "", as.character(x))
    if (as_numeric) as.numeric(x) else x
  } else {
    decimal <- if (incl_decimal) "."
    neg <- if (incl_neg) "-"
    comma <- if (!as_numeric && incl_comma) ","
    split_pattern <- paste0("[^", comma, "0-9", decimal, neg, "]+")
    x <- strsplit(x = x, split = split_pattern)
    lapply(x, function(y) {
      y <- y[y != ""]
      y <- sub(pattern = "\\.$", replacement = "", x = y)
      y <- unlist(lapply(y, function(z) {
        if (!grepl(pattern = ",", x = z, fixed = TRUE)) {
          z
        } else {
          z_split <- strsplit(z, ",")[[1L]]
          z_digits <- nchar(strsplit(z_split[-1], "\\.")[[1L]][1L])
          z <- if (all(z_digits == 3)) z else z_split
          z[z != ","]
        }
      }), use.names = FALSE)
      if (as_numeric) as.numeric(y) else y
    })
  }
}

#' Extract nth number from string
#'
#' @param n nth number in `x`. Can be numeric, `"first"` (`n = 1`), or `"last"` (`n = -1`). For second to last occurrence enter `n = -2`
#' @inheritParams str_numerics
#' @export
str_nth_number <- function(x, n = 1, incl_decimal = TRUE, incl_comma = FALSE, incl_neg = FALSE, as_numeric = FALSE) {
  n <- if (n == "first") 1L else if (n == "last") -1L else n
  num_list <- str_numerics(x = x, single_number = FALSE, incl_decimal = incl_decimal, incl_comma = incl_comma, incl_neg = incl_neg, as_numeric = as_numeric)
  num_list <- remove_null(num_list)
  if (as_numeric) {
    output_type <- numeric(1)
  } else {
    output_type <- character(1)
  }
  if (length(num_list) == 0L) return(output_type)
  vapply(num_list, function(y) {
    if (n < 0) {
      y <- Rev(y)
      n <- abs(n)
    }
    y[n]
  }, output_type, USE.NAMES = FALSE)
}

#' Extract text that occurs between two specified patterns
#'
#' @param x Character vector
#' @param before,after Pattern before and after text to be extracted. Enter as length 1 character vectors
#' @returns Character vector with length equal to input. If no match is found, `""` is returned. If multiple matches present, only the first is returned
#' @export
str_extract_between <- function(x, before, after) {
  x_names <- names(x)
  pattern <- paste0("(?<=", after, ").*(?=", before, ")")
  # alternative 1: pattern <- sprintf("(?<=%s).*?(?=%s)", before, after)
  # alternative 2: pattern <- sprintf("(%s)(.*?)(%s)", before, after)
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1))
  names(x) <- x_names
  x
}

#' Extract text that occurs before a specified pattern
#'
#' @param x Character vector
#' @param pattern Pattern that follows text to be extracted. Enter as length 1 character vector
#' @returns Character vector with length equal to input. If no match is found, `""` is returned. If multiple matches present, only the first is returned
#' @export
str_extract_before <- function(x, pattern) {
  x_names <- names(x)
  pattern <- paste0(".*(?=", pattern, ")")
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1))
  names(x) <- x_names
  x
}

#' Extract text that follows a specified pattern
#'
#' If a string has more than one match, this will only return the first one
#' @param x Character vector
#' @param pattern Pattern before text to be extracted. Enter as length 1 character vector
#' @returns Character vector with length equal to input. If no match is found, `""` is returned. If multiple matches present, only the first is returned
#' @export
str_extract_after <- function(x, pattern) {
  x_names <- names(x)
  pattern <- paste0("(?<=", pattern, ").*")
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1))
  names(x) <- x_names
  x
}

#' Extract dates
#'
#' @param x Character vector
#' @param sep Separator between month, day, year. Default is `"/"`
#' @param all If `TRUE`, all dates are extracted. If `FALSE` (default), only first date is extracted
#' @returns Date as character vector or list of character vectors
#' @export
str_date <- function(x, sep = "/", all = TRUE) {
  pattern <- paste("[0-9]*", "[0-9]*", "[0-9]*", sep = sep)
  if (all) {
    lapply(x, function(z) {
      z <- unlist(str_extract(x = z, pattern = pattern, all = TRUE), use.names = FALSE)
      if (length(z) == 0) return(NA_character_) else z
    })
  } else {
    vapply(x, function(z) {
      z <- str_extract(x = z, pattern = pattern, all = FALSE)
      if (length(z) == 0) return(NA_character_) else z
    }, character(1), USE.NAMES = FALSE)
  }
}

#' Identify the longest prefix shared by a character vector
#'
#' @param x Character vector
#' @param ignore_case If `FALSE` (default), case of both `x` and `pattern` are ignored in search. If `TRUE`, case used in `x` and `pattern` must match
#' @returns Character vector of length 1 (when shared prefix exists) or 0 (no shared prefix)
#' @export
str_common_prefix <- function(x, ignore_case = FALSE) {
  if (length(x) == 0L) return(character(0L))
  x <- unique.default(x)
  x <- x[!is.na(x) & x != ""]
  if (ignore_case) {
    x <- tolower(x)
  }
  if (length(unique.default(substr(x, 1L, 1L))) != 1L) return(character(0L))
  n <- nchar(x)
  idx_min <- which.min(n)
  n_min <- n[idx_min]
  x_min <- x[idx_min]
  x <- x[-idx_min]
  for (i in rev(seq_len(n_min))) {
    prefix <- substr(x_min, 1L, i)
    if (all(startsWith(x, prefix))) return(prefix)
  }
}

#' Identify the longest suffix shared by a character vector
#'
#' @rdname str_common_prefix
#' @return Character vector of length 1 (when shared suffix exists) or 0 (no shared suffix)
#' @export
str_common_suffix <- function(x, ignore_case = FALSE) {
  if (length(x) == 0L) return(character(0L))
  x <- unique.default(x)
  x <- x[!is.na(x) & x != ""]
  if (ignore_case) {
    x <- tolower(x)
  }
  if (length(unique.default(substr(x, 1L, 1L))) != 1L) return(character(0L))
  n <- nchar(x)
  idx_min <- which.min(n)
  n_min <- n[idx_min]
  x_min <- x[idx_min]
  x <- x[-idx_min]
  for (i in seq_len(n_min)) {
    suffix <- substr(x_min, i, n_min)
    if (all(endsWith(x, suffix))) return(suffix)
  }
}

#' Remove prefix/suffix shared by a character vector from each strings
#'
#' @rdname str_common_prefix
#' @return Character vector with same length as input
#' @export
str_remove_common <- function(x, ignore_case = FALSE) {
  if (length(x) == 0L) return(character(0L))
  x_original <- x
  x <- unique.default(x)
  x <- x[!is.na(x) & x != ""]
  if (ignore_case) {
    x <- tolower(x)
  }
  n <- nchar(x)
  analyze_prefix <- length(unique.default(substr(x, 1L, 1L))) == 1L
  analyze_suffix <- length(unique.default(substr(x, n, n))) == 1L
  if (!analyze_prefix && !analyze_suffix) return(x_original)
  idx_min <- which.min(n)
  n_min <- n[idx_min]
  x_min <- x[idx_min]
  x_other <- x[-idx_min]
  idx_char <- seq_len(n_min)
  prefix <- if (analyze_prefix) {
    extract_prefix <- function(idx, y, z) {
      for (i in rev(idx)) {
        prefix <- substr(y, 1L, i)
        if (all(startsWith(z, prefix))) return(prefix)
      }
    }
    extract_prefix(idx = idx_char, y = x_min, z = x_other)
  } else {
    ""
  }
  suffix <- if (analyze_suffix) {
    extract_suffix <- function(idx, y, z, j) {
      for (i in idx_char) {
        suffix <- substr(y, i, j)
        if (all(endsWith(z, suffix))) return(suffix)
      }
    }
    extract_suffix(idx = idx_char, y = x_min, z = x_other, j = n_min)
  } else {
    ""
  }
  common_substrings <- sprintf("^%s|%s$", prefix, suffix)
  gsub(common_substrings, "", x_original, ignore.case = ignore_case)
}

#' Capitalize each word
#'
#' @param x Character vector
#' @param other_letters_lower If `FALSE` (default), subsequent letters left as is. If `TRUE`, subsequent letters converted to lower case
#' @returns String with first letter capitalized (remaining letters not capitalized)
#' @export
str_capitalize <- function(x, other_letters_lower = FALSE) {
  if (other_letters_lower) {
    x <- tolower(x)
  }
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}

#' Convert from CamelCase to snake_case
#'
#' @param x Character vector
#' @param lower If `TRUE` (default), output is converted to lower case
#' @returns Character vector with length same as input
#' @export
str_snake_case <- function(x, lower = TRUE) {
  x <- gsub("[^a-zA-Z0-9]+", "_", x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("([a-zA-Z])([0-9])", "\\1_\\2", x)
  if (lower) tolower(x) else x
}

#' Convert snake_case to CamelCase
#'
#' @rdname str_snake_case
#' @export
str_camel_case <- function(x) gsub("_(.)", "\\U\\1", x, perl = TRUE)

#' Split CamelCase to Camel Case
#'
#' @rdname str_snake_case
#' @export
str_split_camel <- function(x) {
  x <- gsub("(?!^)(?=[[:upper:]])", " ", gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", x), perl = TRUE)
  unlist(strsplit(x, " "), use.names = FALSE)
}

#' Sort vector or columns in a data frame alphabetically by names of elements
#'
#' @param x Named vector, named list, or data frame
#' @param blank_last If `TRUE` (default), elements without names will be placed at the end
#' @param reverse If `TRUE`, names ordered reverse alphabetically
#' @export
sort_by_name <- function(x, blank_last = TRUE, reverse = FALSE) {
  if (length(x) == 0) return(x)
  x_names <- names(x)
  x_names[x_names == ""] <- NA
  x[order(x_names, na.last = blank_last, decreasing = reverse)]
}

#' Sort strings by number of characters
#'
#' @param x Vector
#' @param desc If `FALSE` (default), `x` is order from shortest to longest string. If `TRUE`, `x` is order from longest to shortest string
#' @export
sort_by_length <- function(x, desc = FALSE) x[order(nchar(x), decreasing = desc, method = "quick")]

#' Reverse the order of letters in a string
#'
#' @rdname str_explode
#' @export
str_reverse <- function(x) vapply(lapply(strsplit(x, NULL), rev), paste, "", collapse = "")

#' Remove all spaces/whitespace (not just ends of string)
#'
#' @rdname str_remove
#' @export
str_remove_space <- function(x) gsub(" ", "", x)
