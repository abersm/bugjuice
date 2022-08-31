#' Extract day of month from date
#'
#' @param x Vector of dates
#' @returns Integer vector containing day, month, or year (4 digit year). Length equal to input
#' @export
day <- function(x) as.integer(format(if (!inherits(x, "Date")) as_date(x) else x, "%d"))

#' Extract month from date
#'
#' @rdname day
#' @export
month <- function(x) as.integer(format(if (!inherits(x, "Date")) as_date(x) else x, "%m"))

#' Extract year from date
#'
#' @rdname day
#' @export
year <- function(x) as.integer(format(if (!inherits(x, "Date")) as_date(x) else x, "%Y"))

#' Time interval between 2 dates
#'
#' @param t0,t1 Dates for beginning and end of time period, respectively. Can be character or date vector
#' @param units Units for time interval. Default is `"days"`
#' @param date_fn Function used to convert `t0` and `t1` to dates. Default is `as.Date.` Other options: `lubridate::ymd`
#' @returns Numeric vector with units of time determined by `units`
#' @export
time_interval <- function(t0, t1, units = "days", date_fn = function(x) as.Date(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) {
  as.numeric(difftime(date_fn(t1), date_fn(t0), units = units))
}

#' Convert between units of time
#'
#' @param x Numeric vector
#' @param from,to Initial and desired units of time, respectively. Options for both: `"msec"`, `"sec"`, `"min"`, `"hr"`, `"day"`, `"week"`, `"month"` (30 days), `"year"`, `"decade"`, `"century"`, `"millennium"`
#' @returns Numeric vector with length equal to input
convert_time <- function(x, from, to) {
  # Units
  from <- tolower(from)
  to <- tolower(to)
  three_characters <- startsWith(from, "m") || startsWith(from, "d")
  from <- substr(from, 1, if (three_characters) 3 else 1)
  three_characters <- startsWith(to, "m") || startsWith(to, "d")
  to <- substr(to, 1, if (three_characters) 3 else 1)

  # Unit definition (in days)
  unit_def <- c(
    mse = 1/86400000,
    s = 1/86400,
    min = 1/1440,
    h = 1/24,
    day = 1,
    d = 1,
    w = 7,
    mo = 1/30,
    mon = 1/30,
    y = 1/365,
    dec = 1/3650,
    c = 1/36500,
    mil = 1/365000)
  units <- names(unit_def)
  if (from %!in% units) {
    stop("In convert_time(), 'from' argument should be one of the following: ", paste(shQuote(c("msec", "sec", "min", "hr", "day", "week", "month", "year", "decade", "century", "millennium")), collapse = ", "), call. = FALSE)
  }
  if (to %!in% units) {
    stop("In convert_time(), 'to' argument should be one of the following: ", paste(shQuote(c("msec", "sec", "min", "hr", "day", "week", "month", "year", "decade", "century", "millennium")), collapse = ", "), call. = FALSE)
  }

  # Conversion
  x <- x/unit_def[from]*unit_def[to]
  names(x) <- NULL
  x
}

#' Determine whether the class of a vector is POSIXct
#'
#' @param x Vector
#' @returns Logical
#' @export
is_POSIXct <- function(x) inherits(x, "POSIXct")

#' Check whether vector contains dates
#'
#' @rdname is_POSIXct
#' @export
is_date <- function(x) inherits(x, c("Date", "POSIXct"))

#' Transform input to YMD format
#'
#' @param x Vector containing year, month, day
#' @returns Date in YYYY-MM-DD format
#' @export
as_date <- function(x) {
  if (is_date(x)) return(as.Date(x))
  if (is.numeric(x)) return(as.Date.numeric(floor(as.numeric(x)), origin = "1899-12-30"))
  x <- trimws(x)
  x <- strsplit(x, "\\/|-")
  x <- vapply(x, function(y) {
    if (length(y) != 3) return(NA_character_)
    n <- nchar(y)
    y[n == 1] <- paste0("0", y[n == 1])
    yr <- n == 4
    if (any(yr)) {
      paste(y[yr], paste(y[n != 4], collapse = "-"), sep = "-")
    } else {
      y[3] <- paste0(if (as.numeric(y[3]) > 30) "19" else "20", y[3])
      paste(y[3], paste(y[1:2], collapse = "-"), sep = "-")
    }
  }, character(1))
  as.Date(x)
}
