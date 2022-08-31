#' Join > 2 data frames
#'
#' @param ... Enter data frames as comma separated list
#' @param join_fn Joing function. Unless `full_join` is used, order of data frames in `...` influences the resulting data frame. Default uses `left_join.` Enter as quoted or unquoted function name
#' @param by Variable shared by data frames used to join all data frames. Must be the same in all data frames. Enter as quoted variable name
#' @returns Single data frame a result of joining all data frames
#' @importFrom purrr reduce
#' @export
join_many <- function(..., join_fn = dplyr::left_join, by = NULL) {
  dfs <- list(...)
  join_fn <- match_fun(join_fn)
  purrr::reduce(dfs, join_fn, by = by)
}
