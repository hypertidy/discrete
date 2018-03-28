#' Discrete
#'
#' table of axes
#' @param x object to derive properties from (all other named arguments are ignored)
#' @param ... ignored currently
#' @param mins minimum coordinates
#' @param maxs maximum coordinates
#' @param dims axis length
#' @param name optional name for each axis
#'
#' @export
dc_discrete <- function(x, ..., mins = NULL, maxs = NULL, dims = NULL, name = NULL) {
  UseMethod("dc_discrete")

}
#' @export
dc_discrete.default <- function(x, ..., mins = NULL, maxs = NULL, dims = NULL, name = NULL) {
  if (is.null(mins) & is.null(maxs) & is.null(dims)) return(dummy_discrete())
  stopifnot(length(mins) == length(maxs))
  stopifnot(length(mins) == length(dims))
  walphabet <- c(letters[-20], letters[20])
  if (is.null(name)) name <- walphabet[(seq_along(mins) + 21) %% 26 + 1]
  structure(data.frame(name = name, min = mins, max = maxs, dim = dims), class = c("discrete", "data.frame"), include = NULL)
}
#' @export
dc_discrete.BasicRaster <- function(x, ..., mins = NULL, maxs = NULL, dims = NULL, name = NULL) {
  rext <- raster_extent(x)
  rdim <- raster_dimension(x)
  dc_discrete(mins = rext[c("xmin", "ymin")],
              maxs  = rext[c("xmax", "ymax")],
              dims = rdim,
              name = name)
}

raster_extent <- function(x, ...) {
  unlist(slot_getter(x@extent, c("xmin", "xmax", "ymin", "ymax")))
}
raster_dimension <- function(x, ...) {
  unlist(slot_getter(x, c("ncols", "nrows")))
}

dc_coordinates <- function(x, ...) {
  UseMethod("dc_coordinates")
}
