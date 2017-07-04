#' Collection of axes.
#'
#'
#' @param ... d_axis inputs
#'
#' @return
#' @export
#'
#' @examples
#' @name axes
#' @export
axes <- function(...) UseMethod("axes")
#' @name axes
#' @export
axes.default <- function(...) {
  lst <- list(...)
  nms <- names(lst)
  print(nms)
  if (is.null(nms) | any(nchar(nms) < 1L)) stop("all axes must be named")
  lst <- lapply(seq_along(lst), function(x) {lst[[x]]$name <- nms[x]; lst[[x]]})
  structure(lst, class = c("axes", "list"))
}



#' Abstract axis transform.
#'
#' An axis transform may be rectlinear or affine. The shear affine (3 parameters per axis) case is
#' rarely needed in this context, we leave it for later.  Rectilinear is wasteful for the 2 parameter
#' affine but it's so worthwhile because of the extra simplicity that we accept the inefficiency for now.
#'
#' In the context of an axis being recitlinar or affine (2 parameters), it may in fact be "degenerate",
#' in different ways. Any discretization axis has two possible (and sensible) default affine transforms,
#' one is "scaled", to exist within the interval \[0,1\]. The second is to exist within
#' the interval \[0,n\] where 'n' is the number of discrete steps within the axis. If the
#' transform is not specified, we must choose one or the other. `graphics::image` and `raster::raster`
#' choose the first, as can be seen when called on a `matrix`. it's not like those packages
#' are in agreement, however - the first interprets the interval as being a half-cell offset,
#' but the latter fits literally within the range.  A rectlinear axis is a bit simpler, in that
#' it is degenerate if its index is identical to its coordinate. To get real work done we choose
#' to proceed within the interval \[0,n\] which has the advantage of the degenerate case still
#' being simply informative about the structure of the original data, with a straightforward
#' correspondence between structural position and "coordinate". The angstroms package takes this approach
#' for dealing with curvlinear rasters that fit neither an affine or "non-degenerate" rectilinear transform.
#' The raster package only deals with the affine case (support for 3-parameters per axis is present, but rarely used).
#' Raster is terribly powerful though, as an I/O and "abstract discretization" engine, and can be coaxed
#' into service for rectlinear and curvlinear cases relatively easily (see angstroms).
#'
#' Rectlinear that isn't degenerate also has the problem of explicit bounds on the interval,
#' which it usually won't specify in NetCDF as it has "centre coordinates". That's not always
#' true but it's wild enough to be a problem.
#'
#' Affine is not supported yet, the primary motivation for this is to have sets of
#' rectlinear axes (even if they are degenerate affine) as this is completely absent
#' from anywhere in the R ecosystem as far as I can tell.
#'
#' If you are looking for affine transformation I suggest the spatstat or sf packages as
#' starting points, but there are many other packages that implement these basic tools in
#' various ways.
#'
#' @param x a discretizable-axis input
#' @param ...
#' @importFrom tibble tibble
#' @name axis_transform
#' @export
axis_transform <- function(x, ...) {
  UseMethod("axis_transform")
}
#' @name axis_transform
#' @export
axis_transform.default <- function(x, coord = NULL, affine = NULL, ...)  {
  stop("blah")
}
#' @name axis_transform
#' @export
axis_transform.d_axis <- function(x, coord = NULL, affine = NULL, ...) {
  if (is.null(coord)) coord <- seq(min(x), max(x), length = length(x))
  lst <- list(index = seq_len(length(x)),
              coord = coord, ...)

  out <- tibble::as_tibble(lst)
  structure(out, class = c("axis_transform", class(out)))
}

#' Abstract axis
#'
#' Create an axis from inputs `n`umber, `min`imum and `max`imum, and optionally
#' a `name`. The name is intended to be present and unique for downstream use, but in
#' isolation are optional.
#'
#' Continuous axes are supported, but only for some kind of completeness. The
#' intention is for straightforward 1-D discretization and the ability to be continuous is
#' probably better supported elsewhere (i.e. the scales package).
#' @param n number of discrete steps in the axis
#' @param min the minimum location in the axis (defaults to zero)
#' @param max the maximum location in the axis (defaults to `n`)
#' @param name a name to apply to the axis
#' @name d_axis
#' @export
d_axis <- function(n, min, max, name) UseMethod("d_axis")
#' @name d_axis
#' @export
#' @importFrom tibble tibble
d_axis.default <- function(n,  min = 0, max = n, name = NA_character_) {
  if (!min < max) stop("minimum position must be less than the maximum")
  structure(tibble::tibble(n = n, min = min, max = max,  name = name), class = "d_axis")
}

#' name print
#' @export
print.d_axis <- function(x, ...) {
  cat(sprintf("d_axis  %s\n", x$name))
  cat(sprintf("type: %s\n", axis_type(x)))

  cat(sprintf("min: %f\n", min(x)))
  cat(sprintf("max: %f\n", max(x)))
}
length.d_axis <- function(x) x$n
min.d_axis <- function(x, ...) x$min
max.d_axis <- function(x, ...) x$max
axis_type <- function(x) {
  if (is.finite(x$n))  "discrete" else "continuous"
}

is_continuous <- function(x, ...)  UseMethod("is_continuous")
is_continuous.axis <- function(x, ...) axis_type(x) == "continuous"
is_discrete <- function(x, ...) UseMethod("is_discrete")
is_discrete <- function(x, ...) axis_type(x) == "discrete"

