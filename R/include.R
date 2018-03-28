dc_include <- function(x, ...) {
  UseMethod("dc_include")
}
dc_include.discrete <- function(x, include = NULL, ...) {
  if (is.null(include)) return(x)
  attr(x, "include") <- sort(intersect(attr(x, "include"), include))
  x
}
dc_exclude <- function(x, ...) {
  UseMethod("dc_exclude")
}
dc_exclude.discrete <- function(x, exclude = NULL, ...) {
  if (is.null(exclude)) return(x)
  attr(x, "include") <- sort(setdiff(attr(x, "include"), exclude))
  x
}
