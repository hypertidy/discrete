#' @importFrom methods slot
#' @importFrom stats setNames
slot_getter <- function(x, slots) {
  setNames(lapply(slots, function(i) methods::slot(x, i)), slots)
}
