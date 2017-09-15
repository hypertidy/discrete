#' tile guess based on overall dimension
tile_heuristic <- function(dim, base_guess = c(1024L, 1024L), fix = FALSE) {
  rem <- c(0, 0)
  if (!fix) {
    rem[1L] <- dim[1L] %% base_guess[1L]
    rem[2L] <- dim[2L] %% base_guess[2L]
  }# TODO: everything is not done here
  rem
}


#' @param dim is the size of the grid
#' @param tile is the size of the tiles
tile_dim <- function(dim, tile = NULL) {
  if (is.null(tile)) {
    tile <-  c(dim[1], rep(1L, length(dim) -1))
    } else {
      stopifnot(length(dim) == length(tile))
    }
  tile
}

#' 6 figure transform to discrete axes
#' @param x 6 figure geotransform as returned by raster_info
#' @param dim 2 figure dimXY as returned by raster_info
transform6 <- function(x, dim) {
  disc_rete(mins = c(x[1], x[4] + dim[2] * x[6]),
            maxs = c(x[1] + dim[1] * x[2], x[4]),
            dims = dim)
}
#' a default discretization for testing, called by `disc_rete`
dummy_discrete <- function() {
  as.data.frame(list(name = c("x", "y"), min = c(0, 0), max = c(3, 4), dim = c(3, 4)), stringsAsFactors = FALSE)
}
#' table of axes
#' @param mins minimum coordinates
#' @param maxs maximum coordinates
#' @param dims axis length
#' @param name optional name for each axis
disc_rete <- function(mins, maxs, dims, name = NULL) {
  if (missing(mins)) return(dummy_discrete())
  stopifnot(length(mins) == length(maxs))
  stopifnot(length(mins) == length(dims))
  walphabet <- c(letters[-20], letters[20])
  if (is.null(name)) name <- walphabet[(seq_along(mins) + 21) %% 26 + 1]
  data.frame(name = name, min = mins, max = maxs, dim = dims)
}
#' the coordinates of the edges of cells
#'
#' @param x (ordered) row of axes
edge_coord <- function(x) {
  setNames(do.call(expand.grid, lapply(split(x, seq_len(nrow(x))), function(axisrow) {
    axis <- as.list(axisrow)
    seq(axis$min, axis$max, length = axis$dim + 1)
  }
  )), x$name)
}

#' a segment head/tail expander
pair_segs <- function (x) {
  cbind(head(x, -1), tail(x, -1))
}
#' the modulo "n-column" index  required
#' for the local 4-cornes of a cell from the
#' ordered vertex pool
pair_four <- function (xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}
#' build quad mesh from first two axes
#' @param x a discrete set of axes
#'
quad_mesh <- function(x, ...) {
  exy <- edge_coord(x[1:2, ])
  ncolumns <- x$dim[1]
  nrows <- x$dim[2]
  ind <- apply(pair_segs(seq(ncolumns + 1)), 1, pair_four, nc = ncolumns +
                 1)
  ind0 <- as.vector(ind) + rep(seq(0, length = nrows, by = ncolumns +
                                     1), each = 4 * ncolumns)
  list(vb = t(cbind(exy, 0, 1)), ib =  matrix(ind0, nrow = 4))

}
#' fast creation of simple features tiles from abstract
#' axis specification
#' methods? raster_info, geotransform/dim, raster
tile_gon <- function(x, crs = NA_character_, ...) {
  qm <- quad_mesh(x)
  close_ring <- c(1L, 2L, 3L, 4L, 1L)
  ## spex:::polygonize.RasterLayer
  l <- lapply(split(t(qm$vb[1:2, qm$ib]),
                    rep(seq_len(ncol(qm$ib)), each = 4L)),
              function(x) structure(list(matrix(x, ncol = 2L)[close_ring, ]),
                                    class = c("XY", "POLYGON", "sfg")))
  ##  too slow
  #st_sf(geometry = st_sfc(l))
  sf1 <- list(tile = seq_along(l))

  sf1[["geometry"]] <- structure(l, n_empty = 0L, crs = structure(list(epsg = NA_integer_,
                                                                       proj4string = NA_character_), class = "crs"),
                                 precision = 0, bbox = structure(c(xmin = x$min[1],
                                                                   ymin = x$min[2], xmax = x$max[1], ymax = x$max[2]),
                                                                 class = "bbox"), class = c("sfc_POLYGON", "sfc"))

  st_as_sf(as.data.frame(sf1))
}


#' worker to build an empty list of NULLs, except optionally
#' for one of the cells to hold a table of data
populate_raster <- function(n, tile = NULL, data = NULL) {
  out <- replicate(n, NULL)
  ##, tibble::as_tibble(list(data = numeric())))
  if (!is.null(tile)) out[[tile]] <- tibble::tibble(data = data)
  out
}

#' the index extent of a local geom () to  a real extent of `obj` converted to GDAL's standard
#' offsetX/Y, srcwinX/Y form
gdal_source <- function(obj, geom) {
  ex <- tabularaster:::index_extent(obj, extent(geom))
  offset_source_x <- c(xmin(ex), xmax(ex))
  offset_source_y <- c(nrow(obj) - c(ymax(ex), ymin(ex)))
  as.vector(rbind(offset_source_x, offset_source_y))
}


