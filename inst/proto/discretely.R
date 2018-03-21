library(silicore)
library(silicate)
library(sf)
x <- minimal_mesh[1, ]
x <- inlandwaters[4, ]
#x <- st_as_sf(anglr::simpleworld)

#x <- st_sf(geometry = st_sfc(
#  st_polygon(list(cbind(c(0, 1, 2, 0), c(0, 1, 0, 0))))
#))

xstart <- function(Xstart, Ystart, dxdy, Y) {
  ## Ystart is the top of the edge
  ## Yend is the row we are in
  ## Xstart is the left of the edge
  ## Xend  is the right of the edge
  Xstart + (Y - Ystart)*dxdy
}


library(raster)
r <- raster(extent(x) + 0.5, nrows = 200, ncols = 300)


library(dplyr)
sc <- SC0(x)
sc$coord <- sc$coord %>% mutate(i = as.integer(colFromX(r, x_)),
                                j = as.integer(rowFromY(r, y_)))
sc_start <- function(x) {
  x$segment %>%
    inner_join(x$coord %>% mutate(.vx0 = row_number()))%>%
    transmute(i0 = i, j0 = j)
}
sc_end <- function(x) {
  x$segment %>%
    inner_join(x$coord %>% mutate(.vx1 = row_number())) %>%
    transmute(i1 = i,
              j1 = j)

}


x0 <- sc_start(sc)
x1 <- sc_end(sc)
edges <- setNames(tibble::as_tibble(rbind(cbind(as.matrix(x0), 1:nrow(x0)),
                                          cbind(as.matrix(x1), 1:nrow(x0)))),
                  c("xi", "yi", "edge")) %>%
  group_by(edge) %>%
  arrange(yi) %>%
  mutate(node = row_number(), dxdy = diff(xi)/diff(yi)) %>%
  mutate(dxdy = ifelse(is.infinite(dxdy), 0, dxdy)) %>%
  ungroup() %>%
  arrange(edge, node)

em <- as.matrix(edges[, 1:2])
wideys <- cbind(em[seq(1, nrow(edges), by = 2), ],
                em[seq(2, nrow(edges), by = 2), ])
colnames(wideys) <- c(".x0", ".y0", ".x1", ".y1")
wideys <- as_tibble(wideys)
wideys$dxdy <- edges$dxdy[seq(1, nrow(edges), by = 2)]
plot(extent(0, ncol(r), 0, nrow(r)))
points(sc$coord[c("i", "j")], pch = ".")
for (jrow in 1:nrow(r)) {
  active <- jrow >= wideys$.y0 & jrow <= wideys$.y1
  if (any(active)) {
    e0 <- wideys %>% filter(active) %>% select(.x0, .x1, .y0, .y1, dxdy)
    e0 <- e0[order(pmin(e0$.x0, e0$.x1)), ]
    e0$X0 <- pmin(e0$.x0, e0$.x1)
    e0$Y0 <- ifelse(e0$dxdy < 0, e0$.y1, e0$.y0)
    e0 <- e0 %>% mutate(Xstart = xstart(X0, Y0, dxdy, jrow))

    e0 <- e0 %>% distinct(Xstart, .keep_all= TRUE)
    if (nrow(e0) > 0 ) {
     # stop()
      xints <- e0$Xstart
      #if (length(xints) == 3) stop()
if (length(unique(xints)) == 1) next;
      xints <- matrix(xints, ncol = 2, byrow = TRUE)
      xints[,1] <- floor(xints[,1])
      xints[,2] <- ceiling(xints[,2])
      #if (any(is.na(xints))) stop()
      #xints[is.na(xints)] <- 1
      xints <- na.omit(xints)
     for (j in seq_len(nrow(xints))) {
        points(cbind(seq(xints[j, 1], xints[j, 2]), jrow), pch = ".")
      }
    }
  }
}

