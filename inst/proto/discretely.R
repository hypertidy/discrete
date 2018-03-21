library(silicore)
library(silicate)
library(sf)
x <- minimal_mesh[1, ]
x <- inlandwaters[1, ]
#x <- st_as_sf(anglr::simpleworld)
eget <- function(i = 1) {
  edges %>% filter(edge == i)
}
edraw <- function(i = 1) {
  eget(i) %>% select(xi, yi) %>% lines(lwd = 3)
  invisible(NULL)
}

library(raster)
r <- raster(extent(x) + 0.5, nrows = 500, ncols = 3000)

#r <- raster(extent(1.5, 3, -0.1, 0.5), nrows = 20, ncols = 30)
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




library(dplyr)
sc <- SC0(x)


#y0 = (ras.ymax - poly(i, 1))/ras.yres - 0.5;
#y1 = (ras.ymax - poly(i+1, 1))/ras.yres - 0.5;

#(ymax(r) - y_) / res(r)[2] - 0.5
extent.data.frame <- function(x) extent(as.matrix(x))
setMethod("extent", "data.frame", extent.data.frame)
sc$coord <- sc$coord %>% mutate(i = as.integer(trunc((x_ - xmin(r))/res(r)[1]) + 1),
                                j = as.integer(1L + trunc(((ymax(r) - y_)/res(r)[2]))))

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



wideys <- wideys %>% filter(.y0 != .y1)
plot(extent(0, ncol(r), 0, nrow(r)))
points(sc$coord[c("i", "j")], pch = ".")

plot(raster::union(extent(sc$coord %>% dplyr::select(x_, y_)), extent(r)))
plot(extent(sc$coord %>% dplyr::select(x_, y_)), add = TRUE)
polygon(sc$coord[c("x_", "y_")], col = "grey")
plot(extent(r), add = TRUE, col = "dodgerblue")



plot(raster::union(extent(sc$coord %>% dplyr::select(i, j)), extent(0, ncol(r), 0, nrow(r))))
plot(extent(sc$coord %>% dplyr::select(i, j)), add = TRUE)
polygon(sc$coord[c("i", "j")], col = "grey")
plot(extent(0, ncol(r), 0, nrow(r)), add = TRUE, col = "dodgerblue")

system.time({
  l <- vector("list", nrow(r))
  yrange <- range(c(wideys$.y0, wideys$.y1))
  for (jrow in min(yrange):max(yrange)) {
    active <- jrow >= wideys$.y0 & jrow <= wideys$.y1
    if (any(active)) {
      e0 <- wideys %>% filter(active) %>% select(.x0, .x1, .y0, .y1, dxdy)
      e0 <- e0[order(pmin(e0$.x0, e0$.x1)), ]
      e0$X0 <- pmin(e0$.x0, e0$.x1)
      e0$Y0 <- ifelse(e0$dxdy < 0, e0$.y1, e0$.y0)
      e0 <- e0 %>% mutate(Xstart = xstart(X0, Y0, dxdy, jrow))

      # print(nrow(e0))
      inner <- vector("list", nrow(e0)/2)
      for (i in seq_len(nrow(e0))) {
        x_end <- ncol(r)
        if (i %% 2 == 1) {  ## ODD
          x_start <- min(c(max(c(1, e0$Xstart[i])), ncol(r)))
        } else {            ## EVEN
          x_end <- min(c(max(c(1, e0$Xstart[i])), ncol(r)))
          #run <- seq(x_start, x_end)
          #    points(cbind(run, jrow), pch = ".")
          inner[[i]] <- c(x_start, x_end)
        }
      }
      l[[jrow]] <- inner
    }
  }
})
