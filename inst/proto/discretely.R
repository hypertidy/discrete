library(silicore)
library(silicate)
library(sf)
x <- minimal_mesh[2, ]
#x <- inlandwaters[4, ]
#x <- st_as_sf(simpleworld[9, ])

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
for (jrow in 1:nrow(r)) {
  wideys$active <- jrow >= wideys$.y0 & jrow <= wideys$.y1
  e0 <- wideys %>% filter(active) %>% 
    mutate(xfirst = min(.x0, .x1))
  e0$odd <- seq_len(nrow(e0))
  xstart(228, 45, -17.4, jrow)
  if (nrow(e0) > 0 ) break; 
}

plot(setExtent(setValues(r, seq(ncell(r))), extent(0, ncol(r), 0, nrow(r))))
points(sc$coord[c("i", "j")])
abline(v = range(edges$xi), h = nrow(r) - range(edges$yi) + 1)

eget <- function(i = 1) {
  edges %>% filter(edge == i)
}
edraw <- function(i = 1) {
  eget(i) %>% select(xi, yi) %>% lines(lwd = 3)
invisible(NULL)
}
system.time({
l <- split(edges, edges$edge)
for (i in seq_along(l)) {
  y <- l[[i]]
  purrr::walk(seq(y$yi[1], y$yi[2]), 
              ~abline(v = xstart(y$xi[1], y$yi[1], y$dxdy[1], .x)))
}
})
