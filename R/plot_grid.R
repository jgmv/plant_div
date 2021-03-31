#' Plot Grid with Plants Layout for Neighbor Experiments
#'
#' Generates and plots with a grid representing the plant layout in the neighbor experiments, with selected number of plant per side.
#' @param n_side number of plants per side (rows or columns).
#' @param pch type of point to plot   
#' @return A plot with a plant grid.
#' @keywords neighbors
#' @export
#' @examples
#' phy_plants()
plot_grid <- function(n_side = 7, pch = 16, ...) {
  par(mar = rep(0.5, 4), lwd = 1.5, cex = 1.5)
  n_side <- n_side - 1
  plot(1, 1, type = "n", xlim = c(-(n_side + 1), n_side + 1),
    ylim = c(-(n_side + 1), n_side + 1), axes = F, xlab = NA, ylab = NA)
  l <- list(x = seq(-n_side, n_side, 2), y = seq(-n_side, n_side, 2))
  points(do.call(expand.grid, l), pch = pch, ...)
  box(lwd = 5)
}
