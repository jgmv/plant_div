plot_par <- function(...) {
  par(
    bty = "l", # type of box surrounding plot
    cex = 1, # size of elements
    las = 1, # style of axis labels
    lend = 2, # line end style
    ljoin = 2, # line join style
    lwd = 1, # line width
    mar = c(4, 4, 1, 1), # number of lines of margin
    mgp = c(1.75, 0.25, 0), # position of axis elements
    oma = c(0, 0, 0, 0), # size of outer margins
    ps = 12, # point size
    pty = "m", # type of plot region
    tcl = -0.15, # height of axis tick marks
    xaxs = "i", # interval calculation in x axis
    xpd = F, # plotting region
    yaxs = "i", # interval calculation in y axis
    ... # additional arguments from par
  )
}
