# this function creates a waffle plot using the code snippet from
# https://stats.stackexchange.com/questions/17842/how-to-make-waffle-charts-in-r

waffleplot <- function(x, rows, cols = seq_along(x), ...) {
  xx <- rep(cols, times = x)
  lx <- length(xx)
  m <- matrix(nrow = rows, ncol = (lx %/% rows) + (lx %% rows != 0))
  m[1:length(xx)] <- xx
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(list(...))
  plot.new()
  o <- cbind(c(row(m)), c(col(m))) + 1
  plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
              asp = 1, xaxs = 'i', yaxs = 'i')
   rect(o[, 2], o[, 1], o[, 2] + .85, o[, 1] + .85, col = c(m), border = NA)
  invisible(list(m = m, o = o))
}



