> circular:::LinesCircularRad
function (x, y, join = FALSE, nosort = FALSE, offset = 1, shrink = 1, ...) {
  n <- length(x)
  if (!nosort) {
    xorder <- order(x)
    x <- x[xorder]
    y <- y[xorder]
    spacings <- c(diff(x), x[1] - x[n] + 2 * pi)
    pos <- which.max(spacings)[1]
    if (pos == n) 
      xorder <- 1:n
    else xorder <- c((pos + 1):n, 1:pos)
  }
  else {
    xorder <- 1:n
  }
  z <- (y/shrink + offset) * cos(x)
  w <- (y/shrink + offset) * sin(x)
  z <- z[xorder]
  w <- w[xorder]
  if (join) {
    z <- c(z, z[1])
    w <- c(w, w[1])
  }
  lines.default(x = z, y = w, ...)
  invisible(list(x = z, y = w))
}
<environment: namespace:circular>