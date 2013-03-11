> circular:::CirclePlotRad
function (xlim = c(-1, 1), ylim = c(-1, 1), uin = NULL, shrink = 1,  tol = 0.04, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
          control.circle = circle.control())  {
  xlim <- shrink * xlim
  ylim <- shrink * ylim
  midx <- 0.5 * (xlim[2] + xlim[1])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
  midy <- 0.5 * (ylim[2] + ylim[1])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
  oldpin <- par("pin")
  xuin <- oxuin <- oldpin[1]/diff(xlim)
  yuin <- oyuin <- oldpin[2]/diff(ylim)
  if (is.null(uin)) {
    if (yuin > xuin) 
      yuin <- xuin
    else xuin <- yuin
  }
  else {
    if (length(uin) == 1) 
      uin <- uin * c(1, 1)
    if (any(c(xuin, yuin) < uin)) 
      stop("uin is too large to fit plot in")
    xuin <- uin[1]
    yuin <- uin[2]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  n <- control.circle$n
  x <- cos(seq(0, 2 * pi, length = n))
  y <- sin(seq(0, 2 * pi, length = n))
  axes <- FALSE
  log <- ""
  xaxs <- "i"
  yaxs <- "i"
  ann <- par("ann")
  frame.plot <- axes
  panel.first <- NULL
  panel.last <- NULL
  asp <- NA
  plot.default(x = x, y = y, type = control.circle$type, xlim = xlim, 
               ylim = ylim, log = "", main = main, sub = sub, xlab = xlab, 
               ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot, 
               panel.first = panel.first, panel.last = panel.last, asp = asp, 
               col = control.circle$col, bg = control.circle$bg, pch = control.circle$pch, 
               cex = control.circle$cex, lty = control.circle$lty, lwd = control.circle$lwd)
}
<environment: namespace:circular>