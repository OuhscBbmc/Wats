#plot.circular
function (x, pch = 16, cex = 1, stack = FALSE, axes = TRUE, sep = 0.025,shrink = 1, bins = NULL, ticks = FALSE, tcl = 0.025, tcl.text = 0.125, 
          col = NULL, tol = 0.04, uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1), digits = 2, units = NULL, template = NULL, zero = NULL, 
          rotation = NULL, main = NULL, sub = NULL, xlab = "", ylab = "",  control.circle = circle.control(), ...)  {
  
  if (is.matrix(x) | is.data.frame(x)) {
    nseries <- ncol(x)
  }
  else {
    nseries <- 1
  }
  xx <- as.data.frame(x)
  xcircularp <- attr(as.circular(xx[, 1]), "circularp")
  type <- xcircularp$type
  modulo <- xcircularp$modulo
  if (is.null(units)) 
    units <- xcircularp$units
  if (is.null(template)) 
    template <- xcircularp$template
  if (template == "geographics" | template == "clock24") {
    zero <- pi/2
    rotation <- "clock"
  }
  else if (template == "clock12") {
    zero <- pi/2
    rotation <- "clock"
  }
  else {
    if (is.null(zero)) 
      zero <- xcircularp$zero
    if (is.null(rotation)) 
      rotation <- xcircularp$rotation
  }
  CirclePlotRad(xlim = xlim, ylim = ylim, uin = uin, shrink = shrink,  tol = tol, main = main, sub = sub, xlab = xlab, ylab = ylab, 
                control.circle = control.circle)
  if (is.null(bins)) {
    bins <- NROW(x)
  }
  else {
    bins <- round(bins)
    if (bins <= 0) 
      stop("bins must be non negative")
  }
  if (is.null(col)) {
    col <- seq(nseries)
  }
  else {
    if (length(col) != nseries) {
      col <- rep(col, nseries)[1:nseries]
    }
  }
  pch <- rep(pch, nseries, length.out = nseries)
  if (!is.logical(ticks)) 
    stop("ticks must be logical")
  if (axes) {
    axis.circular(at = NULL, labels = NULL, units = units, template = template, modulo = "2pi", zero = zero, 
                  rotation = rotation, tick = ticks, cex = cex, tcl = tcl,  tcl.text = tcl.text, digits = digits)
  }
  if (axes == FALSE & ticks) {
    at <- circular((0:bins)/bins * 2 * pi, zero = zero, rotation = rotation)
    ticks.circular(at, tcl = tcl)
  }
  for (iseries in 1:nseries) {
    x <- xx[, iseries]
    x <- na.omit(x)
    n <- length(x)
    if (n) {
      x <- conversion.circular(x, units = "radians", modulo = "2pi")
      attr(x, "circularp") <- attr(x, "class") <- NULL
      if (rotation == "clock") 
        x <- -x
      if (template == "clock12") 
        x <- 2 * x
      x <- x + zero
      x <- x%%(2 * pi)
      PointsCircularRad(x, bins, stack, col, pch, iseries, nseries, sep, 0, shrink, cex, ...)
    }
  }
  return(invisible(list(zero = zero, rotation = rotation, next.points = nseries * sep)))
}