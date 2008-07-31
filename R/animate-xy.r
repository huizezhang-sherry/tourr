#X animate_xy(mtcars[, 1:5])
#X animate_xy(mtcars[, 1:5], little_tour)
#X animate_xy(mtcars[, 1:5],center=F)
#X animate_xy(mtcars[, 1:5],axes="bottomleft")
#X animate_xy(mtcars[, 1:5],axes="off")
animate_xy <- function(data, tourf = grand_tour, center=T, axes="center", ...) {
  labels <- abbreviate(colnames(data), 2)
  
  render_frame <- function() {
    par(pty = "s", mar = rep(1,4))
    blank_plot(xlim = c(-2, 2), ylim = c(-2, 2))    
  }
  render_transition <- function() {
    rect(-1.99, -1.99, 1.99, 1.99, col="#FFFFFFE6", border=NA)
  }
  render_data <- function(data, proj) {
    # Render axes
    if (axes == "center") {
      segments(0, 0, proj[, 1], proj[, 2], col="grey50")
      theta <- seq(0, 2 * pi, length = 50)
      lines(cos(theta), sin(theta), col="grey50")
      text(proj, label = labels, col="grey50")
    }
    else if (axes == "bottomleft") {
      segments(-1.25, -1.25, -1.25+proj[, 1]/2, -1.25+proj[, 2]/2, col="grey50")
      theta <- seq(0, 2 * pi, length = 50)
      lines(-1.25+cos(theta)/2, -1.25+sin(theta)/2, col="grey50")
      text(-1.25+proj[, 1]/2, -1.25+proj[, 2]/2, label = labels, col="grey50")
    }

    # Render projected points
    x <- data %*% proj
    if (center) x <- apply(x, 2, function(vec) {vec-mean(vec)})
    points(x, pch=20)
  }
  render_target <- function(target) {
    rect(-1.99, -1.99, 1.99, 1.99, col="#7F7F7F33", border=NA)
  }

  animate(
    data = data, tourf = tourf, d = 2, 
    render_frame = render_frame, render_data = render_data,
    render_transition = render_transition, render_target = render_target, 
    ...
  )
}