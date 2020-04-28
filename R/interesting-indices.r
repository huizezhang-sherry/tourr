chi <- function(){

  function(data, proj){
    weight = TRUE
    lambda = 0.1
    r = 1
    ck = NA

    if (is.na(ck[1])) {
      # Encontrar a probabilidade de normalizacao bivariada normal sobre cada caixa radial
      fnr <- function(x) { x * exp(-0.5 * x^2) } # veja que aqui a funcao normal padrao bivariada esta em Coordenadas Polares
      ck  <- rep(1,40)
      ck[1:8]   <- integrate(fnr, 0, sqrt(2*log(6))/5)$value/8
      ck[9:16]  <- integrate(fnr, sqrt(2*log(6))/5  , 2*sqrt(2*log(6))/5)$value/8
      ck[17:24] <- integrate(fnr, 2*sqrt(2*log(6))/5, 3*sqrt(2*log(6))/5)$value/8
      ck[25:32] <- integrate(fnr, 3*sqrt(2*log(6))/5, 4*sqrt(2*log(6))/5)$value/8
      ck[33:40] <- integrate(fnr, 4*sqrt(2*log(6))/5, 5*sqrt(2*log(6))/5)$value/8
    }

    x <- as.matrix(data)
    a <- as.matrix(proj[,1])
    b <- as.matrix(proj[,2])

    n <- nrow(data)
    z   <- matrix(0, nrow = n, ncol = 2)
    ppi <- 0
    pk  <- rep(0,48)
    eta <- pi * (0:8)/36
    delang <- 45 * pi/180
    delr   <- sqrt(2 * log(6))/5
    angles <- seq(0, (2 * pi), by = delang)
    rd <- seq(0, (5 * delr), by = delr)
    nr <- length(rd)
    na <- length(angles)
    j <- 1
    while (j <= 9) {
      # rotaciona o plano
      aj <- a * cos(eta[j]) - b * sin(eta[j])
      bj <- a * sin(eta[j]) + b * cos(eta[j])

      # projeta os dados
      z[,1] <- x %*% aj
      z[,2] <- x %*% bj

      # Converte coordenadas cardesianas em polares
      r  <- sqrt(z[,1]^2 + z[,2]^2)
      th <- atan2(z[,2],z[,1])

      # Encontrar todos os angulos que sao negativos
      ind <- which(th < 0)
      th[ind] <- th[ind] + 2*pi

      # find points in each box
      i <- 1
      while (i <= (nr-1)) {	# loop over each ring
        k <- 1
        while (k <= (na-1)) { # loop over each wedge
          ind <- which(r > rd[i] & r < rd[i+1] & th > angles[k] & th < angles[k+1])
          pk[(i-1)*8+k] <- (length(ind)/n - ck[(i-1)*8+k])^2 / ck[(i-1)*8+k]
          k <- k + 1
        }
        i <- i + 1
      }

      # find the number in the outer line of boxes
      k <- 1
      while (k <= (na-1)) {
        ind <- which(r > rd[nr] & th > angles[k] & th < angles[k+1])
        pk[40+k] <- (length(ind)/n-(1/48))^2/(1/48)
        k <- k + 1
      }

      ppi <- ppi + sum(pk)

      j <- j + 1
    }

    index <- ppi / 9
    index
  }
}



#' Kolmogorov index.
#'
#' Calculates the Kolmogorov index.
#'
#' @keywords hplot
#' @export
#'
kol <- function() {


  function(mat){

    norm_bin_count <- ash::bin1(rnorm(nrow(mat)) %>% scale(), c(min(mat), max(mat)), 100)$nc

    mat_bin_count <- ash::bin1(mat %>% scale(), c(min(mat), max(mat)), 100)$nc
    diff <- sum((mat_bin_count - norm_bin_count)^2)/nrow(mat)

    diff
  }
}



kol_cdf <- function() {

  function(mat){
    norm <- rnorm(nrow(mat))

    as.numeric(ks.test(mat, norm)$statistic)
  }
}

#' Holes index.
#'
#' Calculates the holes index. See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#'
#' @keywords hplot
#' @export
holes <- function() {

  function(mat) {
    n <- nrow(mat)
    d <- ncol(mat)

    num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
    den <- 1 - exp(-d / 2)

    num / den
  }
}

#' Central mass index.
#'
#' Calculates the central mass index.  See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#'
#' @keywords hplot
#' @export
cmass <- function() {
  function(mat) {
    n <- nrow(mat)
    d <- ncol(mat)

    num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
    den <- 1 - exp(-d / 2)

    1 - num / den
  }
}

#' LDA projection pursuit index.
#'
#' Calculate the LDA projection pursuit index.  See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#'
#' @param cl class to be used.  Such as "color"
#' @keywords hplot
#' @export
lda_pp <- function(cl) {
  if (length(unique(cl)) == 0)
    stop("You need to select the class variable!")
  if (length(unique(cl)) == 1)
    stop("LDA index needs at least two classes!")

  function(mat) {
    if (ncol(mat) > 1) {
      fit <- stats::manova(mat ~ cl)

      1 - summary(fit, test = "Wilks")$stats[[3]]
    } else {
      summary(stats::aov(mat ~ cl))[[1]][4]
    }
  }
}

#' PDA projection pursuit index.
#'
#' Calculate the PDA projection pursuit index.  See Lee and Cook (2009)
#' A Projection Pursuit Index for Large p, Small n Data
#'
#' @param cl class to be used.  Such as "color"
#' @param lambda shrinkage parameter (0 = no shrinkage, 1 = full shrinkage)
#' @keywords hplot
#' @export
pda_pp <- function(cl, lambda=0.2) {
  if (length(unique(cl)) == 0)
    stop("You need to select the class variable!")
  if (length(unique(cl)) < 2)
    stop("PDA index needs at least two classes!")

  # Convert class to sequential integers, and sort
  cl.i <- as.integer(as.factor(cl))
  cl.sort <- order(cl.i)
  cl <- cl.i[cl.sort]

  function(mat) {
    mat <- as.matrix(mat)

    # Reorder so classes are adjacent
    if (ncol(mat) == 1)
      mat <- as.matrix(mat[cl.sort, ])
    else
      mat <- mat[cl.sort, ]

    ngroup <- table(cl.i)     # the obs. no. in each class, stored in table
    groups <- length(ngroup)  # no. of classes
    gname <- names(ngroup)    # names of classes, now is integer 1,2,3...

    .CalIndex(nrow(mat), ncol(mat), groups, mat, cl, gname,
        as.integer(ngroup), lambda)
  }
}

# @param n no. of obs,
# @param p no. of variables,
# @param groups no. of classes,
# @param fvals sorted matrix
# @param groupraw sorted class label of whole matrix
# @param gname names of classes, now is integer 1,2,3...;
# @param ngroup the obs. no. in each class
# @param lambda shrinkage parameter (0 = no shrinkage, 1 = full shrinkage)
.CalIndex<-function(n, p, groups, fvals, groupraw, gname, ngroup, lambda) {
  # int i, j, k, right, left
  g <- groups
  mean <- matrix(rep(0, g*p), g, p)
  ovmean <- matrix(rep(0, p), p)
  group <- matrix(rep(0, n), n)  # the class label of each obs

  right <- n-1
  left <- 0

  group <- groupraw;

  val <- 0

# Calculate mean for within class and the overall mean
  for (i in 1:n) {
    for (j in 1:p) {
      mean[group[i],j] <- mean[group[i],j] + fvals[i,j]/ngroup[group[i]]
      ovmean[j] <- ovmean[j] + fvals[i,j]/n
    }
  }

  cov <- matrix(rep(0,p*p),p,p)
  tempcov <- matrix(rep(0,p*p),p,p)

  for (i in 1:n) {
    for (j in 1:p) {
      for (k in 1:j) {
        cov[k,j] <- cov[k,j] + (1-lambda)*(fvals[i,j] -
             mean[group[i],j]) * (fvals[i,k]-mean[group[i],k])
        cov[j,k] <- cov[k,j]
      }
    }
  }

  for (j in 1:p)
    cov[j,j] <- cov[j,j] + lambda*n

  tempcov <- cov

# =========================================================
  if (ncol(tempcov) > 1) {
    pivot <- matrix(rep(0, p), p)
    det <- 0

    val <- .ludcomp(tempcov, p, pivot)
  }
  else
    val <- as.numeric(tempcov)
# ===========================================================

  for (j in 1:p)
    for (k in 1:p)
      for (i in 1:g)
	cov[j,k] <- cov[j,k] + (1-lambda) * ngroup[i] *
          (mean[i,j] - ovmean[j]) * (mean[i,k] - ovmean[k])

  tempcov <- cov
  if (ncol(tempcov) > 1)
    tempval <- .ludcomp(tempcov, p, pivot)
  else
    tempval <- tempcov

  if (tempval < 0.00000001) {
    val <- 0
    print ("ZERO VARIANCE!")
  }
  else
    val <- 1 - val/tempval

  return(val)
}

#==============================================================================
.ludcomp <- function(a, n, pivot)
{
  det <- 1;
  temp <- 0;
  s <- matrix(rep(0, n), n)
  for (i in 1:n) {
    s[i] <- a[i,1+1]
    for (j in 2:n)
      if (s[i] < a[i,j])
        s[i] <- a[i,j]
  }

  for( k in 1:(n-1)) {
    for ( i in k:n )
    {
      temp <- abs(a[i,k]/s[i])
      if (i==k) {
        c <- temp
        pivot[k] <- i
      }
      else if (c<temp)  {
	c <- temp
	pivot[k] <- i
      }
    }

    if (pivot[k] != k)
    {
      det <- det * (-1)
      for (j in k:n) {
        temp <- a[k,j]
        a[k,j] <- a[pivot[k],j]
        a[pivot[k],j] <- temp
      }
      temp <- s[k]
      s[k] <- s[pivot[k]]
      s[pivot[k]] <- temp
    }

    for ( i in (k+1):n) {
      temp <- a[i,k]/a[k,k]
      a[i,k] <- temp
      for (j in (k+1):n)
        a[i,j] <- a[i,j] - temp*a[k,j]
      }
      det <- det * a[k,k]
  }

  det <- det * a[n,n]

  return(det)

}
