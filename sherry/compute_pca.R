pca_geodesic <- function(data){

  pca <- foreach::foreach(i  = 1:nrow(data), .combine = "rbind") %do%{
    t(data$basis[[i]])
  }

  loadings <- stats::predict(stats::prcomp(pca))[,1:2]
  pca2 <- cbind(pca, loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = data$info,
                                       tries = data$tries,
                                       index_val = data$index_val,
                                       stepS = data$stepS)

  colnames(result)[1:5] <- c("x1", "x8", "x9", "x10", "x2")

  return(result)

}

pca_better <- function(data){

  pca <- foreach::foreach(i  = 1:nrow(data), .combine = "rbind") %do%{
    t(data$basis[[i]])
  }

  loadings <- stats::predict(stats::prcomp(pca))[,1:2]
  pca2 <- cbind(pca, loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = data$info,
                                       tries = data$tries,
                                       index_val = data$index_val,
                                       alpha = data$alpha,
                                       cooling = data$cooling)

  colnames(result)[1:5] <- c("x1", "x8", "x9", "x10", "x2")

  return(result)

}

pca_better_interrupt <- function(data){

  pca <- foreach::foreach(i  = 1:nrow(data), .combine = "rbind") %do%{
    t(data$basis[[i]])
  }

  loadings <- stats::predict(stats::prcomp(pca))[,1:2]
  pca2 <- cbind(pca, loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = data$info,
                                       tries = data$tries,
                                       index_val = data$index_val,
                                       alpha = data$alpha,
                                       cooling = data$cooling,
                                       type = data$type,
                                       compare = data$compare)

  colnames(result)[1:5] <- c("x1", "x8", "x9", "x10", "x2")

  return(result)

}


pca_better_eps <- function(data){

  pca <- foreach::foreach(i  = 1:nrow(data), .combine = "rbind") %do%{
    t(data$basis[[i]])
  }

  loadings <- stats::predict(stats::prcomp(pca))[,1:2]
  pca2 <- cbind(pca, loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                       info = data$info,
                                       tries = data$tries,
                                       index_val = data$index_val,
                                       alpha = data$alpha,
                                       cooling = data$cooling,
                                       eps = data$eps)

  colnames(result)[1:5] <- c("x1", "x8", "x9", "x10", "x2")

  return(result)

}
