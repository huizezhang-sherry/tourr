load("result_geodesic.rda")
library(foreach)
library(factoextra)

compute_pca <- function(names){
  rows <- result_geodesic %>% filter(info== "interpolation", col == names)
  pca <- foreach(i  = 1:nrow(rows), .combine = "rbind") %do%{
    t(rows$basis[[i]])
  }

  res.pca <- prcomp(pca)
  loadings <- res.pca$rotation[,c(1,2)]
  pca2 <- cbind(pca, pca %*% loadings)

  result <- as.data.frame(pca2) %>% mutate(id = 1: nrow(pca2),
                                           info = rows$info,
                                           V5_names = rows$col)

  colnames(result)[1:4] <- c("x1", "x8", "x9", "x10")

  return(result)

}

names <- list("x2", "x3", "x4", "x5", "x6", "x7")
result <- map_df(names, compute_pca)
result %>% View()
