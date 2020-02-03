load("result_geodesic_old.rda")
library(foreach)
library(factoextra)

compute_pca <- function(names){
  rows <- result_geodesic_old %>% filter( col == names)
  pca <- foreach(i  = 1:nrow(rows), .combine = "rbind") %do%{
    t(rows$basis[[i]])
  }

  res.pca <- prcomp(pca)
  loadings <- res.pca$rotation[,c(1,2)]
  pca2 <- cbind(pca, pca %*% loadings)

  result <- as_tibble(pca2) %>% mutate(id = 1: nrow(pca2),
                                           info = rows$info,
                                           V5_names = rows$col,
                                       tries = rows$tries)

  colnames(result)[1:4] <- c("x1", "x8", "x9", "x10")

  return(result)

}

index_val <- result_geodesic_old %>% dplyr::filter(col == "x2")
pca_result <- compute_pca("x2") %>%
  mutate(index_val = index_val$index_val)

# colored by index_val
pca_result %>%
  ggplot(aes(x= PC1, y = PC2, col = index_val)) +
  geom_point() +
  theme(aspect.ratio = 1)

# colored by id
pca_result %>%
  ggplot(aes(x= PC1, y = PC2, col = as.factor(tries))) +
  geom_path() +
  theme(aspect.ratio = 1)

# color by start/end
pca_result %>%
  ggplot(aes(x= PC1, y = PC2)) +
  geom_point() +
  geom_point(data = filter(pca_result, id ==1), color = "red") +
  geom_point(data = filter(pca_result, info == "linear_search_best"),
           color = "blue") +
  theme(aspect.ratio = 1)

# colour by parts of optimisation
ggplot(data = pca_result, aes(x= PC1, y = PC2, col = info)) +
  geom_path() +
  theme(aspect.ratio = 1)

ggplot(data = pca_result, aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_point(data = filter(pca_result, info == "interpolation"), col = "green") +
  geom_point(data = filter(pca_result, id ==1), color = "red") +
  geom_point(data = filter(pca_result, info == "linear_search_best"),
             color = "blue") +

  theme(aspect.ratio = 1)

# interpolation path
ggplot(data =filter(pca_result, info == "interpolation") ,
       aes(x= PC1, y = PC2, col = tries)) +
  geom_path() +
  theme(aspect.ratio = 1)

#####


ggplot(data = pca_result, aes(x= PC1, y = PC2)) +
  geom_path(alpha = 0.01) +
  geom_path(data = filter(pca_result, counter ==2), color = "red") +
  theme(aspect.ratio = 1)
