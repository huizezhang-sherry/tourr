x1 <- rnorm(100, 0, 1)
x2 <- c(rnorm(50, -1, 1), rnorm(50, 1, 1))
x3 <- c(rep(-1, 50), rep(1, 50))
x4 <- c(rnorm(25, -1, 1), rnorm(75, 1, 1))
x5 <- c(rnorm(33, -1, 1), rnorm(33, 0, 1), rnorm(34, 1, 1))
x6 <- c(rnorm(45, -1, 1), rnorm(10, 0, 1), rnorm(45, 1,1))
x7 <- c(rnorm(50, -3, 1), rnorm(50, 3, 1))
x8 <- rnorm(100, 0, 1)
x9 <- rnorm(100, 0, 1)
x10 <- rnorm(100, 0, 1)

dt <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
df <- as.matrix(scale(dt, center = TRUE, scale = TRUE))


dt <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
df <- as.matrix(scale(dt, center = TRUE, scale = TRUE))

animate_dist(df[,c(3,1,8,9, 10)], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(5, 1))
animate_dist(df[,c(1,7,8,9, 10)], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(5, 1))
animate_dist(df[, 1:7], tour_path=guided_tour(index_f = holes(), 1), sphere = TRUE, start = basis_random(7, 1))

holes()(matrix(df[,6]))


animate_dist(flea[, 1:6], method = "hist", tour_path = guided_tour(holes())) ->a


dt <- cbind(x1, x2, x7, x10)
animate_dist(dt, method = "hist", tour_path = guided_tour(holes())) ->a
animate_dist(dt, method = "hist", tour_path = guided_tour(holes(), search_f = search_better)) ->a



