# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
results_many_n = readRDS(file = "validation/conditional-top-n-cov-n.Rds")

res_tnc_n = t(sapply(results_many_n, colMeans))
graph_points = seq(from = 1500, to = 2500, by = 50)
rownames(res_tnc_n) = graph_points
res_tnc_n = as.data.frame(res_tnc_n)
res_tnc_n$n = as.numeric(rownames(res_tnc_n))
res_tnc_n = res_tnc_n |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher"))

res_tnc_n |>
  ggplot() +
  aes(x = n, y = value, color = name) +
  geom_point() +
  geom_line() +
  xlab("Number of grids") +
  ylab("Average conditional coverage") +
  ggtitle("Conditional coverage vs region size") +
  theme_bw()


knitr::kable(round(res_tnc_n %>% spread(name, value), 3), format = "latex")

## standard error calculation
res_tnc_n_se = t(sapply(results_many_n, FUN = function(xx) apply(xx, 2, function(x) sd(x)/sqrt(length(x))) ))
res_tnc_n_se


## K-S calculations
foo <- round(res_tnc_n %>% spread(name, value), 3)
ks.test(unlist(foo[,1]), unlist(foo[,3]))
ks.test(unlist(foo[,1]), unlist(foo[,2]))


## regression analysis
bar <- do.call(rbind, results_many_n)
colnames(bar)[1] <- "aseam"
bar_graph_points = rep(seq(from = 1500, to = 2500, by = 50), each = 195)
bar = as.data.frame(bar)
bar$n = unlist(bar_graph_points)
bar = bar |> tidyr::pivot_longer(cols = c("aseam", "batter", "pitcher"))

m <- lm(value ~ n + name, data = bar)
summary(m)
par(mfrow = c(2,2))
plot(m)

set.seed(13)
baz <- t(replicate(5000, coef(lm(value ~ n + name, data = bar[sample(1:nrow(bar), replace = TRUE), ]))))

ggplot(as.data.frame(baz), aes(x = namepitcher)) +
  geom_histogram(bins = 50) +
  theme_minimal() +
  geom_vline(xintercept = 0, color = "red", size = 1)
ggplot(as.data.frame(baz), aes(x = namebatter)) +
  geom_histogram(bins = 50) +
  theme_minimal() +
  geom_vline(xintercept = 0, color = "red", size = 1)


