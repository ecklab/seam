# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
results_many_n = readRDS(file = "validation/conditional-top-n-cov-n.Rds")

res_tnc_n = t(sapply(results_many_n, colMeans))
graph_points = c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000)
rownames(res_tnc_n) = graph_points
res_tnc_n = as.data.frame(res_tnc_n)
res_tnc_n$n = as.numeric(rownames(res_tnc_n))
res_tnc_n = res_tnc_n |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher"))

res_tnc_n |>
  ggplot() +
  aes(x = n, y = value, color = name) +
  geom_point() +
  geom_line() +
  ylab("Average conditional coverage") +
  ggtitle("Conditional coverage vs region size") +
  theme_bw()
