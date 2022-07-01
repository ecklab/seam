# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
res_010 = readRDS(file = "validation/conditional-hdr-area-010.Rds")
res_025 = readRDS(file = "validation/conditional-hdr-area-025.Rds")
res_050 = readRDS(file = "validation/conditional-hdr-area-050.Rds")
res_075 = readRDS(file = "validation/conditional-hdr-area-075.Rds")
res_090 = readRDS(file = "validation/conditional-hdr-area-090.Rds")

res_mean = rbind(
  colMeans(res_010),
  colMeans(res_025),
  colMeans(res_050),
  colMeans(res_075),
  colMeans(res_090)
)

colnames(res_mean) = c("seam", "pitcher", "batter")
rownames(res_mean) = c("0.10", "0.25", "0.50", "0.75", "0.90")

# preview table
res_mean

# output with latex formatting
knitr::kable(res_mean, format = "latex")

res_sd = rbind(
  apply(res_010, 2, sd),
  apply(res_025, 2, sd),
  apply(res_050, 2, sd),
  apply(res_075, 2, sd),
  apply(res_090, 2, sd)
)

colnames(res_sd) = c("seam", "pitcher", "batter")
rownames(res_sd) = c("0.10", "0.25", "0.50", "0.75", "0.90")

# preview table
res_sd

# output with latex formatting
knitr::kable(res_sd, format = "latex")

res_050 |>
  as_tibble() |>
  pivot_longer(col = seam:batter, names_to = "type", values_to = "area") |>
  ggplot() +
  aes(x = area, group = type, fill = type) +
  geom_histogram() +
  facet_wrap(~ type, nrow = 3)
