# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
res_010 = readRDS(file = "validation/conditional-hdr-area-010.Rds")
res_025 = readRDS(file = "validation/conditional-hdr-area-025.Rds")
res_050 = readRDS(file = "validation/conditional-hdr-area-050.Rds")
res_075 = readRDS(file = "validation/conditional-hdr-area-075.Rds")
res_090 = readRDS(file = "validation/conditional-hdr-area-090.Rds")

colnames(res_010) = c("seam", "pitcher", "batter")
colnames(res_025) = c("seam", "pitcher", "batter")
colnames(res_050) = c("seam", "pitcher", "batter")
colnames(res_075) = c("seam", "pitcher", "batter")
colnames(res_090) = c("seam", "pitcher", "batter")

res_050 |>
  as_tibble() |>
  pivot_longer(col = seam:batter, names_to = "type", values_to = "area") |>
  ggplot() +
  aes(x = area, group = type, fill = type) +
  geom_histogram() +
  facet_wrap(~ type, nrow = 3)

res_075 |>
  as_tibble() |>
  pivot_longer(col = seam:batter, names_to = "type", values_to = "area") |>
  ggplot() +
  aes(x = area, group = type, fill = type) +
  geom_histogram() +
  facet_wrap(~ type, nrow = 3)

res_090 |>
  as_tibble() |>
  pivot_longer(col = seam:batter, names_to = "type", values_to = "area") |>
  ggplot() +
  aes(x = area, group = type, fill = type) +
  geom_histogram() +
  facet_wrap(~ type, nrow = 3)
