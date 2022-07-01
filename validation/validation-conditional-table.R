# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load intermediate results
results = readRDS(file = "validation/conditional-coverage.Rds")

# view "average" conditional results
Reduce(`+`, results) / length(results)

# helper function
extract_row = function(mat, row) {
  mat[row, ]
}

# extract and combine results for each level
res_010 = dplyr::bind_rows(lapply(results, extract_row, row = 1))
res_025 = dplyr::bind_rows(lapply(results, extract_row, row = 2))
res_050 = dplyr::bind_rows(lapply(results, extract_row, row = 3))
res_075 = dplyr::bind_rows(lapply(results, extract_row, row = 4))
res_090 = dplyr::bind_rows(lapply(results, extract_row, row = 5))

cond_covs = cbind(
  round(c(
    seam = mean(res_010$seam > 0.10),
    batter = mean(res_010$batter > 0.10),
    pitcher = mean(res_010$pitcher > 0.10)
  ), 3),

  round(c(
    seam = mean(res_025$seam > 0.25),
    batter = mean(res_025$batter > 0.25),
    pitcher = mean(res_025$pitcher > 0.25)
  ), 3),

  round(c(
    seam = mean(res_050$seam > 0.50),
    batter = mean(res_050$batter > 0.50),
    pitcher = mean(res_050$pitcher > 0.50)
  ), 3),

  round(c(
    seam = mean(res_075$seam > 0.75),
    batter = mean(res_075$batter > 0.75),
    pitcher = mean(res_075$pitcher > 0.75)
  ), 3),

  round(c(
    seam = mean(res_090$seam > 0.90),
    batter = mean(res_090$batter > 0.90),
    pitcher = mean(res_090$pitcher > 0.90)
  ), 3)
)

colnames(cond_covs) = c(0.10, 0.25, 0.50, 0.75, 0.90)
cond_covs

# output with latex formatting
knitr::kable(cond_covs, format = "latex")

n = length(res_010$seam)
cond_covs_se = cbind(
  round(c(
    seam = sd(res_010$seam > 0.10) / sqrt(n),
    batter = sd(res_010$batter > 0.10) / sqrt(n),
    pitcher = sd(res_010$pitcher > 0.10) / sqrt(n)
  ), 3),

  round(c(
    seam = sd(res_025$seam > 0.25) / sqrt(n),
    batter = sd(res_025$batter > 0.25) / sqrt(n),
    pitcher = sd(res_025$pitcher > 0.25) / sqrt(n)
  ), 3),

  round(c(
    seam = sd(res_050$seam > 0.50) / sqrt(n),
    batter = sd(res_050$batter > 0.50) / sqrt(n),
    pitcher = sd(res_050$pitcher > 0.50) / sqrt(n)
  ), 3),

  round(c(
    seam = sd(res_075$seam > 0.75) / sqrt(n),
    batter = sd(res_075$batter > 0.75) / sqrt(n),
    pitcher = sd(res_075$pitcher > 0.75) / sqrt(n)
  ), 3),

  round(c(
    seam = sd(res_090$seam > 0.90) / sqrt(n),
    batter = sd(res_090$batter > 0.90) / sqrt(n),
    pitcher = sd(res_090$pitcher > 0.90) / sqrt(n)
  ), 3)
)

colnames(cond_covs_se) = c(0.10, 0.25, 0.50, 0.75, 0.90)
cond_covs_se

# output with latex formatting
knitr::kable(cond_covs_se, format = "latex")

res_050_long = tidyr::pivot_longer(res_050, cols = 1:3)
ggplot(res_050_long) +
  aes(x = value, fill = name) +
  geom_density(alpha = 0.2)

res_075_long = tidyr::pivot_longer(res_075, cols = 1:3)
ggplot(res_075_long) +
  aes(x = value, fill = name) +
  geom_density(alpha = 0.2)

res_090_long = tidyr::pivot_longer(res_090, cols = 1:3)
ggplot(res_090_long) +
  aes(x = value, fill = name) +
  geom_density(alpha = 0.2)

res_050 %>% filter(seam >= 0.50 & batter < 0.50) %>% nrow()
res_050 %>% filter(seam < 0.50 & batter >= 0.50) %>% nrow()

res_050 %>% filter(seam >= 0.50 & pitcher < 0.50) %>% nrow()
res_050 %>% filter(seam < 0.50 & pitcher >= 0.50) %>% nrow()

res_050 %>% filter(seam >= 0.50 & pitcher < 0.50 & batter < 0.50) %>% nrow()
res_050 %>% filter(seam < 0.50 & pitcher >= 0.50 & batter >= 0.50) %>% nrow()

res_075 %>% filter(seam >= 0.75 & batter < 0.75) %>% nrow()
res_075 %>% filter(seam < 0.75 & batter >= 0.75) %>% nrow()

res_075 %>% filter(seam >= 0.75 & pitcher < 0.75) %>% nrow()
res_075 %>% filter(seam < 0.75 & pitcher >= 0.75) %>% nrow()

res_075 %>% filter(seam >= 0.75 & pitcher < 0.75 & batter < 0.75) %>% nrow()
res_075 %>% filter(seam < 0.75 & pitcher >= 0.75 & batter >= 0.75) %>% nrow()

res_090 %>% filter(seam >= 0.90 & batter < 0.90) %>% nrow()
res_090 %>% filter(seam < 0.90 & batter >= 0.90) %>% nrow()

res_090 %>% filter(seam >= 0.90 & pitcher < 0.90) %>% nrow()
res_090 %>% filter(seam < 0.90 & pitcher >= 0.90) %>% nrow()

res_090 %>% filter(seam >= 0.90 & pitcher < 0.90 & batter < 0.90) %>% nrow()
res_090 %>% filter(seam < 0.90 & pitcher >= 0.90 & batter >= 0.90) %>% nrow()
