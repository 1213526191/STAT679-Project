library(mlmRev)
library(tidyverse)
library(lme4)


data(star)
star2 <- na.omit(star)
star_sub <- star2 %>%
  group_by(id, cltype, sch) %>%
  summarise(count = n()) %>%
  filter(count == 4)
id_index <- star_sub$id
dat <- star %>%
  filter(id %in% id_index) %>%
  group_by(id) %>%
  mutate(mean_math = mean(math), mean_read = mean(read)) %>%
  group_by(id) %>%
  top_n(1, yrs) %>%
  select(-gr, -read, -math, -yrs)

set.seed(12345)

library("rstan") # observe startup messages
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat$eth = factor(dat$eth)

X <- model.matrix(mean_math ~ cltype + ses + eth, data = dat)

stanDat <- list(
  N = nrow(X),
  J = length(unique(dat$sch)),
  L = length(unique(dat$birthq)),
  K = ncol(X),
  math = dat$mean_math,
  X = X,
  sch = as.integer(factor(dat$sch)),
  birthq = as.integer(factor(dat$birthq))
)

set.seed(12345)
ranIntFit <- stan(file = "fit8.stan", data = stanDat,
                  iter = 3000, chains = 4)

require(shinystan)
my_sso = launch_shinystan(ranIntFit)