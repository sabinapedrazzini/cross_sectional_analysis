#libraries
library(tidyverse)
library(brms)

#data
d_scales <- read.csv("data/d_scales.csv")
d_long_c4 <- d_scales %>% 
  pivot_longer(cols=c("coop_5_2","coop_5_3","coop_5_4")) %>% 
  rename(item_ca=name,
         ca=value)
d_long_c4_m2 <- d_long_c4 %>% 
  pivot_longer(cols=starts_with("ce_house")) %>% 
  rename(item_ce=name,
         ce=value) %>% 
  pivot_longer(cols=starts_with("anger_house")) %>% 
  rename(item_an=name,
         an=value)

#decide with basic settings to use
prior_empty <- c(#first item
  prior(normal(-1.07,2), class=Intercept, coef=1),
  prior(normal(-0.57,2), class=Intercept, coef=2),
  prior(normal(-0.18,2), class=Intercept, coef=3),
  prior(normal(0.18,2), class=Intercept, coef=4),
  prior(normal(0.57,2), class=Intercept, coef=5),
  prior(normal(1.07,2), class=Intercept, coef=6),
  #random intercepts => so they can vary across 0
  prior(exponential(1), class = sd))

prior_empty_T <- c(#first item
  prior(normal(-1.07,2), class=Intercept, coef=1, group=coop_5_2),
  prior(normal(-0.57,2), class=Intercept, coef=2, group=coop_5_2),
  prior(normal(-0.18,2), class=Intercept, coef=3, group=coop_5_2),
  prior(normal(0.18,2), class=Intercept, coef=4, group=coop_5_2),
  prior(normal(0.57,2), class=Intercept, coef=5, group=coop_5_2),
  prior(normal(1.07,2), class=Intercept, coef=6, group=coop_5_2),
  #second item
  prior(normal(-1.07,2), class=Intercept, coef=1, group=coop_5_3),
  prior(normal(-0.57,2), class=Intercept, coef=2, group=coop_5_3),
  prior(normal(-0.18,2), class=Intercept, coef=3, group=coop_5_3),
  prior(normal(0.18,2), class=Intercept, coef=4, group=coop_5_3),
  prior(normal(0.57,2), class=Intercept, coef=5, group=coop_5_3),
  prior(normal(1.07,2), class=Intercept, coef=6, group=coop_5_3),
  #third item
  prior(normal(-1.07,2), class=Intercept, coef=1, group=coop_5_4),
  prior(normal(-0.57,2), class=Intercept, coef=2, group=coop_5_4),
  prior(normal(-0.18,2), class=Intercept, coef=3, group=coop_5_4),
  prior(normal(0.18,2), class=Intercept, coef=4, group=coop_5_4),
  prior(normal(0.57,2), class=Intercept, coef=5, group=coop_5_4),
  prior(normal(1.07,2), class=Intercept, coef=6, group=coop_5_4),
  #random intercepts => so they can vary across 0
  prior(exponential(1), class = sd))

c4_empty <- brm(data=d_long_c4,
                family=cumulative(),
                prior=prior_empty,
                ca ~ (1|ID)+(1|item_ca),
                iter=4000, warmup=1000, chains=4, cores=4, 
                #threads=threading(10),
                seed=1)

c4_empty_probit <- brm(data=d_long_c4,
                       family=cumulative(link=probit),
                       prior=prior_empty,
                       ca ~ (1|ID)+(1|item_ca),
                       iter=4000, warmup=1000, chains=4, cores=3, threads=threading(10),
                       seed=1)

c4_empty_T <- brm(data=d_long_c4,
                  family=cumulative(),
                  prior=prior_empty_T,
                  ca | thres(gr = item_ca) ~ (1|ID)+(1|item_ca),
                  iter=4000, warmup=1000, chains=4, cores=3, threads=threading(10),
                  seed=1)

c4_empty_probit_T <- brm(data=d_long_c4,
                         family=cumulative(),
                         prior=prior_empty_T,
                         ca | thres(gr = item_ca) ~ (1|ID)+(1|item_ca),
                         iter=4000, warmup=1000, chains=4, cores=3, threads=threading(10),
                         seed=1)

saveRDS("fits/c4_empty")
saveRDS("fits/c4_empty_probit")
saveRDS("fits/c4_empty_T")
saveRDS("fits/c4_empty_probit_T")