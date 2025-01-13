## ----knitr_setup, include=FALSE, cache=FALSE---------------------------------------------------------------------

library('knitr')

### Chunk options ###

## Text results
opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, size = 'tiny')

## Code decoration
opts_chunk$set(tidy = FALSE, comment = NA, highlight = TRUE, prompt = FALSE, crop = TRUE)

# ## Cache
# opts_chunk$set(cache = TRUE, cache.path = 'knitr_output/cache/')

# ## Plots
# opts_chunk$set(fig.path = 'knitr_output/figures/')
opts_chunk$set(fig.align = 'center', out.width = '90%')

### Hooks ###
## Crop plot margins
knit_hooks$set(crop = hook_pdfcrop)

## Reduce font size
## use tinycode = TRUE as chunk option to reduce code font size
# see http://stackoverflow.com/a/39961605
knit_hooks$set(tinycode = function(before, options, envir) {
  if (before) return(paste0('\n \\', options$size, '\n\n'))
  else return('\n\n \\normalsize \n')
  })



## ----out.width='15%', fig.align='left'---------------------------------------------------------------------------
include_graphics('images/nest.jpg')


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# n = 300
# diameter <- round(runif(n, 5, 15))
# old <- sample(c('no', 'yes'), size = n, replace = TRUE, prob = c(0.7, 0.3))
# eggs <- data.frame(diameter, old)
# 
# eggs$n.eggs <- VGAM::rzipois(n, lambda = 0.5*diameter, pstr0 = ifelse(eggs$old == 'yes', 0.9, 0.1))
# readr::write_csv(eggs, 'data/eggs.csv')
# 


## ----echo=1------------------------------------------------------------------------------------------------------
eggs <- read.csv('data/eggs.csv')
kable(head(eggs, n = 3))


## ----out.width='80%'---------------------------------------------------------------------------------------------
hist(eggs$n.eggs, breaks = length(unique(eggs$n.eggs)))


## ----echo=TRUE---------------------------------------------------------------------------------------------------
coplot(n.eggs ~ diameter | old, data = eggs)


## ----echo=1------------------------------------------------------------------------------------------------------
eggs.poi <- glm(n.eggs ~ old * diameter, 
              data = eggs, 
              family = poisson)


## ----------------------------------------------------------------------------------------------------------------
summary(eggs.poi)


## ----------------------------------------------------------------------------------------------------------------
library('visreg')
visreg(eggs.poi, xvar = 'diameter', by = 'old')


## ----echo=TRUE, out.width='80%'----------------------------------------------------------------------------------
library('easystats')
check_predictions(eggs.poi)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('DHARMa')
eggs.poi.res <- simulateResiduals(eggs.poi, plot = TRUE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testDispersion(eggs.poi.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testZeroInflation(eggs.poi.res)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
library('glmmTMB')
eggs.zip <- glmmTMB(n.eggs ~ diameter, 
                    family = 'poisson', 
                    ziformula = ~ old, 
                    data = eggs)


## ----------------------------------------------------------------------------------------------------------------
summary(eggs.zip)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
check_predictions(eggs.zip)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
eggs.zip.res <- simulateResiduals(eggs.zip, plot = TRUE)


## ----echo=TRUE, out.width='100%'---------------------------------------------------------------------------------
testZeroInflation(eggs.zip.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testDispersion(eggs.zip.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
eggs.zinb <- glmmTMB(n.eggs ~ diameter, 
                     family = 'nbinom2', 
                     ziformula = ~ old, 
                     data = eggs)


## ----------------------------------------------------------------------------------------------------------------
summary(eggs.zinb)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('parameters')
compare_models(eggs.poi, eggs.zip, eggs.zinb)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('performance')
compare_performance(eggs.poi, eggs.zip, eggs.zinb)


## ----out.width='30%'---------------------------------------------------------------------------------------------
include_graphics('images/hives.png')


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# n <- 500
# vaccinated <- sample(c(1, 0), size = n, replace = TRUE, prob = c(0.7, 0.3))
# age <- round(runif(n, 1, 90))
# area.cm2 <- round(runif(n, 5, 10))
# hives <- data.frame(age, vaccinated, area.cm2)
# 
# exposed <- sample(1:n, n-200)
# 
# n.hives <- extraDistr::rtpois(n = length(exposed),
#                                 lambda = -2*hives$vaccinated[exposed] +
#                                   0.1*hives$age[exposed] + log(hives$area.cm2[exposed]),
#                                 a = 1)
# summary(n.hives)
# hives$n.hives <- 0
# hives$n.hives[exposed] <- n.hives
# 
# write_csv(hives, 'data/hives.csv')


## ----echo=1------------------------------------------------------------------------------------------------------
hives <- read.csv('data/hives.csv')
summary(hives)


## ----------------------------------------------------------------------------------------------------------------
hist(hives$n.hives, breaks = length(unique(hives$n.hives)))


## ----echo=TRUE---------------------------------------------------------------------------------------------------
coplot(n.hives ~ age | as.factor(vaccinated), data = hives)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
hives.poi <- glm(n.hives ~ vaccinated * age, 
                 offset = log(area.cm2),
                 data = hives,
                 family = poisson)


## ----------------------------------------------------------------------------------------------------------------
summary(hives.poi)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
hives.poi.vr <- glm(n.hives ~ vaccinated * age +  offset(log(area.cm2)),
                 data = hives,
                 family = poisson)
visreg(hives.poi.vr, xvar = 'age', by = 'vaccinated')


## ----echo=TRUE---------------------------------------------------------------------------------------------------
check_predictions(hives.poi)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
hives.poi.res <- simulateResiduals(hives.poi, plot = TRUE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testDispersion(hives.poi.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testZeroInflation(hives.poi.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
hives.hur <- glmmTMB(n.hives ~ vaccinated + age,
                     family = truncated_poisson, 
                     ziformula = ~ 1,
                     offset = log(area.cm2),
                     data = hives)


## ----------------------------------------------------------------------------------------------------------------
summary(hives.hur)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
check_predictions(hives.hur)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
hives.hur.res <- simulateResiduals(hives.hur, plot = TRUE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testZeroInflation(hives.hur.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
testDispersion(hives.hur.res)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
compare_models(hives.poi, hives.hur)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
compare_performance(hives.poi, hives.hur)

