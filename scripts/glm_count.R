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



## ----------------------------------------------------------------------------------------------------------------
library('ggplot2')


## ----seedl_load, echo=1------------------------------------------------------------------------------------------
seedl <- read.csv('data/seedlings.csv')
summary(seedl)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
table(seedl$count)


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
hist(seedl$count)


## ----poisson_eda2------------------------------------------------------------------------------------------------
plot(seedl$light, seedl$count, xlab = 'Light (GSF)', ylab = 'Seedlings')


## ----echo=TRUE---------------------------------------------------------------------------------------------------
seedl.glm <- glm(count ~ light, 
                 data = seedl, 
                 family = poisson)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(seedl.glm)


## ----poisson_glm-------------------------------------------------------------------------------------------------
seedl.glm <- glm(count ~ light, data = seedl, family = poisson)
summary(seedl.glm)


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# library(arm)
# library(ggplot2)
# 
# coefs <- as.data.frame(coef(sim(seedl.glm)))
# names(coefs) <- c('intercept', 'slope')
# 
# ggplot(coefs) +
#   geom_density(aes(slope), fill = 'grey80') +
#   xlim(-0.02, 0.02) +
#   geom_vline(xintercept = 0) +
#   ggtitle('Effect of light on seedling abundance')


## ----echo=T, out.width='70%'-------------------------------------------------------------------------------------
library('parameters')
plot(simulate_parameters(seedl.glm)) +
  geom_vline(xintercept = 0) +
  ggtitle('Effect of light on seedling abundance')


## ----poisson_params, echo=T--------------------------------------------------------------------------------------
coef(seedl.glm)[1]


## ----echo=T------------------------------------------------------------------------------------------------------
exp(coef(seedl.glm)[1])


## ----message = FALSE, echo=TRUE----------------------------------------------------------------------------------
library('easystats')
parameters(seedl.glm)
parameters(seedl.glm, exponentiate = TRUE)


## ----------------------------------------------------------------------------------------------------------------
estimate_relation(seedl.glm)


## ----poisson_visreg, echo = 2:3----------------------------------------------------------------------------------
library(visreg)
visreg(seedl.glm, scale = 'response', ylim = c(0, 7))
points(count ~ light, data = seedl, pch = 20)


## ----echo=4, eval=FALSE------------------------------------------------------------------------------------------
# library(sjPlot)
# library(ggplot2)
# theme_set(theme_minimal(base_size = 16))
# sjPlot::plot_model(seedl.glm, type = 'eff', show.data = TRUE)


## ----echo=T------------------------------------------------------------------------------------------------------
library('performance')
r2(seedl.glm)


## ----echo=T, results='asis'--------------------------------------------------------------------------------------
library('report')
report(seedl.glm)


## ----poisson_check, echo=2---------------------------------------------------------------------------------------
layout(matrix(1:4, nrow=2))
plot(seedl.glm)
dev.off()


## ----------------------------------------------------------------------------------------------------------------
ggResidpanel::resid_panel(seedl.glm)


## ----echo=2------------------------------------------------------------------------------------------------------
library('performance')
check_model(seedl.glm)


## ----poisson_check2, echo=T--------------------------------------------------------------------------------------
plot(seedl$light, resid(seedl.glm))


## ----------------------------------------------------------------------------------------------------------------
sims <- simulate(seedl.glm, nsim = 100)
yrep <- t(as.matrix(sims))
bayesplot::ppc_rootogram(seedl$count, yrep)


## ----echo=TRUE, out.width='80%'----------------------------------------------------------------------------------
check_predictions(seedl.glm)


## ----echo=2------------------------------------------------------------------------------------------------------
library(DHARMa)
simulateResiduals(seedl.glm, plot = TRUE)


## ----out.width='100%'--------------------------------------------------------------------------------------------
include_graphics('images/Gaus-Pois.png')


## ----echo=T------------------------------------------------------------------------------------------------------
simres <- simulateResiduals(seedl.glm, refit = TRUE)
testDispersion(simres)


## ----poisson_overdisp, echo=FALSE--------------------------------------------------------------------------------
seedl.overdisp <- glm(count ~ light, data = seedl, family = quasipoisson)
summary(seedl.overdisp)


## ----poisson_overdisp2, echo=TRUE, message = FALSE---------------------------------------------------------------
parameters(seedl.overdisp)
parameters(seedl.glm)


## ----pois_overdisp_eff1, echo=FALSE------------------------------------------------------------------------------
visreg(seedl.overdisp, scale = 'response')


## ----pois_overdisp_eff2, echo=FALSE------------------------------------------------------------------------------
visreg(seedl.glm, scale = 'response')


## ----echo=1:2----------------------------------------------------------------------------------------------------
library('MASS')
seedl.nb <- glm.nb(count ~ light, data = seedl)
summary(seedl.nb)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
compare_models(seedl.glm, seedl.nb)
compare_performance(seedl.glm, seedl.nb)


## ----------------------------------------------------------------------------------------------------------------
head(seedl)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
seedl.offset <- glm(count ~ light, 
                    offset = log(area), 
                    data = seedl, 
                    family = poisson)


## ----------------------------------------------------------------------------------------------------------------
summary(seedl.offset)


## ----echo=T------------------------------------------------------------------------------------------------------
exp(coef(seedl.offset)[1])


## ----echo =TRUE--------------------------------------------------------------------------------------------------
new.lights <- data.frame(light = c(10, 90))
predict(seedl.glm, newdata = new.lights, type = 'response', se.fit = TRUE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
new.lights <- data.frame(light = c(10, 90))
estimate_expectation(seedl.glm, data = new.lights)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
estimate_prediction(seedl.glm, data = new.lights)

