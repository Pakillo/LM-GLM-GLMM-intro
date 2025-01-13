## ----knitr_setup, include=FALSE, cache=FALSE---------------------------------------------------------------------

library('knitr')

### Chunk options ###

## Text results
opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, size = 'tiny')

## Code decoration
opts_chunk$set(tidy = FALSE, comment = NA, highlight = TRUE, prompt = FALSE, crop = TRUE)

# ## Cache
opts_chunk$set(cache = TRUE)

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



## ----echo=1------------------------------------------------------------------------------------------------------
trees <- read.csv('data/trees.csv')
summary(trees[, 1:3])


## ----echo=FALSE--------------------------------------------------------------------------------------------------
plot(trees$dbh, trees$height, pch=20, las=1, cex.lab=1.4, xlab='DBH (cm)', ylab='Height (m)')


## ----lm, echo=1--------------------------------------------------------------------------------------------------
simple.lm <- lm(height ~ dbh, data = trees)
summary(simple.lm)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
summary(trees$dbh)
trees$dbh.c <- trees$dbh - 25


## ----echo=FALSE--------------------------------------------------------------------------------------------------
plot(trees$dbh.c, trees$height, pch=20, las=1, cex.lab=1.4, xlab='DBH (cm)', ylab='Height (m)')
abline(lm(height ~ dbh.c, data=trees), col='red', lwd=3)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
library(arm)
simple.lm <- lm(height ~ dbh.c, data = trees)
display(simple.lm)


## ----echo = FALSE, results='hide'--------------------------------------------------------------------------------

set.seed(28)
# example with tree diameters
diam.sd <- 20
diam <- rnorm(8, 30, diam.sd)

prior.diam <- 50
prior.diam.var <- 100

library(blmeco)
blmeco::triplot.normal.knownvariance(theta.data = mean(diam), 
                                     n = length(diam), 
                                     variance.known = diam.sd*diam.sd, 
                                     prior.theta = prior.diam, 
                                     prior.variance = prior.diam.var)
title("Sample size = 8")


## ----echo = FALSE, results='hide', eval=FALSE--------------------------------------------------------------------
# 
# # height <- runif(10, 170, 190)
# #
# # prior.height <- 160
# # prior.height.var <- 20
# 
# set.seed(123)
# # example with students' hours of sleep
# sleephours.sd <- 2
# sleephours <- rnorm(8, 9, sleephours.sd)
# 
# prior.sleep <- 7
# prior.sleep.var <- 1
# 
# library(blmeco)
# blmeco::triplot.normal.knownvariance(theta.data = mean(sleephours),
#                                      n = length(sleephours),
#                                      variance.known = sleephours.sd*sleephours.sd,
#                                      prior.theta = prior.sleep,
#                                      prior.variance = prior.sleep.var)
# title("How many hours of daily sleep?")


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics("images/likelihood.PNG")


## ----echo = FALSE, results='hide'--------------------------------------------------------------------------------

set.seed(28)
# example with tree diameters
diam.sd <- 20
diam <- rnorm(8, 30, diam.sd)

prior.diam <- 50
prior.diam.var <- 100

library(blmeco)
blmeco::triplot.normal.knownvariance(theta.data = mean(diam), 
                                     n = length(diam), 
                                     variance.known = diam.sd*diam.sd, 
                                     prior.theta = prior.diam, 
                                     prior.variance = prior.diam.var)
title("Sample size = 8")


## ----echo = FALSE, results='hide'--------------------------------------------------------------------------------

set.seed(28)
# example with tree diameters
diam.sd <- 20
diam <- rnorm(100, 30, diam.sd)

prior.diam <- 50
prior.diam.var <- 100

library(blmeco)
blmeco::triplot.normal.knownvariance(theta.data = mean(diam), 
                                     n = length(diam), 
                                     variance.known = diam.sd*diam.sd, 
                                     prior.theta = prior.diam, 
                                     prior.variance = prior.diam.var)
title("Sample size = 100")


## ----echo=T------------------------------------------------------------------------------------------------------
library('brms')

height.formu <- brmsformula(height ~ dbh.c)


## ----echo=1------------------------------------------------------------------------------------------------------
get_prior(height.formu, data = trees)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
plot(density(rnorm(1000, 0, 1000)), main='', xlab='Height (m)', cex = 3)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
plot(density(rnorm(1000, 2, 0.5)), main='', xlab='Height (m)', cex = 3)


## ----echo=T------------------------------------------------------------------------------------------------------
priors <- c(
  set_prior('normal(30, 10)', class = 'Intercept'), 
  set_prior('normal(0.5, 0.4)', class = 'b'),
  set_prior('normal(0, 5)', class = 'sigma')
)


## ----echo=T------------------------------------------------------------------------------------------------------
plot(density(rnorm(1000, 25, 10)))


## ----echo=T------------------------------------------------------------------------------------------------------
plot(density(rnorm(1000, 0.5, 0.5)))


## ----echo=F------------------------------------------------------------------------------------------------------
sig <- rnorm(10000, 0, 5)
sigma <- na.omit(ifelse(sig < 0, NA, sig))
hist(sigma, breaks = 30, probability = T)


## ----echo=T, results='hide'--------------------------------------------------------------------------------------
height.mod <- brm(height.formu,
         data = trees,
         prior = priors,
         sample_prior = 'only')


## ----echo=T------------------------------------------------------------------------------------------------------
pp_check(height.mod, ndraws = 100)


## ----echo=T, results='hide'--------------------------------------------------------------------------------------
height.mod <- brm(height.formu,
         data = trees,
         prior = priors)


## ----echo=T------------------------------------------------------------------------------------------------------
summary(height.mod)


## ----echo=T------------------------------------------------------------------------------------------------------
plot(height.mod)


## ----echo=T------------------------------------------------------------------------------------------------------
pp_check(height.mod, ndraws = 100)


## ----echo=T, eval=FALSE------------------------------------------------------------------------------------------
# library('shinystan')
# launch_shinystan(height.mod)


## ----out.width='70%'---------------------------------------------------------------------------------------------
include_graphics("images/Bayesian_workflow.png")

