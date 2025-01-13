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
include_graphics('images/gam_simpson_fig2.jpg')


## ----------------------------------------------------------------------------------------------------------------
include_graphics('images/gam_simpson_fig3.jpg')


## ----echo = 1----------------------------------------------------------------------------------------------------
isotopes <- readRDS('data/isotope.rds')
head(isotopes)


## ----echo = 1:2--------------------------------------------------------------------------------------------------
library('mgcv')
m <- gam(d15N ~ s(Year, k = 15), data = isotopes, method = 'REML')
summary(m)


## ----echo = 2----------------------------------------------------------------------------------------------------
library('visreg')
visreg(m)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
library('DHARMa')
simulateResiduals(m, plot = TRUE)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
gam.check(m)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
testTemporalAutocorrelation(simulateResiduals(m), 
                            time = isotopes$Year)


## ----echo = 1----------------------------------------------------------------------------------------------------
mod <- gamm(d15N ~ s(Year, k = 15), data = isotopes,
            correlation = corCAR1(form = ~ Year), method = 'REML')
summary(mod$gam)


## ----echo=1------------------------------------------------------------------------------------------------------
mort <- read.csv('data/UN_GDP_infantmortality.csv')
head(mort)


## ----echo = 1:2--------------------------------------------------------------------------------------------------
library('MASS')
mort.glm <- glm.nb(infant.mortality ~ gdp, data = mort)
summary(mort.glm)


## ----------------------------------------------------------------------------------------------------------------
visreg(mort.glm)


## ----echo = 1:2--------------------------------------------------------------------------------------------------
mort$log.gdp <- log(mort$gdp)
mort.glm.log <- glm.nb(infant.mortality ~ log.gdp, data = mort)
summary(mort.glm.log)


## ----------------------------------------------------------------------------------------------------------------
visreg(mort.glm.log)


## ----echo = 1:2--------------------------------------------------------------------------------------------------
library('mgcv')
mort.gam <- gam(infant.mortality ~ s(log.gdp), family = nb, data = mort)
summary(mort.gam)


## ----------------------------------------------------------------------------------------------------------------
visreg(mort.gam)


## ----echo =T-----------------------------------------------------------------------------------------------------
gam.check(mort.gam)


## ----echo =T-----------------------------------------------------------------------------------------------------
library('easystats')
compare_performance(mort.glm, mort.glm.log, mort.gam)


## ----echo=1:2----------------------------------------------------------------------------------------------------
library('lme4')
data('sleepstudy')
library('ggplot2')
ggplot(sleepstudy) +
  aes(x = Days, y = Reaction) +
  geom_point() +
  facet_wrap(~Subject) 


## ----echo = 1----------------------------------------------------------------------------------------------------
sgamm <- gam(Reaction ~ s(Days, Subject, k = 3, bs = 'fs'), 
                   data = sleepstudy, method = 'REML')
summary(sgamm)
#gam.check(sleep.gamm)
#coef(p)


## ----echo = T----------------------------------------------------------------------------------------------------
visreg(sgamm, xvar = 'Days', by = 'Subject')

