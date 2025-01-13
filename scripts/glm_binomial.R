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



## ----prepare_titanic_data, echo=FALSE, eval=FALSE----------------------------------------------------------------
# titanic <- read.table('http://www.amstat.org/publications/jse/datasets/titanic.dat.txt')
# names(titanic) <- c('class', 'age', 'sex', 'survived')
# titanic$class <- factor(titanic$class, labels = c('crew', 'first', 'second', 'third'))
# titanic$age <- factor(titanic$age, labels = c('child', 'adult'))
# titanic$sex <- factor(titanic$sex, labels = c('female', 'male'))
# write.csv(titanic, file = 'data/titanic_long.csv', row.names=FALSE, quote=FALSE)


## ----read_titanic, echo=FALSE------------------------------------------------------------------------------------
titanic <- read.csv('data/titanic_long.csv')
head(titanic)


## ----titanic_lm, echo=TRUE---------------------------------------------------------------------------------------
m5 <- lm(survived ~ class, data = titanic)
library('easystats')
check_model(m5)


## ----titanic_lm_resid, echo=FALSE--------------------------------------------------------------------------------
hist(resid(m5))


## ----out.width='40%'---------------------------------------------------------------------------------------------
include_graphics('images/modeling_process.png')


## ----echo=TRUE---------------------------------------------------------------------------------------------------
table(titanic$class, titanic$survived)


## ----titanic_dplyr, echo=c(-1)-----------------------------------------------------------------------------------
library('dplyr')
titanic |>
  group_by(class, survived) |>
  summarise(count = n())


## ----titanic_eda, echo=TRUE, out.width = '100%'------------------------------------------------------------------
plot(factor(survived) ~ factor(class), data = titanic)


## ----echo=TRUE, out.width='80%'----------------------------------------------------------------------------------
library('ggmosaic')
ggplot(titanic) +
  geom_mosaic(aes(x = product(survived, class))) +
  labs(x = '', y = 'Survived')


## ----echo=TRUE---------------------------------------------------------------------------------------------------
tit.glm <- glm(survived ~ class, 
               data = titanic, 
               family = binomial)


## ----titanic_glm, echo=1-----------------------------------------------------------------------------------------
tit.glm <- glm(survived ~ class, data = titanic, family = binomial)
summary(tit.glm)


## ----tit_glm_effects, echo=TRUE----------------------------------------------------------------------------------
library('effects')
allEffects(tit.glm)


## ----tit_glm_effects2, echo=TRUE---------------------------------------------------------------------------------
summary(allEffects(tit.glm))


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('easystats')   # 'modelbased' pkg
estimate_means(tit.glm)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
estimate_contrasts(tit.glm)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('easystats')   # 'performance' pkg
r2(tit.glm)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
kable(xtable::xtable(tit.glm), digits = 2)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('modelsummary')
modelsummary(tit.glm, output = 'markdown')


## ----effects_plot, echo=TRUE-------------------------------------------------------------------------------------
plot(allEffects(tit.glm))


## ----echo=2------------------------------------------------------------------------------------------------------
library('visreg')
visreg(tit.glm, scale = 'response', rug = FALSE)


## ----echo=4, out.width='70%', eval=FALSE-------------------------------------------------------------------------
# library(ggplot2)
# library(sjPlot)
# theme_set(theme_minimal(base_size = 16))
# sjPlot::plot_model(tit.glm, type = 'eff', terms = 'class')


## ----echo=2------------------------------------------------------------------------------------------------------
library('easystats')
plot(parameters(tit.glm), show_intercept = TRUE)


## ----echo=TRUE, out.width='70%'----------------------------------------------------------------------------------
no.intercept <- glm(survived ~ class - 1, family = binomial, data = titanic)
plot(parameters(no.intercept))


## ----tit_glm_check, echo=2---------------------------------------------------------------------------------------
layout(matrix(1:4, nrow = 2))
plot(tit.glm)
dev.off()


## ----echo=TRUE, out.width='80%', eval=FALSE----------------------------------------------------------------------
# plot(binned_residuals(tit.glm))


## ----binnedplot, eval=FALSE--------------------------------------------------------------------------------------
# predvals <- predict(tit.glm, type='response')
# arm::binnedplot(predvals, titanic$survived - predvals)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
check_model(tit.glm)


## ----out.width='70%', echo = 2, tinycode = TRUE------------------------------------------------------------------
library('easystats')
check_predictions(tit.glm)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# library(bayesplot)
# sims <- simulate(tit.glm, nsim = 100)
# ppc_bars(titanic$survived, yrep = t(as.matrix(sims)))


## ----echo=TRUE---------------------------------------------------------------------------------------------------
library('DHARMa')
simulateResiduals(tit.glm, plot = TRUE)


## ----echo=TRUE, out.width='60%'----------------------------------------------------------------------------------
library('predtools')
titanic$surv.pred <- predict(tit.glm, type = 'response')
calibration_plot(data = titanic, obs = 'survived', pred = 'surv.pred', 
                 x_lim = c(0,1), y_lim = c(0,1)) 


## ----echo=FALSE--------------------------------------------------------------------------------------------------
visreg(tit.glm, scale = 'response')


## ----tit_sex_eda, out.width='100%'-------------------------------------------------------------------------------
plot(factor(survived) ~ as.factor(sex), data = titanic)


## ----tit_sex, echo=FALSE-----------------------------------------------------------------------------------------
tit.sex <- glm(survived ~ sex, data = titanic, family = binomial)
summary(tit.sex)


## ----tit_sex_effects, echo=TRUE----------------------------------------------------------------------------------
estimate_means(tit.sex)


## ----echo=FALSE, out.width='50%'---------------------------------------------------------------------------------
visreg(tit.sex, scale = 'response', rug = FALSE)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
simulateResiduals(tit.sex, plot = TRUE)


## ----echo=FALSE, out.width='70%'---------------------------------------------------------------------------------
library('dagitty')
g1 <- dagitty('dag {
              Class -> Survival
              Sex -> Survival
              Sex -> Class
              }')
plot(g1)


## ----tit_women, echo=TRUE----------------------------------------------------------------------------------------
table(titanic$class, titanic$survived, titanic$sex)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
tit.sex.class.add <- glm(survived ~ class + sex, family = binomial, data = titanic)
summary(tit.sex.class.add)


## ----echo=2------------------------------------------------------------------------------------------------------
par(mfcol = c(1, 2))
visreg(tit.sex.class.add, scale = 'response', rug = FALSE)
dev.off()


## ----tit_sex_class, echo=FALSE-----------------------------------------------------------------------------------
tit.sex.class.int <- glm(survived ~ class * sex, family = binomial, data = titanic)
summary(tit.sex.class.int)


## ----tit_sex_class_effects, echo=TRUE----------------------------------------------------------------------------
estimate_means(tit.sex.class.int)


## ----tit_sex_class_effects2, echo=TRUE---------------------------------------------------------------------------
visreg(tit.sex.class.int, by = 'sex', xvar = 'class', scale = 'response', rug = FALSE)


## ----echo=TRUE, out.width='80%'----------------------------------------------------------------------------------
library('sjPlot')
plot_model(tit.sex.class.int, type = 'int')


## ----echo=T------------------------------------------------------------------------------------------------------
library('easystats')   # 'performance' pkg
compare_performance(tit.sex.class.add, tit.sex.class.int)


## ----echo=T------------------------------------------------------------------------------------------------------
compare_parameters(tit.sex.class.add, tit.sex.class.int)


## ----echo=FALSE, eval = FALSE------------------------------------------------------------------------------------
# ## Calibration plot
# titanic$surv.pred <- predict(tit.sex.class.int, type = 'response')
# calibration_plot(data = titanic, obs = 'survived', pred = 'surv.pred',
#                  x_lim = c(0,1), y_lim = c(0,1), nTiles = 10)


## ----read_tit_short, echo = FALSE--------------------------------------------------------------------------------
tit.prop <- read.csv('data/titanic_prop.csv')
head(tit.prop)


## ----binom_prop, echo=1------------------------------------------------------------------------------------------
prop.glm <- glm(cbind(Yes, No) ~ Class, data = tit.prop, family = binomial)
summary(prop.glm)


## ----prop_glm_effects, echo=FALSE--------------------------------------------------------------------------------
allEffects(prop.glm)


## ----comp, echo=FALSE--------------------------------------------------------------------------------------------
allEffects(tit.glm)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
estimate_means(prop.glm)


## ----read_gdp, echo = FALSE--------------------------------------------------------------------------------------
#gdp <- read.csv('http://vincentarelbundock.github.io/Rdatasets/csv/car/UN.csv')
gdp <- read.csv('data/UN_GDP_infantmortality.csv')
names(gdp) <- c('country', 'mortality', 'gdp')
summary(gdp)


## ----gdp_eda-----------------------------------------------------------------------------------------------------
plot(mortality ~ gdp, data = gdp, main = 'Infant mortality (per 1000 births)')


## ----gdp_glm, echo=1---------------------------------------------------------------------------------------------
gdp.glm <- glm(cbind(mortality, 1000 - mortality) ~ gdp, 
               data = gdp, family = binomial)
summary(gdp.glm)


## ----gdp_effects, echo=T-----------------------------------------------------------------------------------------
allEffects(gdp.glm)


## ----gdp_effectsplot---------------------------------------------------------------------------------------------
plot(allEffects(gdp.glm))


## ----gdp_visreg, echo=c(2,3)-------------------------------------------------------------------------------------
library(visreg)
visreg(gdp.glm, scale = 'response')
points(mortality/1000 ~ gdp, data = gdp)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
simulateResiduals(gdp.glm, plot = TRUE)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
simres <- simulateResiduals(gdp.glm, refit = TRUE)
testDispersion(simres, plot = FALSE)


## ----echo = TRUE, message=FALSE----------------------------------------------------------------------------------
check_overdispersion(gdp.glm)


## ----logreg_overdisp, echo=1-------------------------------------------------------------------------------------
gdp.overdisp <- glm(cbind(mortality, 1000 - mortality) ~ gdp, 
               data = gdp, family = quasibinomial)
summary(gdp.overdisp)


## ----logreg_overdisp2, echo=TRUE---------------------------------------------------------------------------------
parameters(gdp.overdisp)


## ----echo=T------------------------------------------------------------------------------------------------------
parameters(gdp.glm)


## ----overdisp_plot1, echo=FALSE, out.width='100%'----------------------------------------------------------------
visreg(gdp.glm, scale = 'response', main = 'Binomial')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----overdisp_plot2, echo=FALSE, out.width='100%'----------------------------------------------------------------
visreg(gdp.overdisp, scale = 'response', main = 'Quasibinomial')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----------------------------------------------------------------------------------------------------------------
visreg(gdp.glm, ylab = 'Mortality (logit scale)')


## ----echo=FALSE--------------------------------------------------------------------------------------------------
library(ggResidpanel)
resid_panel(gdp.glm)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
gdp.na <- na.omit(gdp)
gdp.na$fit <- fitted(gdp.glm)
plot(gdp.na$fit, gdp.na$mortality, 
     xlab = 'Probability of mortality (predicted)',
     ylab = 'Mortality (observed)')


## ----------------------------------------------------------------------------------------------------------------
gdp.na$mort.pred <- predict(gdp.glm, type = 'response')
gdp.na$mort.prob <- gdp.na$mortality/1000
calibration_plot(data = gdp.na, obs = 'mort.prob', pred = 'mort.pred', 
                 x_lim = c(0,0.1), y_lim = c(0,0.1), nTiles = 10)


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
visreg(gdp.overdisp, main = 'Mortality ~ GDP', ylab = 'Mortality (logit scale)')


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ gdp + I(gdp*gdp), 
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = 'Mortality ~ GDP + GDP^2', ylab = 'Mortality (logit scale)')


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
visreg(gdp.overdisp, main = 'Mortality ~ GDP', scale = 'response', ylab = 'Mortality')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ gdp + I(gdp*gdp), 
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = 'Mortality ~ GDP + GDP^2', scale = 'response', ylab = 'Mortality')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
visreg(gdp.overdisp, main = 'Mortality ~ GDP', ylab = 'Mortality (logit scale)')


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ log(gdp), 
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = 'Mortality ~ log(GDP)', ylab = 'Mortality (logit scale)')


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
visreg(gdp.overdisp, main = 'Mortality ~ GDP', scale = 'response', ylab = 'Mortality')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----echo=FALSE, out.width='100%'--------------------------------------------------------------------------------
gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ log(gdp), 
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = 'Mortality ~ log(GDP)', scale = 'response', ylab = 'Mortality')
points(mortality/1000 ~ gdp, data = gdp, pch = 20)


## ----eval=FALSE, echo=FALSE--------------------------------------------------------------------------------------
# ## Trying Poisson
# m <- glm(mortality ~ log(gdp), data = gdp, family = quasipoisson)
# summary(m)
# visreg(m, scale = 'response')
# points(mortality ~ gdp, data = gdp)
# 
# gdp.na <- na.omit(gdp)
# gdp.na$fit <- fitted(m)
# plot(gdp.na$fit, gdp.na$mortality,
#      xlab = 'Probability of mortality (predicted)',
#      ylab = 'Mortality (observed)')


## ----out.width='90%'---------------------------------------------------------------------------------------------
include_graphics('images/moths.jpg')


## ----echo = 1----------------------------------------------------------------------------------------------------
moth <- read.csv('data/moth.csv')
head(moth)


## ----echo = 1----------------------------------------------------------------------------------------------------
moth$REMAIN <- moth$PLACED - moth$REMOVED
head(moth)


## ----------------------------------------------------------------------------------------------------------------
pred.morph <- glm(cbind(REMOVED, REMAIN) ~ MORPH, data = moth, family = binomial)
summary(pred.morph)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(pred.morph))


## ----------------------------------------------------------------------------------------------------------------
pred.dist <- glm(cbind(REMOVED, REMAIN) ~ DISTANCE, data = moth, family = binomial)
summary(pred.dist)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(pred.dist))


## ----------------------------------------------------------------------------------------------------------------
pred.int <- glm(cbind(REMOVED, REMAIN) ~ MORPH * DISTANCE, data = moth, family = binomial)
summary(pred.int)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(pred.int))


## ----echo = TRUE-------------------------------------------------------------------------------------------------
simulateResiduals(pred.int, plot = TRUE)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics('images/tomato.jpg')


## ----------------------------------------------------------------------------------------------------------------
seed <- readr::read_csv('data/seedset.csv')
head(seed)
seed$plant <- as.factor(seed$plant)


## ----------------------------------------------------------------------------------------------------------------
plot(seeds ~ ovulecnt, data = seed)


## ----------------------------------------------------------------------------------------------------------------
plot(seeds ~ pcmass, data = seed)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
seedm <- glm(cbind(seeds, ovulecnt - seeds) ~ plant, data = seed, family = binomial)
#summary(seedm)
plot(allEffects(seedm))


## ----echo=FALSE--------------------------------------------------------------------------------------------------
seedm <- glm(cbind(seeds, ovulecnt - seeds) ~ plant + pcmass, data = seed, family = binomial)
#summary(seedm)
plot(allEffects(seedm))


## ----echo = TRUE-------------------------------------------------------------------------------------------------
soccer <- read.csv('data/soccer.csv')
soccer


## ----echo=FALSE--------------------------------------------------------------------------------------------------
soccer.mod <- glm(cbind(Scored, Nshots - Scored) ~ GoalkeeperTeam, data = soccer, family = binomial)
visreg(soccer.mod, scale = 'response', 
       ylab = 'Probability of scoring')

