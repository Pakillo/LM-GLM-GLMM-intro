## ----knitr_setup, include=FALSE, cache=FALSE--------------------------------------------------------

library('knitr')

### Chunk options ###

## Text results
opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, size = 'tiny')

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



## ----echo = 2---------------------------------------------------------------------------------------
trees <- read.csv('data/trees.csv')
head(trees)
trees$site <- as.factor(trees$site)


## ----echo = 1---------------------------------------------------------------------------------------
lm.simple <- lm(height ~ dbh, data = trees)
summary(lm.simple)


## ----echo=FALSE-------------------------------------------------------------------------------------
library(ggplot2)
ggplot(trees) +
  aes(dbh, height) +
  geom_point() +
  geom_smooth(method = 'lm', linewidth = 3) +
  labs(x = 'DBH (cm)', y = 'Height (m)', title = 'Single intercept') +
  theme_minimal(base_size = 16)


## ----echo=FALSE-------------------------------------------------------------------------------------
ggplot(subset(trees, site == 1 | site == 2)) +
  aes(dbh, height, colour = site) +
  geom_point() +
  geom_smooth(method = 'lm', linewidth = 3) +
  labs(x = 'DBH (cm)', y = 'Height (m)', 
       title = 'Different intercept for each site') +
  theme_minimal(base_size = 16) +
  theme(legend.position = 'none')


## ----lm_varying, echo=FALSE-------------------------------------------------------------------------
lm.interc <- lm(height ~ site + dbh, data = trees)
summary(lm.interc)


## ----single_interc, echo=FALSE----------------------------------------------------------------------
library(visreg)
visreg(lm.simple)


## ----varying_interc, echo=FALSE---------------------------------------------------------------------
visreg(lm.interc, xvar = 'dbh', by = 'site', overlay = TRUE, band = FALSE)


## ----mixed, echo=1:2--------------------------------------------------------------------------------
library('glmmTMB')
mixed <- glmmTMB(height ~ dbh + (1|site), data = trees)
summary(mixed)


## ----mixed_lmer, echo=1:2---------------------------------------------------------------------------
library('lme4')
mixed <- lmer(height ~ dbh + (1|site), data = trees)
summary(mixed)


## ---------------------------------------------------------------------------------------------------
library(equatiomatic)
equatiomatic::extract_eq(mixed)


## ----mixed_coefs------------------------------------------------------------------------------------
coef(mixed)


## ----echo=FALSE-------------------------------------------------------------------------------------
lm.noint <- lm(height ~ site + dbh - 1, data = trees)


## ----echo=FALSE-------------------------------------------------------------------------------------
data.frame(lm = round(coef(lm.noint)[1:10], digits = 1), 
  mixed = round(coef(mixed)$site[,1], digits = 1))


## ---------------------------------------------------------------------------------------------------
library(broom.mixed)
tidy(mixed)


## ---------------------------------------------------------------------------------------------------
library(modelsummary)
modelsummary(mixed)


## ----mixed_vis3, echo=2-----------------------------------------------------------------------------
library(visreg)
visreg(mixed, xvar = 'dbh', by = 'site') 


## ----echo=2-----------------------------------------------------------------------------------------
mixed <- glmmTMB(height ~ dbh + (1|site), data = trees)
visreg(mixed, xvar = 'dbh', by = 'site', overlay = TRUE)


## ----echo=1-----------------------------------------------------------------------------------------
visreg(mixed, xvar = 'dbh', by = 'site', overlay = TRUE, band = FALSE)
mixed <- lmer(height ~ dbh + (1|site), data = trees)


## ----echo=FALSE-------------------------------------------------------------------------------------
library(sjPlot)
theme_set(theme_minimal(base_size = 16))
#sjp.lmer(mixed, type = 'ri.slope')
#plot_model(mixed, type = 'eff')


## ---------------------------------------------------------------------------------------------------
sjPlot::plot_model(mixed, type = 're')


## ----eval=FALSE-------------------------------------------------------------------------------------
# library('merTools')
# shinyMer(mixed)


## ----mixed_resid------------------------------------------------------------------------------------
plot(mixed)


## ----echo=TRUE--------------------------------------------------------------------------------------
library('performance')
check_model(mixed)


## ---------------------------------------------------------------------------------------------------
DHARMa::simulateResiduals(mixed, plot = TRUE, re.form = NULL)


## ----echo=TRUE--------------------------------------------------------------------------------------
check_predictions(mixed)


## ---------------------------------------------------------------------------------------------------
r2(mixed)


## ----echo=FALSE, eval=FALSE-------------------------------------------------------------------------
# ## Predicting heights at NEW sites!
# #https://github.com/lme4/lme4/issues/388#issuecomment-231398937
# newtree <- data.frame(dbh = 30, site = as.factor(25))
# p <- bootMer(mixed,
#         function(x) {simulate(x, newdata = newtree, re.form = ~0, allow.new.levels = TRUE)[[1]]},
#         nsim = 100)
# apply(p$t, 2, mean)
# apply(p$t, 2, sd)
# # similar to:
# apply(simulate(mixed, newdata = newtree, re.form = ~0, allow.new.levels = TRUE, nsim = 100), 1, mean)
# 


## ----read_sitedata, echo=TRUE, message=FALSE--------------------------------------------------------
sitedata <- read.csv('data/sitedata.csv')
sitedata


## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------
trees.full <- merge(trees, sitedata, by = 'site')
head(trees.full)


## ----echo=1-----------------------------------------------------------------------------------------
group.pred <- lmer(height ~ dbh + (1 | site) + temp, data = trees.full)
summary(group.pred)


## ---------------------------------------------------------------------------------------------------
mean(sitedata$temp)
trees.full$temp.c <- trees.full$temp - 18


## ----echo=1-----------------------------------------------------------------------------------------
group.pred <- lmer(height ~ dbh + (1 | site) + temp.c, data = trees.full)
summary(group.pred)


## ----eval=FALSE-------------------------------------------------------------------------------------
# shinyMer(group.pred)


## ----echo=FALSE-------------------------------------------------------------------------------------
plot(coef(mixed)$site[,1], coef(group.pred)$site[,1],
     xlim = c(10, 25), ylim = c(10, 25), 
     xlab = 'Without group predictor', ylab = 'With group predictor',
     main = 'Estimated site effects', las = 1)
abline(a = 0, b = 1)


## ----echo=FALSE-------------------------------------------------------------------------------------
plot(sitedata$temp, coef(group.pred)$site[,1],
     xlab = 'Temperature', ylab = 'site effect')


## ---------------------------------------------------------------------------------------------------
modelsummary(group.pred)


## ---------------------------------------------------------------------------------------------------
check_model(group.pred)


## ----echo=FALSE-------------------------------------------------------------------------------------
df <- data.frame(dbh = seq(10, 50, by = 1), 
                 height = seq(20, 60, by = 1))
ggplot(df) +
  aes(dbh, height) +
  geom_blank() +
  geom_abline(intercept = 25, slope = 0.6, linewidth = 2) +
  geom_abline(intercept = 40, slope = 0.1, colour = 'steelblue', linewidth = 2) +
  geom_abline(intercept = 50, slope = -0.3, colour = 'orangered', linewidth = 2)
  


## ---------------------------------------------------------------------------------------------------
mixed.slopes <- lmer(height ~ dbh + (1 + dbh | site), data=trees)
equatiomatic::extract_eq(mixed.slopes)


## ----echo = FALSE-----------------------------------------------------------------------------------
summary(mixed.slopes)


## ----echo = FALSE-----------------------------------------------------------------------------------
coef(mixed.slopes)


## ---------------------------------------------------------------------------------------------------
modelsummary(mixed.slopes)


## ---------------------------------------------------------------------------------------------------
visreg(mixed.slopes, xvar = "dbh", by = "site")


## ---------------------------------------------------------------------------------------------------
plot_model(mixed.slopes, type = 're')


## ---------------------------------------------------------------------------------------------------
check_model(mixed.slopes)


## ----echo=FALSE-------------------------------------------------------------------------------------
data('sleepstudy')
library(ggplot2)
ggplot(sleepstudy) +
  aes(x = Days, y = Reaction) +
  geom_point() +
  facet_wrap(~Subject) 


## ----echo=1-----------------------------------------------------------------------------------------
sleep <- lmer(Reaction ~ Days + (1+Days|Subject), data = sleepstudy)
summary(sleep)


## ----eval=TRUE--------------------------------------------------------------------------------------
visreg(sleep, xvar = 'Days', by = 'Subject')


## ----echo = 2---------------------------------------------------------------------------------------
library(mgcv)
sgamm <- mgcv::gam(Reaction ~ s(Days, Subject, k = 3, bs = 'fs'), 
                   data = sleepstudy, method = 'REML')
summary(sgamm)
#gam.check(sleep.gamm)
#coef(p)


## ---------------------------------------------------------------------------------------------------
visreg(sgamm, xvar = 'Days', by = 'Subject')


## ----echo=FALSE-------------------------------------------------------------------------------------
include_graphics('images/gamm_paper.PNG')


## ---------------------------------------------------------------------------------------------------
plot(dead ~ dbh, data = trees)


## ---------------------------------------------------------------------------------------------------
plot(factor(dead) ~ dbh, data = trees)


## ----echo=1-----------------------------------------------------------------------------------------
simple.logis <- glm(dead ~ dbh, data = trees, family=binomial)
summary(simple.logis)


## ----echo=1-----------------------------------------------------------------------------------------
logis2 <- glm(dead ~ dbh + site, data = trees, family=binomial)
summary(logis2)


## ----mixed_logis, echo=1----------------------------------------------------------------------------
mixed.logis <- glmer(dead ~ dbh + (1|site), data=trees, family = binomial)
summary(mixed.logis)


## ----mixedlogis_coefs-------------------------------------------------------------------------------
coef(mixed.logis)


## ----echo = TRUE------------------------------------------------------------------------------------
visreg(mixed.logis, xvar = 'dbh', by = 'site', scale = 'response')


## ----out.width='70%'--------------------------------------------------------------------------------
# plot_model(mixed.logis, type = 'eff', show.ci = TRUE)


## ----echo=FALSE, out.width = '100%'-----------------------------------------------------------------
set.seed(123)

nsites <- 20
ntrees <- 30

df <- data.frame(
  site = rep(1:nsites, each = ntrees),
  tree = rep(1:ntrees, times = nsites))

## Temperature around each tree
df <- df |>
  dplyr::group_by(site) |>
  dplyr::mutate(tree.temp = rnorm(n = ntrees, mean = site, 3)
  )

# df |>
#   dplyr::group_by(site) |>
#   dplyr::summarise(mean(tree.temp))

## Simulate tree height
df <- df |>
  dplyr::rowwise() |>
  dplyr::mutate(tree.height = rnorm(n = 1, mean = 10 + 1*site + 0*tree.temp, sd = 2))

df$site <- as.factor(df$site)

readr::write_csv(df, "data/trees_bias.csv")

# summary(df)

library(ggplot2)
ggplot(df) +
  aes(tree.temp, tree.height, colour = site) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Temperature around tree", y = "Tree height")



## ----echo=FALSE-------------------------------------------------------------------------------------
library(glmmTMB)
m <- glmmTMB(tree.height ~ tree.temp + (1 |site), data = df)
summary(m)


## ----echo=TRUE--------------------------------------------------------------------------------------
visreg(m, xvar = "tree.temp", by = "site", overlay = T, band = F, legend = F)


## ----echo=FALSE-------------------------------------------------------------------------------------
m <- glmmTMB(tree.height ~ tree.temp + (1 + tree.temp |site), data = df)
summary(m)


## ----echo=TRUE--------------------------------------------------------------------------------------
visreg(m, xvar = "tree.temp", by = "site", overlay = T, band = F, legend = F)


## ----echo=TRUE--------------------------------------------------------------------------------------
performance::check_group_variation(m)


## ----echo=TRUE--------------------------------------------------------------------------------------
df <- datawizard::demean(df, select = "tree.temp", by = "site")
head(df)


## ----echo=1-----------------------------------------------------------------------------------------
m <- glmmTMB(tree.height ~ tree.temp_between + tree.temp_within + (1|site), data = df)
summary(m)


## ----echo=TRUE--------------------------------------------------------------------------------------
visreg(m, xvar = "tree.temp_between", by = "site")


## ----echo=TRUE--------------------------------------------------------------------------------------
visreg(m, xvar = "tree.temp_within", by = "site")


## ----eval = FALSE, echo=FALSE-----------------------------------------------------------------------
# 
# counts <- DHARMa::createData(sampleSize = 1000,
#                        intercept = 0,
#                        fixedEffects = 1,
#                        numGroups = 20,
#                        family = poisson(),
#                        overdispersion = 0.5)
# counts <- counts[,1:4]
# names(counts) <- c('ID', 'count', 'environment', 'species')
# readr::write_csv(counts, 'data/mixed_count.csv')
# 
# mod <- glmmTMB(count ~ environment + (1|species), data = counts, family = poisson)
# DHARMa::simulateResiduals(mod, plot = TRUE)
# DHARMa::testDispersion(mod)


## ----eval = FALSE, echo=FALSE-----------------------------------------------------------------------
# binom <- DHARMa::createData(sampleSize = 1000,
#                        intercept = 0,
#                        fixedEffects = 1,
#                        numGroups = 20,
#                        family = binomial())
# binom <- binom[,1:4]
# names(binom) <- c('ID', 'presabs', 'environment', 'species')
# readr::write_csv(binom, 'data/mixed_binom.csv')
# 
# mod <- glmmTMB(presabs ~ environment + (1|species), data = binom, family = binomial)
# DHARMa::simulateResiduals(mod, plot = TRUE)

