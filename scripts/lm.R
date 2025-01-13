## ----knitr_setup, include=FALSE, cache=FALSE---------------------------------------------------------------------

library('knitr')

### Chunk options ###

## Text results
opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, size = 'tiny')

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



## ----------------------------------------------------------------------------------------------------------------
trees <- read.csv('data/trees.csv')
head(trees)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics('images/anscombe.png')


## ----indexplot---------------------------------------------------------------------------------------------------
plot(trees$height)


## ----out.width='70%', echo=FALSE---------------------------------------------------------------------------------
include_graphics('images/reg_outliers.png')


## ----histog------------------------------------------------------------------------------------------------------
hist(trees$height)


## ----------------------------------------------------------------------------------------------------------------
hist(trees$dbh)


## ----scatterplot-------------------------------------------------------------------------------------------------
plot(height ~ dbh, data = trees, las = 1)


## ----echo=3:4, tinycode = TRUE-----------------------------------------------------------------------------------
library(ggplot2)
theme_set(theme_minimal(base_size = 18))
ggplot(trees) +
  geom_point(aes(x = dbh, y = height)) 


## ----lm_trees----------------------------------------------------------------------------------------------------
m1 <- lm(height ~ dbh, data = trees)


## ----------------------------------------------------------------------------------------------------------------
library('equatiomatic')
m1 <- lm(height ~ dbh, data = trees)
equatiomatic::extract_eq(m1)


## ----------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(m1, use_coefs = TRUE)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# library(texPreview)
# tex_preview(equatiomatic::extract_eq(m1))


## ----summary_lm, echo=TRUE---------------------------------------------------------------------------------------
summary(m1)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics('images/gaussian.png')


## ----echo=FALSE, message=FALSE-----------------------------------------------------------------------------------
parameters::parameters(m1)[1,]


## ----echo=FALSE--------------------------------------------------------------------------------------------------
library(arm)
library(ggplot2)

coefs <- as.data.frame(coef(sim(m1)))
names(coefs) <- c('intercept', 'slope')

ggplot(coefs) +
  geom_density(aes(intercept), fill = 'grey80') +
  xlim(-1, 21) +
  geom_vline(xintercept = 0)


## ----echo=FALSE, message=FALSE-----------------------------------------------------------------------------------
parameters::parameters(m1)[2,]


## ----echo=FALSE--------------------------------------------------------------------------------------------------
ggplot(coefs) +
  geom_density(aes(slope), fill = 'grey80') +
  xlim(-0.1, 0.7) +
  geom_vline(xintercept = 0)


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# library(easystats)
# plot(simulate_parameters(m1)) +
#   labs(title = 'Density of the slope parameter')


## ----echo=FALSE--------------------------------------------------------------------------------------------------
res <- data.frame(residual = residuals(m1))
ggplot(res) +
  geom_histogram(aes(residual), fill = 'grey80') +
  geom_vline(xintercept = 0) +
  annotate('text', x = -10, y = 50, label = 'SD = 4', size = 7)


## ----------------------------------------------------------------------------------------------------------------
mean(trees$dbh)


## ----------------------------------------------------------------------------------------------------------------
trees$dbh.c <- trees$dbh - 30


## ----------------------------------------------------------------------------------------------------------------
summary(trees$dbh)
summary(trees$dbh.c)


## ----echo=c(1)---------------------------------------------------------------------------------------------------
m1.c <- lm(height ~ dbh.c, data = trees)
summary(m1.c)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
coef(m1)


## ----echo = TRUE-------------------------------------------------------------------------------------------------
confint(m1)


## ----------------------------------------------------------------------------------------------------------------
library('broom')
tidy(m1)


## ----------------------------------------------------------------------------------------------------------------
glance(m1)


## ----------------------------------------------------------------------------------------------------------------
library('easystats')   # parameters package
parameters(m1)


## ----message=FALSE-----------------------------------------------------------------------------------------------
library('effects')
summary(allEffects(m1))


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics('images/nature_significance.PNG')


## ----results='asis'----------------------------------------------------------------------------------------------
library('report')
report(m1)


## ----echo=TRUE, comment=NA---------------------------------------------------------------------------------------
report_table(m1, include_effectsize = FALSE)


## ----echo=TRUE, results='asis'-----------------------------------------------------------------------------------
library('xtable')
xtable(m1, digits = 2)


## ----echo=TRUE, results='asis'-----------------------------------------------------------------------------------
library('texreg')
texreg(m1, single.row = TRUE)


## ----------------------------------------------------------------------------------------------------------------
library('gtsummary')
tbl_regression(m1, intercept = TRUE) 


## ----results='asis'----------------------------------------------------------------------------------------------
library('modelsummary')
modelsummary(m1, output = 'markdown')  # Word, PDF, PowerPoint, png...


## ----results='asis'----------------------------------------------------------------------------------------------
modelsummary(m1, fmt = 2, 
             estimate = '{estimate} ({std.error})', 
             statistic = NULL,
             gof_map = c('nobs', 'r.squared', 'rmse'),
             output = 'markdown')  # Word, PDF, PowerPoint, png...


## ----echo = TRUE-------------------------------------------------------------------------------------------------
library('effects')
plot(allEffects(m1))


## ----visreg------------------------------------------------------------------------------------------------------
library('visreg')
visreg(m1)


## ----out.width='80%'---------------------------------------------------------------------------------------------
visreg(m1, gg = TRUE) + theme_bw()


## ----------------------------------------------------------------------------------------------------------------
library('easystats')
plot(estimate_expectation(m1))


## ----out.width='70%'---------------------------------------------------------------------------------------------
library('sjPlot')
plot_model(m1, type = 'eff', terms = 'dbh')


## ----------------------------------------------------------------------------------------------------------------
library('ggeffects')


## ----------------------------------------------------------------------------------------------------------------
mydf <- ggpredict(m1, terms = 'dbh')
dplyr::glimpse(mydf, width = 40)


## ----------------------------------------------------------------------------------------------------------------
ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.1)


## ----------------------------------------------------------------------------------------------------------------
modelplot(m1)


## ----out.width='80%'---------------------------------------------------------------------------------------------
library('easystats')
plot(parameters(m1), show_intercept = TRUE, show_labels = TRUE)


## ----out.width='70%'---------------------------------------------------------------------------------------------
plot(simulate_parameters(m1)) +
  labs(title = 'Density of the slope parameter')


## ----out.width='50%', echo = FALSE-------------------------------------------------------------------------------
include_graphics('images/lm_resid_assump.png')


## ----resid_hist, out.width='70%'---------------------------------------------------------------------------------
hist(residuals(m1))


## ----echo=FALSE--------------------------------------------------------------------------------------------------
def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m1)
par(def.par)


## ----out.width='80%'---------------------------------------------------------------------------------------------
library('easystats')
check_model(m1)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# library('easystats')
# model_dashboard(m1)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics("images/model_dashboard.png")


## ----out.width='60%'---------------------------------------------------------------------------------------------
library('cannonball')   # https://github.com/janhove/cannonball
lin_plot(parade(m1))


## ----message=TRUE------------------------------------------------------------------------------------------------
reveal(parade(m1))


## ----------------------------------------------------------------------------------------------------------------
trees$height.pred <- fitted(m1)
trees$resid <- residuals(m1)
head(trees)


## ----obs_pred, echo=FALSE----------------------------------------------------------------------------------------
plot(height ~ height.pred, data = trees,
     xlab = 'Tree height (predicted)', ylab = 'Tree height (observed)', 
     las = 1, xlim = c(10,60), ylim = c(10,60))
abline(a = 0, b = 1)


## ----------------------------------------------------------------------------------------------------------------
new.dbh <- data.frame(dbh = c(39))
predict(m1, new.dbh, se.fit = TRUE)


## ----------------------------------------------------------------------------------------------------------------
predict(m1, new.dbh, interval = 'confidence')


## ----------------------------------------------------------------------------------------------------------------
predict(m1, new.dbh, interval = 'prediction')


## ----echo=FALSE--------------------------------------------------------------------------------------------------
library('modelbased')
conf <- estimate_expectation(m1, data = NULL)
conf$obs <- trees$height

ggplot(conf) +
  aes(x = dbh, y = obs) +
  geom_point(size = 0.5, alpha = 0.3) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = 'blue', alpha = 0.2) +
  geom_line(aes(x = dbh, y = Predicted), colour = 'blue', size = 0.5) +
  labs(x = 'diameter', y = 'height', title = 'Confidence interval') +
  theme_minimal(base_size = 18)


## ----echo=FALSE--------------------------------------------------------------------------------------------------

pred <- estimate_prediction(m1)
pred$obs <- trees$height

ggplot(pred) +
  aes(x = dbh, y = obs) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = 'blue', alpha = 0.2) +
  geom_line(aes(x = dbh, y = Predicted), colour = 'blue', size = 2) +
  labs(x = 'diameter', y = 'height', title = 'Prediction interval') +
  theme_minimal(base_size = 18)


## ----echo=1------------------------------------------------------------------------------------------------------
pred <- estimate_expectation(m1)
head(pred)


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_expectation(m1))


## ----------------------------------------------------------------------------------------------------------------
pred$height.obs <- trees$height
plot(height.obs ~ Predicted, data = pred, xlim = c(15, 60), ylim = c(15, 60))
abline(a = 0, b = 1)


## ----------------------------------------------------------------------------------------------------------------
pred <- estimate_prediction(m1)
head(pred)


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_expectation(m1))


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_prediction(m1))


## ----------------------------------------------------------------------------------------------------------------
estimate_expectation(m1, data = data.frame(dbh = 39))


## ----------------------------------------------------------------------------------------------------------------
estimate_prediction(m1, data = data.frame(dbh = 39))


## ----------------------------------------------------------------------------------------------------------------
performance_cv(m1, method = 'k_fold', metrics = 'common', k = 10)


## ----boxplot-----------------------------------------------------------------------------------------------------
boxplot(height ~ sex, data = trees)


## ----echo=1------------------------------------------------------------------------------------------------------
m2 <- lm(height ~ sex, data = trees)
summary(m2)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# m2 <- lm(height ~ sex, data = trees)


## ----echo=1------------------------------------------------------------------------------------------------------
m2 <- lm(height ~ sex, data = trees)
summary(m2)


## ----results='asis'----------------------------------------------------------------------------------------------
report(m2)


## ----echo=FALSE, message=FALSE-----------------------------------------------------------------------------------
parameters(m2)[1,]


## ----echo=FALSE, out.width='65%'---------------------------------------------------------------------------------
coefs <- as.data.frame(coef(sim(m2)))
names(coefs) <- c('intercept', 'slope')

ggplot(coefs) +
  geom_density(aes(intercept), fill = 'grey80') +
  xlim(-1, 40) +
  geom_vline(xintercept = 0)


## ----echo=FALSE, message=FALSE-----------------------------------------------------------------------------------
parameters(m2)[2,]


## ----echo=FALSE, out.width = '65%'-------------------------------------------------------------------------------
ggplot(coefs) +
  geom_density(aes(slope), fill = 'grey80') +
  xlim(-3, 2) +
  geom_vline(xintercept = 0)


## ----out.width="100%", echo=FALSE, cache = TRUE------------------------------------------------------------------

set.seed(8)
xdist <- rnorm(1000, 70, 15)
ydist <- rnorm(1000, 50, 15)
dist <- data.frame(group = rep(c("A", "B"), each = 1000), 
                   value = c(xdist, ydist))
p1 <- ggplot(dist) +
  geom_density(aes(value, fill = group), alpha = 0.3) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Two groups have different distribution") 

xsamp <- sample(xdist, 10)
ysamp <- sample(ydist, 10)
samp <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = c(xsamp, ysamp))

tt <- t.test(xsamp, ysamp)

p2 <- ggplot(samp) +
  geom_histogram(aes(value, fill = group)) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Sampling 10 individuals from each group and making t-test",
       y = "count") +
  annotate("text", x = 75, y = 3, hjust = 0, size = 8,
           label = paste0("p = ", round(tt$p.value, 3)))

p1/p2



## ----echo=FALSE--------------------------------------------------------------------------------------------------
p1 <- ggplot(dist) +
  geom_violin(aes(group, value, fill = group), 
              alpha = 0.3, show.legend = FALSE) +
  theme_minimal(base_size = 15) +
  labs(title = "True distributions",
  subtitle = "Difference of means = 20", x = "", y = "") +
  coord_cartesian(ylim = c(0, 120))

p2 <- ggplot(samp) +
  geom_violin(aes(group, value, fill = group), 
              alpha = 0.3, show.legend = FALSE) +
  theme_minimal(base_size = 15) +
  labs(title = "Samples",
  subtitle = paste0("p = ", round(tt$p.value, 3)),
  y = "", x = "") +
  coord_cartesian(ylim = c(0, 120))

p1 + p2



## ----out.width="95%", echo=FALSE, cache = TRUE-------------------------------------------------------------------
library(ggplot2)
library(patchwork)

set.seed(8)
xdist <- rnorm(1000, 50, 15)
ydist <- rnorm(1000, 50, 15)
dist <- data.frame(group = rep(c("A", "B"), each = 1000), 
                   value = c(xdist, ydist))
p1 <- ggplot(dist) +
  geom_density(aes(value, fill = group), alpha = 0.3) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Two groups have same distribution") 

xsamp <- sample(xdist, 10)
ysamp <- sample(ydist, 10)
samp <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = c(xsamp, ysamp))

tt <- t.test(xsamp, ysamp)

p2 <- ggplot(samp) +
  geom_histogram(aes(value, fill = group)) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Sampling 10 individuals from each group and making t-test",
       y = "count") +
  annotate("text", x = 70, y = 3, hjust = 0, size = 8,
           label = paste0("p = ", round(tt$p.value, 3)))

p1/p2

# m <- lm(vals ~ samp, data = samp)
# summary(m)
# visreg::visreg(m)


## ----out.width="100%", echo=FALSE, cache = TRUE------------------------------------------------------------------

set.seed(8)
xdist <- rnorm(1000, 50, 15)
ydist <- rnorm(1000, 50, 15)
dist <- data.frame(group = rep(c("A", "B"), each = 1000), 
                   value = c(xdist, ydist))
p1 <- ggplot(dist) +
  geom_density(aes(value, fill = group), alpha = 0.3) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Two groups have same distribution") 

xsamp <- sample(xdist, 500)
ysamp <- sample(ydist, 500)
samp <- data.frame(group = rep(c("A", "B"), each = 500),
                   value = c(xsamp, ysamp))

tt <- t.test(xsamp, ysamp)

p2 <- ggplot(samp) +
  geom_histogram(aes(value, fill = group)) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Sampling 500 individuals from each group and making t-test",
       y = "count") +
  annotate("text", x = 75, y = 80, hjust = 0, size = 8,
           label = paste0("p = ", round(tt$p.value, 3)))

p1/p2



## ----echo=FALSE--------------------------------------------------------------------------------------------------
include_graphics("images/pvalue_SS_Motulski.jpg")


## ----out.height='2.5in', out.width='3.5in', echo=FALSE-----------------------------------------------------------
include_graphics("images/bigSS.png")


## ----out.width="40%", fig.align='left', echo=FALSE---------------------------------------------------------------
include_graphics("images/pvalue-effsize.png")


## ----out.width="100%", echo=FALSE--------------------------------------------------------------------------------
include_graphics("images/pvalue-effsize-A.jpeg")


## ----out.width="100%", echo=FALSE--------------------------------------------------------------------------------
include_graphics("images/pvalue-effsize-B.jpeg")


## ----echo=FALSE, cache = TRUE------------------------------------------------------------------------------------
set.seed(1)
x <- rnorm(10000,0,1)
y <- rnorm(10000,0,1) + x/44
plot(x,y,col=rgb(.5,0,0,.25),pch=19,cex=.5, main = "p = 0.005", cex.main = 3)
abline(lm(y~x),lwd=2)
#https://twitter.com/CaAl/status/908322681958920192?s=20


## ----------------------------------------------------------------------------------------------------------------
parameters(m2)[2,]


## ----------------------------------------------------------------------------------------------------------------
parameters(m2, s_value = TRUE)[2,]


## ----------------------------------------------------------------------------------------------------------------
library('easystats')  # modelbased package
estimate_means(m2)


## ----------------------------------------------------------------------------------------------------------------
estimate_contrasts(m2)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(m2))


## ----------------------------------------------------------------------------------------------------------------
visreg(m2)


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_means(m2))


## ----out.width='60%'---------------------------------------------------------------------------------------------
library('sjPlot')
plot_model(m2, type = 'eff', terms = 'sex')


## ----------------------------------------------------------------------------------------------------------------
hist(resid(m2))


## ----echo=FALSE--------------------------------------------------------------------------------------------------
def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m2)
par(def.par)


## ----------------------------------------------------------------------------------------------------------------
library('easystats')
check_model(m2)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# model_dashboard(m2)


## ----------------------------------------------------------------------------------------------------------------
plot(height ~ site, data = trees)


## ----------------------------------------------------------------------------------------------------------------
m3 <- lm(height ~ site, data = trees)


## ----echo=1------------------------------------------------------------------------------------------------------
m3 <- lm(height ~ site, data = trees)
summary(m3)


## ----------------------------------------------------------------------------------------------------------------
extract_eq(m3)


## ----------------------------------------------------------------------------------------------------------------
trees$site <- as.factor(trees$site)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# trees <- trees |>
#   dplyr::mutate(site = as.factor(site))


## ----------------------------------------------------------------------------------------------------------------
m3 <- lm(height ~ site, data = trees)

extract_eq(m3)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
m3 <- lm(height ~ site, data = trees)
summary(m3)


## ----------------------------------------------------------------------------------------------------------------
plot(simulate_parameters(m3), stack = FALSE)


## ----------------------------------------------------------------------------------------------------------------
estimate_means(m3)


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_means(m3))


## ----------------------------------------------------------------------------------------------------------------
estimate_contrasts(m3)


## ----------------------------------------------------------------------------------------------------------------
library('marginaleffects')
hypotheses(m3, 'site2 = site9')


## ----------------------------------------------------------------------------------------------------------------
parameters(m3)


## ----------------------------------------------------------------------------------------------------------------
estimate_means(m3)


## ----------------------------------------------------------------------------------------------------------------
modelsummary(m3, estimate  = '{estimate} ({std.error})', statistic = NULL, 
             fmt = 1, gof_map = NA, coef_rename = paste0('site', 1:10), output = 'markdown')


## ----------------------------------------------------------------------------------------------------------------
library('gtsummary')
tbl_regression(m3)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(m3))


## ----------------------------------------------------------------------------------------------------------------
visreg(m3)


## ----------------------------------------------------------------------------------------------------------------
plot(estimate_means(m3))


## ----out.width='70%'---------------------------------------------------------------------------------------------
plot_model(m3, type = 'eff', terms = 'site')


## ----out.width='70%'---------------------------------------------------------------------------------------------
modelplot(m3)


## ----out.width='70%'---------------------------------------------------------------------------------------------
plot(parameters(m3), show_intercept = TRUE)


## ----echo=1------------------------------------------------------------------------------------------------------
m3bis <- lm(height ~ site - 1, data = trees)
summary(m3bis)


## ----------------------------------------------------------------------------------------------------------------
plot(parameters(m3bis))


## ----echo=FALSE--------------------------------------------------------------------------------------------------
def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow = 2))
plot(m3)
par(def.par)


## ----------------------------------------------------------------------------------------------------------------
check_model(m3)


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# lm(height ~ site + dbh, data = trees)


## ----echo = FALSE------------------------------------------------------------------------------------------------
m4 <- lm(height ~ site + dbh, data = trees)
summary(m4)


## ----echo=TRUE---------------------------------------------------------------------------------------------------
parameters(m4)


## ----------------------------------------------------------------------------------------------------------------
estimate_means(m4)


## ----echo = 1----------------------------------------------------------------------------------------------------
m4.noint <- lm(height ~ -1 + site + dbh, data = trees)
summary(m4.noint)


## ----------------------------------------------------------------------------------------------------------------
plot(allEffects(m4))


## ----echo=2------------------------------------------------------------------------------------------------------
par(mfcol = c(1, 2))
visreg(m4)
dev.off()


## ----------------------------------------------------------------------------------------------------------------
visreg(m4, xvar = 'dbh', by = 'site', overlay = TRUE, band = FALSE)


## ----echo=TRUE, out.width='30%', eval=FALSE----------------------------------------------------------------------
# plot_model(m4, type = 'eff', terms = 'site')
# 


## ----------------------------------------------------------------------------------------------------------------
plot(parameters(m4))


## ----------------------------------------------------------------------------------------------------------------
plot(parameters(m4, drop = 'dbh'))


## ----------------------------------------------------------------------------------------------------------------
modelplot(m4)


## ----------------------------------------------------------------------------------------------------------------
modelplot(m4, coef_omit = 'dbh')


## ----------------------------------------------------------------------------------------------------------------
visreg(m3)


## ----------------------------------------------------------------------------------------------------------------
visreg(m4, xvar = 'site')


## ----echo = FALSE------------------------------------------------------------------------------------------------
ggplot(trees) +
  geom_boxplot(aes(site, dbh))


## ----------------------------------------------------------------------------------------------------------------
aggregate(trees$dbh ~ trees$site, FUN = mean)


## ----------------------------------------------------------------------------------------------------------------
aggregate(trees$height ~ trees$site, FUN = mean)


## ----echo=FALSE--------------------------------------------------------------------------------------------------
visreg(m4, xvar = 'dbh', by = 'site', overlay = TRUE, band = FALSE)


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# plot(height ~ dbh, data = trees, las = 1)
# abline(a = coef(m4)[1], b = coef(m4)[11])
# for (i in 2:10) {
#   abline(a = coef(m4)[1] + coef(m4)[i], b = coef(m4)[11])
# }


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------
# ## ggplot with different colour for each site
# ggplot(trees) +
#   aes(x = dbh, y = height, colour = site) +
#   geom_point() +
#   geom_abline(intercept = coef(m4)[1], slope = coef(m4)[11]) +
#   geom_abline(intercept = coef(m4)[1] + coef(m4)[2], slope = coef(m4)[11]) +
#   geom_abline(intercept = coef(m4)[1] + coef(m4)[3], slope = coef(m4)[11]) +
#   geom_abline(intercept = coef(m4)[1] + coef(m4)[8], slope = coef(m4)[11])


## ----------------------------------------------------------------------------------------------------------------
parameters(m4, keep = 'dbh')


## ----echo=FALSE--------------------------------------------------------------------------------------------------
def.par <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow=2))
plot(m4)
par(def.par)


## ----------------------------------------------------------------------------------------------------------------
check_model(m4)


## ----------------------------------------------------------------------------------------------------------------
trees$height.pred <- fitted(m4)
plot(trees$height.pred, trees$height, xlab = 'Tree height (predicted)', ylab = 'Tree height (observed)', las = 1, xlim = c(10,60), ylim = c(10,60))
abline(a = 0, b = 1)


## ----out.width='80%'---------------------------------------------------------------------------------------------
pred <- estimate_expectation(m4)
pred$obs <- trees$height
plot(obs ~ Predicted, data = pred, xlim = c(15, 60), ylim = c(15, 60))
abline(a = 0, b = 1)


## ----out.width='70%'---------------------------------------------------------------------------------------------
performance::check_predictions(m4)


## ----eval=FALSE, echo=FALSE--------------------------------------------------------------------------------------
# library(bayesplot)
# sims <- simulate(m4, nsim = 100)
# ppc_dens_overlay(trees$height, yrep = t(as.matrix(sims)))


## ----------------------------------------------------------------------------------------------------------------
trees.10cm <- data.frame(site = as.factor(1:10),
                        dbh = 10)
trees.10cm


## ----------------------------------------------------------------------------------------------------------------
predict(m4, newdata = trees.10cm, interval = 'confidence')


## ----------------------------------------------------------------------------------------------------------------
predict(m4, newdata = trees.10cm, interval = 'prediction')


## ----------------------------------------------------------------------------------------------------------------
predict(m4, newdata = trees.10cm, interval = 'prediction', 
        level = 0.99)


## ----------------------------------------------------------------------------------------------------------------
trees.10cm <- data.frame(site = as.factor(1:10),
                        dbh = 10)
trees.10cm


## ----echo=1------------------------------------------------------------------------------------------------------
pred <- estimate_expectation(m4, data = trees.10cm)
pred


## ----echo=1------------------------------------------------------------------------------------------------------
pred <- estimate_prediction(m4, data = trees.10cm)
pred


## ----echo=FALSE--------------------------------------------------------------------------------------------------
df <- data.frame(dbh = seq(10, 50, by = 1), 
                 height = seq(20, 60, by = 1))
ggplot(df) +
  aes(dbh, height) +
  geom_blank() +
  geom_abline(intercept = 25, slope = 0.6) +
  geom_abline(intercept = 40, slope = 0.1, colour = 'steelblue') +
  geom_abline(intercept = 50, slope = -0.3, colour = 'orangered')
  


## ----echo=FALSE--------------------------------------------------------------------------------------------------
m5 <- lm(height ~ site*dbh, data = trees)
summary(m5)


## ----------------------------------------------------------------------------------------------------------------
visreg(m5, xvar = 'dbh', by = 'site')


## ----------------------------------------------------------------------------------------------------------------
visreg(m5, xvar = 'dbh', by = 'site', overlay = TRUE, band = FALSE)


## ----------------------------------------------------------------------------------------------------------------
library('marginaleffects')
hypotheses(m5, '`site9:dbh` = `site10:dbh`')


## ----eval=FALSE--------------------------------------------------------------------------------------------------
# library('modelStudio')
# m5.explain <- DALEX::explain(
#   m5,
#   data = trees,
#   y = trees$height)
# modelStudio(m5.explain, viewer = 'browser')

