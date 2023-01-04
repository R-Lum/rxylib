## ---- echo = FALSE, results = "hide", message = FALSE-------------------------
require("emmeans")
knitr::opts_chunk$set(fig.width = 4.5, class.output = "ro")

## ---- echo = FALSE------------------------------------------------------------
par(mar = .1 + c(4, 4, 1, 1))   # reduce head space


## ----fig.alt = "Standard-graphics interaction plot showing cell means. Trends are non-parallel"----
with(pigs, interaction.plot(percent, source, conc))

## -----------------------------------------------------------------------------
with(pigs, tapply(conc, percent, mean))

## -----------------------------------------------------------------------------
cell.means <- matrix(with(pigs, 
    tapply(conc, interaction(source, percent), mean)), 
    nrow = 3)
cell.means

## -----------------------------------------------------------------------------
apply(cell.means, 2, mean)

## -----------------------------------------------------------------------------
with(pigs, table(source, percent))

## -----------------------------------------------------------------------------
sum(c(3, 1, 1) * cell.means[, 4]) / 5

## -----------------------------------------------------------------------------
pigs.lm1 <- lm(log(conc) ~ source + factor(percent), data = pigs)

## -----------------------------------------------------------------------------
ref_grid(pigs.lm1)

## -----------------------------------------------------------------------------
ref_grid(pigs.lm1) @ grid

## -----------------------------------------------------------------------------
pigs.lm2 <- lm(log(conc) ~ source + percent, data = pigs)
ref_grid(pigs.lm2)

## -----------------------------------------------------------------------------
ref_grid(pigs.lm2) @ grid

## -----------------------------------------------------------------------------
pigs.pred1 <- matrix(predict(ref_grid(pigs.lm1)), nrow = 3)
pigs.pred1

## -----------------------------------------------------------------------------
apply(pigs.pred1, 1, mean) ### EMMs for source

apply(pigs.pred1, 2, mean) ### EMMs for percent

## -----------------------------------------------------------------------------
predict(ref_grid(pigs.lm2))

## -----------------------------------------------------------------------------
emmeans(pigs.lm1, "percent")

## -----------------------------------------------------------------------------
ref_grid(pigs.lm2, cov.keep = "percent")

## -----------------------------------------------------------------------------
ref_grid(pigs.lm2, cov.reduce = range)

## -----------------------------------------------------------------------------
mtcars.lm <- lm(mpg ~ disp * cyl, data = mtcars)
ref_grid(mtcars.lm)

## -----------------------------------------------------------------------------
mtcars.rg <- ref_grid(mtcars.lm, cov.keep = 3,
                      at = list(disp = c(100, 200, 300)))
mtcars.rg

## -----------------------------------------------------------------------------
mtcars.1 <- lm(mpg ~ factor(cyl) + disp + I(disp^2), data = mtcars)
emmeans(mtcars.1, "cyl")

## -----------------------------------------------------------------------------
mtcars <- transform(mtcars, 
                    Cyl = factor(cyl),
                    dispsq = disp^2)
mtcars.2 <- lm(mpg ~ Cyl + disp + dispsq, data = mtcars)
emmeans(mtcars.2, "Cyl")

## -----------------------------------------------------------------------------
ref_grid(mtcars.1)
ref_grid(mtcars.2)

## -----------------------------------------------------------------------------
emmeans(mtcars.2, "Cyl", at = list(dispsq = 230.72^2))

## ---- eval = FALSE------------------------------------------------------------
#  deg <- 2
#  mod <- lm(y ~ treat * poly(x, degree = deg), data = mydata)

## ---- eval = FALSE------------------------------------------------------------
#  emmeans(mod, ~ treat | x, at = list(x = 1:3), params = "deg")

## ----fig.alt = c("emmip plot of pigs.lm1, showing parallel piecewise-linear trends","emmip plot of pigs.lm2, showing parallel linear trends")----
emmip(pigs.lm1, source ~ percent)
emmip(ref_grid(pigs.lm2, cov.reduce = FALSE), source ~ percent)

## ----fig.alt = "Plot of side-by-side confidence intervals for cyl means, in 3 panels for disp = 100, 200, 300"----
plot(mtcars.rg, by = "disp")

## -----------------------------------------------------------------------------
mtcars.rg_d.c <- ref_grid(mtcars.lm, at = list(cyl = c(4,6,8)),
                          cov.reduce = disp ~ cyl)
mtcars.rg_d.c @ grid

## ----fig.height = 1.5, fig.alt = "Side-by-side CIs for cyl marginal means"----
plot(mtcars.rg_d.c)

## ----fig.alt = "Enhanced interaction plot with CIs and observed data added; we have separate panels for the 3 diets, and the 4 percent conentrations in each panel"----
require("ggplot2")
emmip(pigs.lm1, ~ percent | source, CIs = TRUE) +
    geom_point(aes(x = percent, y = log(conc)), data = pigs, pch = 2, color = "blue")

## ---- eval = FALSE------------------------------------------------------------
#  ci <- confint(mtcars.rg_d.c, level = 0.90, adjust = "scheffe")
#  xport <- print(ci, export = TRUE)
#  cat("<font color = 'blue'>\n")
#  knitr::kable(xport$summary, align = "r")
#  for (a in xport$annotations) cat(paste(a, "<br>"))
#  cat("</font>\n")

## ---- results = "asis", echo = FALSE------------------------------------------
ci <- confint(mtcars.rg_d.c, level = 0.90, adjust = "scheffe")
xport <- print(ci, export = TRUE)
cat("<font color = 'blue'>\n")
knitr::kable(xport$summary, align = "r")
for (a in xport$annotations) cat(paste(a, "<br>"))
cat("</font>\n")

## -----------------------------------------------------------------------------
emmeans(pigs.lm1, "percent", weights = "cells")

## -----------------------------------------------------------------------------
pigs.lm3 <- lm(log(conc) ~ factor(percent), data = pigs)
emmeans(pigs.lm3, "percent")

## -----------------------------------------------------------------------------
MOats.lm <- lm (yield ~ Block + Variety, data = MOats)
ref_grid (MOats.lm, mult.name = "nitro")

## -----------------------------------------------------------------------------
pigs.rg <- ref_grid(pigs.lm1)
class(pigs.rg)

pigs.emm.s <- emmeans(pigs.rg, "source")
class(pigs.emm.s)

## -----------------------------------------------------------------------------
pigs.rg

pigs.emm.s

## -----------------------------------------------------------------------------
str(pigs.emm.s)

## -----------------------------------------------------------------------------
# equivalent to summary(emmeans(pigs.lm1, "percent"), level = 0.90, infer = TRUE))
emmeans(pigs.lm1, "percent", level = 0.90, infer = TRUE)

## -----------------------------------------------------------------------------
class(summary(pigs.emm.s))

