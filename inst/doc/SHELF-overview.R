## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----define v-----------------------------------------------------------------
v <- matrix(c(25, 30, 35, 30, 35, 50), nrow = 3, ncol = 2)

## ----show v, echo = F---------------------------------------------------------
v

## ----define p-----------------------------------------------------------------
p <- c(0.25, 0.5, 0.75)

## ----use myfit----------------------------------------------------------------
library(SHELF)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)

## ----show myfit---------------------------------------------------------------
names(myfit)

## ----show beta----------------------------------------------------------------
myfit$Beta

## ----show ssq-----------------------------------------------------------------
myfit$ssq

## ----plot-fitted-distributions, fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center", fig.cap = "The two fitted distributions and an equal-weighted linear pool."----
plotfit(myfit, lp = TRUE)

## ----define single set--------------------------------------------------------
v <-c(25, 30, 40)
p <-c(0.25, 0.5, 0.75)
consensus <- fitdist(vals = v, probs = p, lower = 0, upper = 100)

## ----plot-RIO, fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center", fig.cap = "The fitted consensus distribution, with the lower and upper 5\\% tail areas shown as feedback."----
plotfit(consensus, ql = 0.05, qu = 0.95, d = "beta")

## ----show feedback------------------------------------------------------------
feedback(consensus, quantiles = c(0.05, 0.95))

