#ALEPlot does not seem to export Varimp
## R code for Example 1
## Load relevant packages
#install.packages("ALEPlot")
#https://cran.r-project.org/web/packages/ALEPlot/vignettes/AccumulatedLocalEffectPlot.pdf
library(ALEPlot)
library(nnet)
## Generate some data and fit a neural network supervised learning model
n = 5000
x1 <- runif(n, min = 0, max = 1)
x2 <- runif(n, min = 0, max = 1)
x3 <- runif(n, min = 0, max = 1)
x4 <- runif(n, min = 0, max = 1)
y = 4*
  x1 + 3.87*
  x2^2 + 2.97*
  exp(-5+10*
        x3)/(1+exp(-5+10*
                     x3))+ rnorm(n, 0, 1)
DAT <- data.frame(y, x1, x2, x3, x4)
nnet.DAT <- nnet(y~., data = DAT, linout = T, skip = F, size = 6,
                 decay = 0.1, maxit = 1000, trace = F)
## Define the predictive function
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,
                                                      type = "raw"))
## Calculate and plot the ALE main effects of x1, x2, x3, and x4
ALE.1 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 1, K = 500,
                NA.plot = TRUE)
ALE.2 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 2, K = 500,
                NA.plot = TRUE)
ALE.3 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 3, K = 500,
                NA.plot = TRUE)
ALE.4 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = 4, K = 500,
                NA.plot = TRUE)
## Calculate and plot the ALE second-order effects of {x1, x2} and {x1, x4}
ALE.12 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = c(1,2), K = 100,
                 NA.plot = TRUE)
ALE.14 = ALEPlot(DAT[,2:5], nnet.DAT, pred.fun = yhat, J = c(1,4), K = 100,
                 NA.plot = TRUE)

## Manually plot the ALE main effects on the same scale for easier comparison
## of the relative importance of the four predictor variables
par(mfrow = c(3,2))
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1",
     ylab="ALE_main_x1", xlim = c(0,1), ylim = c(-2,2), main = "(a)")
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x2",
     ylab="ALE_main_x2", xlim = c(0,1), ylim = c(-2,2), main = "(b)")
plot(ALE.3$x.values, ALE.3$f.values, type="l", xlab="x3",
     ylab="ALE_main_x3", xlim = c(0,1), ylim = c(-2,2), main = "(c)")
plot(ALE.4$x.values, ALE.4$f.values, type="l", xlab="x4",
     ylab="ALE_main_x4", xlim = c(0,1), ylim = c(-2,2), main = "(d)")
## Manually plot the ALE second-order effects of {x1, x2} and {x1, x4}
image(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, xlab = "x1",
      ylab = "x2", main = "(e)")
contour(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, add=TRUE,
        drawlabels=TRUE)
image(ALE.14$x.values[[1]], ALE.14$x.values[[2]], ALE.14$f.values, xlab = "x1",
      ylab = "x4", main = "(f)")
contour(ALE.14$x.values[[1]], ALE.14$x.values[[2]], ALE.14$f.values, add=TRUE,
        drawlabels=TRUE)

#####
## R code for Example 4
## Load relevant packages
library(ALEPlot)
library(nnet)
## Generate some data and fit a neural network supervised learning model
n = 200
x <- runif(n, min = 0, max = 1)
x1 <- x + rnorm(n, 0, 0.05)
x2 <- x + rnorm(n, 0, 0.05)
y = x1 + x2^2 + rnorm(n, 0, 0.1)
DAT = data.frame(y, x1, x2)
nnet.DAT <- nnet(y  ~ ., data = DAT, linout = T, skip = F, size = 10,
                 decay = 0.0001,  maxit = 1000, trace = F)
## Define the predictive function‘
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,
                                                      type= "raw"))
## Calculate and plot the ALE and PD main effects of x1 and x2
par(mfrow = c(2,2), mar = c(4,4,2,2) + 0.1)
ALE.1 = ALEPlot(DAT[,2:3], nnet.DAT, pred.fun = yhat, J = 1, K = 50,
                NA.plot = TRUE)
PD.1 = PDPlot(DAT[,2:3], nnet.DAT, pred.fun = yhat, J = 1, K = 50)
ALE.2 = ALEPlot(DAT[,2:3], nnet.DAT, pred.fun = yhat, J = 2, K = 50,
                NA.plot = TRUE)
PD.2 = PDPlot(DAT[,2:3], nnet.DAT, pred.fun = yhat, J = 2, K = 50)
## Manually plot the ALE main effects on the same scale for easier
## comparison of the relative importance of the four predictor variables
## We also plot the true linear and quadratic effects in black for reference
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1",
     ylab="ALE_main_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(a)")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(PD.1$x.values, PD.1$f.values, type="l", xlab="x2",
     ylab="PD_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(b)")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x3",
     ylab="ALE_main_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(c)")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)
plot(PD.2$x.values, PD.2$f.values, type="l", xlab="x4",
     ylab="PD_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(d)")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)
##Left plots are ALEPlots and right plots are PDPlots
