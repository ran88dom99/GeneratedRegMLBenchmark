#https://cran.r-project.org/web/packages/pdp/README.html
##CAN BE VERY SLOW BUT GRID RESOLUTION CAN BE SET
# Fit a random forest to the Boston housing data
library(randomForest)  # install.packages("randomForest")
library(pdp)
data(boston, package = "pdp")  # load the (corrected) Boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston, importance = TRUE)
library(randomForest)  # for randomForest, partialPlot, and varImpPlot functions
varImpPlot(boston.rf)  # Figure 1

# Partial dependence of cmedv on lstat and rm
pd <- partial(boston.rf, pred.var = c("lstat", "rm"), chull = TRUE)
head(pd)  # print first 6 rows
#>     lstat      rm     yhat
#> 1  7.5284 3.66538 24.13683
#> 2  8.2532 3.66538 23.24916
#> 3  8.9780 3.66538 23.13119
#> 4  9.7028 3.66538 22.13531
#> 5 10.4276 3.66538 20.62331
#> 6 11.1524 3.66538 20.51258

# Lattice version
p1 <- plotPartial(pd, main = "lattice version")

# ggplot2 version
library(ggplot2)
p2 <- autoplot(pd, contour = TRUE, main = "ggplot2 version", 
               legend.title = "Partial\ndependence")

# Show both plots in one figure
grid.arrange(p1, p2, ncol = 2)
#####
# Figure 2 (right)
boston.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "lstat") %>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(lstat)))

# Compute partial dependence data for lstat and rm
pd <- partial(boston.rf, pred.var = c("lstat", "rm"))
# Default PDP
pdp1 <- plotPartial(pd)
# Add contour lines and use a different color palette
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)
# 3-D surface
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
                    colorkey = TRUE, screen = list(z = -20, x = -60))
# Figure 3
grid.arrange(pdp1, pdp2, pdp3, ncol = 3)

# Figure 5
grid.arrange(
  partial(boston.rf, "rm", plot = TRUE),
  partial(boston.rf, "rm", grid.resolution = 30, plot = TRUE),
  partial(boston.rf, "rm", pred.grid = data.frame(rm = 3:9), plot = TRUE),
  ncol = 3
)

#####
pred.ice <- function(object, newdata) predict(object, newdata)
rm.ice <- partial(boston.rf, pred.var = "rm", pred.fun = pred.ice)
# Figure 9
plotPartial(rm.ice, rug = TRUE, train = boston, alpha = 0.3)

# Post-process rm.ice to obtain c-ICE curves
library(dplyr)  # for group_by and mutate functions
rm.ice <- rm.ice %>%
  group_by(yhat.id) %>%  # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))  # so each curve starts at yhat = 0


# ICE curves with their average
p1 <- ggplot(rm.ice, aes(rm, yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)
# c-ICE curves with their average
p2 <- ggplot(rm.ice, aes(rm, yhat.centered)) +
  geom_line(aes(group = yhat.id), alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)
# Figure 10
grid.arrange(p1, p2, ncol = 2)
#####
ozone <- read.csv(paste0("http://statweb.stanford.edu/~tibs/ElemStatLearn/",
                         "datasets/LAozone.data"), header = TRUE)
library(earth)  # for earth function (i.e., MARS algorithm)
ozone.mars <- earth(ozone ~ ., data = ozone, degree = 3)
partial(ozone.mars, pred.var = c("wind", "temp", "dpg"), plot = TRUE,
        chull = TRUE, parallel = TRUE, paropts = list(.packages = "earth"))  # Figure 6
# Add a label to the colorkey
lattice::trellis.focus("legend", side = "right", clipp.off = TRUE, highlight = FALSE)
grid::grid.text("ozone", x = 0.2, y = 1.05, hjust = 0.5, vjust = 1)
lattice::trellis.unfocus()
#####
library(e1071)  # for svm function
iris.svm <- svm(Species ~ ., data = iris, kernel = "radial", gamma = 0.75,
                cost = 0.25, probability = TRUE)


pd <- NULL
for (i in 1:3) {
  tmp <- partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
                 which.class = i, grid.resolution = 101, progress = "text")
  pd <- rbind(pd, cbind(tmp, Species = levels(iris$Species)[i]))
}
# Figure 7
library(ggplot2)
ggplot(pd, aes(x = Petal.Width, y = Petal.Length, z = yhat, fill = yhat)) +
  geom_tile() +
  geom_contour(color = "white", alpha = 0.5) +
  scale_fill_distiller(name = "Centered\nlogit", palette = "Spectral") +
  theme_bw() +
  facet_grid(~ Species)

pred.prob <- function(object, newdata) {  # see ?predict.svm
  pred <- predict(object, newdata, probability = TRUE)
  prob.setosa <- attr(pred, which = "probabilities")[, "setosa"]
  mean(prob.setosa)
}

# PDPs for Petal.Width and Petal.Length on the probability scale
pdp.pw <- partial(iris.svm, pred.var = "Petal.Width", pred.fun = pred.prob,
                  plot = TRUE)
pdp.pl <- partial(iris.svm, pred.var = "Petal.Length", pred.fun = pred.prob,
                  plot = TRUE)
pdp.pw.pl <- partial(iris.svm, pred.var = c("Petal.Width", "Petal.Length"),
                     pred.fun = pred.prob, plot = TRUE)
# Figure 8
grid.arrange(pdp.pw, pdp.pl, pdp.pw.pl, ncol = 3)
