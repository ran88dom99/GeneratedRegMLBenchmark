###################################################
### code chunk number 2: recommenderlab.Rnw:238-239
###################################################
citation("recommenderlab")

###################################################
### code chunk number 3: recommenderlab.Rnw:1064-1065
###################################################
library("recommenderlab")
ratingmat0 <- as.matrix(training)
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings
rm(ratingmat0,sparse_ratings)
gc()
getRatingMatrix(real_ratings)
#rRM <- as(training,"matrix" )#"realRatingMatrix"
#rRM
#getRatingMatrix(rPM)
###################################################
### code chunk number 30: recommenderlab.Rnw:1385-1388
###################################################
e <- evaluationScheme(real_ratings, method="cross-validation", k=3, given=-1)
e


###################################################
### code chunk number 31: recommenderlab.Rnw:1394-1399
###################################################
r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2


###################################################
### code chunk number 32: recommenderlab.Rnw:1406-1410
###################################################
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


###################################################
### code chunk number 33: recommenderlab.Rnw:1416-1421
###################################################
error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error
recommenderRegistry$get_entry_names()
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
