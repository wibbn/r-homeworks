download.file('https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt', destfile = 'data/practice8/yurchenkov.txt')
fileName <- "data/practice8/yurchenkov.txt"
f <- file(fileName, open="r")

yurch <- readLines(f)
close(f)

yurch <- unlist(strsplit(yurch, "\n"))
yurch <- na.omit(yurch)

fileName <- "cleanedData.txt"
f <- file(fileName, open="w")

writeLines(yurch, f)

yurch <- read.csv(file=fileName, sep="\t", dec=",", header=F, col.names=c("Time", "V1", "V2", "V3", "V4", "V5"))

close(f)

exps <- which(yurch[,1] == 0)
stages <- length(exps)
timeExp <- yurch[exps[-1] - 1, 1]

table(cut(x=yurch[,1], breaks=seq(1, length(yurch),
                                  (length(yurch) - 1) / ncol(yurch))))

library(matrixStats)
colMeans(as.matrix(yurch))
colSums(as.matrix(yurch))
colVars(as.matrix(yurch))
colSds(as.matrix(yurch))
colMedians(as.matrix(yurch))

library("ggplot2")
ggplot(yurch[1:100,], aes(Time, V1)) + geom_point(size=1) + geom_area(fill="black")
ggplot(yurch[1:100,], aes(Time, V2)) + geom_point(size=1.5) + geom_area(fill="green")
ggplot(yurch[1:100,], aes(Time, V3)) + geom_point(size=1) + geom_area(fill="blue")
ggplot(yurch[1:100,], aes(Time, V4)) + geom_point(size=1.5) + geom_area(fill="orange")
ggplot(yurch[1:100,], aes(Time, V5)) + geom_point(size=1) + geom_area(fill="red")

rm(yurch, f, fileName, timeExp, stages, exps)