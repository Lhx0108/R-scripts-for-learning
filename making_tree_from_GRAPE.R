#Convert GRAPE on GRAPE to a tree

GRAPE.GRAPE <- read.csv(file = "~/git/R-scripts-for-learning/Antifungal_relative.csv")
d <- dist(as.matrix(GRAPE.GRAPE))
hc <- hclust(d, method = "single")
hc[["labels"]] <- GRAPE.GRAPE[ ,1]
library(ctc)
write.table(hc2Newick(hc), file="~/git/R-scripts-for-learning/hc.newick",row.names=FALSE,col.names=FALSE, quote = FALSE)