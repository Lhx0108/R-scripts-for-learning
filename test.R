#This is a sample script to see how the functions are working from Prism.on.Prism.R

sample.file <- "pop_files/1.0_10001.gbk.json"

garlic.results <- fromJSON(file = sample.file)
mat <- matrix()
query <- garlic.results[[1]][[1]]$query
query.name <- get.name(query)
scores1 <- get.relative.scores(garlic.results[[1]]) #matrix with scores and subject names


##################MADE UP TEST
#Scores
mat <- add.scores.matrix(query.name, scores1, mat)


#Test matrix made
test.mat <- matrix(c(1,0.2,0.1,1), ncol =2)
row.names(test.mat) <- c("a", "b")
colnames(test.mat) <- c("a", "b")


#test name
test.name <- "c"

#test score
test.score <- 0.5

#add test.name and test.score
#Should print the matrix out with a new column, named c with c(0, 0.5)
add.score.matrix(test.name, test.score, test.mat)
