require(rjson)
#'@author hLi
#'@title matrix.from.garlic.scores
#'@param directory of garlic input files
#'@return matrix of scores for each cluster against the rest

matrix.from.garlic.scores <- function(directory){
  list.of.files <- get.list.of.files(directory)
  matrix <- get.matrix.from.files(list.of.files)
  return(matrix)
}

#takes a directory and returns list of all files
get.list.of.files <- function(directory){
  list.of.files <- list.files(path = directory, pattern = ".json", full.names=T, recursive=FALSE)
  return(list.of.files)
}

#Loop through the files and build a matrix
get.matrix.from.files <- function(list.of.files){
  matrix <- matrix()
  for(file in list.of.files){
    matrix <- add.to.matrix(file, matrix)
  }
}

#gets the query name from a garlic.json object
get.name <- function(name){
  base.name <- name$file_name
  cluster.start <- name$cluster_start
  name <- paste(base.name, cluster.start, sep = "_")
  return(name)
}

#adds all the results for a single file to the matrix
#will add one row and 0-n columns
add.to.matrix <- function(file1, matrix){
  garlic.results <- fromJSON(file = file1)
  query <- garlic.results[[1]][[1]]$query
  query.name <- get.name(query)
  scores <- get.relative.scores(garlic.results[[1]]) #matrix with scores and subject names
  matrix <- add.scores.matrix(query.name, subject.scores, matrix)
  return(matrix)
}



#gets the relative scores for each subject in the json object
get.relative.scores <- function(garlic.results){
  subject.scores <- lapply(garlic.results[[1]], function(garlic.result){
    subject <- get.name(garlic.result$subject)
    score <- garlic.result$scores$relative_score
    return(c(subject, score))
  })
}

#add the relative score to matrix
add.scores.matrix <- function(query.name, subject.scores, matrix){
  #add row for query
  rownames(matrix) <- add.query.to.rows(query.name, matrix)
  for(i in 1:length(subject.scores)){
    subject.score <- subject.scores[[i]]
    matrix <- add.score.matrix(subject.score[1], subject.score[2], matrix)
  }
  return(matrix)
}

#add new query to each row and add max score to 
#query to query cell
add.query.to.rows <- function(query.name, matrix){
  query.name[[1]] <- as.vector(matrix(0, rep(nrow(matrix))))
  rbind(matrix, query.name[[1]])
  if(query.name[[1]] %in% colnames(matrix)){
    a <- tail(row.names(matrix), n=1)
    b <- colnames(matrix)[which(colnames(matrix) == tail(row.names(matrix), n=1))]
    matrix[a, b] = 1
  }
  else{
    query.name[[1]] <- as.vector(matrix(0, rep(nrow(matrix))))
    cbind(matrix, query.name[[1]])
    matrix[tail(row.names(matrix), n=1), tail(colnames(matrix), n=1)] = 1
  }
  return(matrix)
}

add.score.matrix <- function(name, score, matrix){
  #check if name exists as a colname, if so append to it
  if(subject.score[1] %in% colnames(matrix)){
    c <- colnames(matrix)[which(colnames(matrix) == subject.score[1])]
    matrix[a, c] = subject.score[2]
  }
  #if not add it
  else{
    z <- as.vector(matrix(0, nrow = 1, ncol = length(nrow(matrix))))
    colnames(z) <- subject.score[1]
    cbind(matrix, z)
    matrix[tail(row.names(matrix), n=1), tail(colnames(matrix), n=1)] = subject.score[2]
  } 
  return(matrix)
}
