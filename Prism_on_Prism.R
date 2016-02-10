require(rjson)
#'@author cDejong
#'@title matrix.from.garlic.scores
#'@param directory of garlic input files
#'@return matrix of scores for each cluster against the rest

matrix.from.garlic.scores <- function(directory){
  list.of.files <- get.list.of.files(directory)
  garlic.matrix <- get.matrix.from.files(list.of.files)
  return(garlic.matrix)
}

#takes a directory and returns list of all files
get.list.of.files <- function(directory){
  list.of.files <- list.files(path = directory, pattern = ".json", full.names=T, recursive=FALSE)
  return(list.of.files)
}

#Loop through the files and build a matrix
get.matrix.from.files <- function(list.of.files){
  garlic.matrix <- matrix()
  for(file in list.of.files){
    garlic.matrix <- add.to.matrix(file, garlic.matrix)
  }
  return(garlic.matrix)
}


#adds all the results for a single file to the matrix
#will add one row and 0-n columns
add.to.matrix <- function(file1, garlic.matrix){
  garlic.results <- fromJSON(file = file1)
  query <- garlic.results[[1]][[1]]$query
  query.name <- get.name(query)
  subject.scores <- get.relative.scores(garlic.results) #matrix with scores and subject names
  garlic.matrix <- add.scores.matrix(query.name, subject.scores, garlic.matrix)
  return(garlic.matrix)
}

#gets the query name from a garlic.json object
get.name <- function(name){
  base.name <- name$file_name
  cluster.start <- name$cluster_start
  name <- paste(base.name, cluster.start, sep = "_")
  return(name)
}


#gets the relative scores for each subject in the json object
get.relative.scores <- function(garlic.results){
  subject.scores <- lapply(garlic.results[[1]], function(garlic.result){
    subject.name <- get.name(garlic.result$subject)
    relative.score <- garlic.result$scores$relative_score
    return(c(subject.name, relative.score))
  })
  return(subject.scores)
}

#add the relative score to matrix
add.scores.matrix <- function(query.name, subject.scores, garlic.matrix){
  #add row for query
  garlic.matrix <- add.query.to.rows(query.name, garlic.matrix)
  for(i in 1:length(subject.scores)){
    subject.score <- subject.scores[[i]]
    subject <- subject.score[1]
    score <- subject.score[2]
    garlic.matrix <- add.score.matrix(subject, score, garlic.matrix)
  }
  return(garlic.matrix)
}

#add query names to each new row and add max score to 
#query to query cell
add.query.to.rows <- function(query.name, garlic.matrix){
  if(is.na(garlic.matrix[1,1])){
    garlic.matrix = matrix(1)
    row.names(garlic.matrix)[1] <- query.name
    colnames(garlic.matrix)[1] <- query.name
  }else{
    garlic.matrix <- rbind(garlic.matrix, rep(0, ncol(garlic.matrix)))
    row.names(garlic.matrix)[nrow(garlic.matrix)] <- query.name
    garlic.matrix <- add.query.to.cols.extention(query.name, garlic.matrix)
  }
  return(garlic.matrix)
}

add.query.to.cols.extention <- function(query.name, garlic.matrix){
  if(query.name %in% colnames(garlic.matrix)){
    garlic.matrix[nrow(garlic.matrix), which(colnames(garlic.matrix) == row.names(garlic.matrix)[nrow(garlic.matrix)])] <- 1
  } else{
    garlic.matrix <- cbind(garlic.matrix, rep(0,nrow(garlic.matrix)))
    colnames(garlic.matrix)[ncol(garlic.matrix)] <- query.name
    garlic.matrix[nrow(garlic.matrix), ncol(garlic.matrix)] <- 1
  }
  return(garlic.matrix)
}

add.score.matrix <- function(subject, score, garlic.matrix){
  #check if name exists as a colname, if so append to it
  if(subject %in% colnames(garlic.matrix)){
    c <- which(colnames(garlic.matrix) == subject)
    garlic.matrix[nrow(garlic.matrix), c] <- score
  }
  #if not add it
  else{
    garlic.matrix <- cbind(garlic.matrix, rep(0, nrow(garlic.matrix)))
    colnames(garlic.matrix)[ncol(garlic.matrix)] <- subject
    garlic.matrix[nrow(garlic.matrix),ncol(garlic.matrix)] <- score
  }
  return(garlic.matrix)
}
