require(rjson)

#General notes from chris:
#Change variable 'matrix' name to something else like 'garlic.matrix' or something so it is more descriptive

#'@author hLi
#'@title matrix.from.garlic.scores
#'@param directory of garlic input files
#'@return matrix of scores for each cluster against the rest
matrix.from.garlic.scores <- function(directory){
  list.of.files <- list.files(path = directory, pattern = ".json", full.names=T, recursive=FALSE)
  matrix <- get.matrix.from.files(list.of.files)
  return(matrix)
}

#Removed a function here, was one line, may as well remove it

#Loop through the files and build a matrix
get.matrix.from.files <- function(list.of.files){
  matrix <- matrix()
  for(file in list.of.files){
    matrix <- add.to.matrix(file, matrix)
  }
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

#Chris notee A QUERY is added as a NEW row not each row, which it looks like what you do here, so it's fine. the description is just off
#add new query to each row and add max score to 
#query to query cell
add.query.to.rows <- function(query.name, matrix){
  #since matrix is populated with NA when it's initially set you can do a check to see if is.na(matrix[1,1])
  if(is.na(matrix[1,1]){ #start new matrix and set names I'd probably extract this if else into a function
    matrix = matrix(1)
    row.names(matrix)[1] <- query.name
    colnames(matrix)[1] <- query.name
  }else{ #append to existing matrix
    matrix <- rbind(matrix, rep(0,ncol(matrix)))
    matrix <- cbind(matrix, rep(0,nrow(matrix)))
    colnames(matrix)[-1] <- query.name
    row.names(matrix)[-1] <- query.name
    matrix[-1,-1] <- 1 #this -1 nomenclature means going backwards from the end, so this gives the bottom right most cell (ie self)
  }
  if(query.name[[1]] %in% colnames(matrix)){ #I'd probably extract out this if else statement to be a function
    a <- tail(row.names(matrix), n=1) #don't like this format, see above for better ways to get this cell / row
    b <- colnames(matrix)[which(colnames(matrix) == tail(row.names(matrix), n=1))] #see above comment
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
  if(subject.score[1] %in% colnames(matrix)){ #have some variables wrong here, name and score are passed separately
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
