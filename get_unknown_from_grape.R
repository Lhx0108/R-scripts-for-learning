require(rjson)
#'@author cDejong
#'@title matrix.from.garlic.scores
#'@param directory of garlic input files
#'@return matrix of scores for each cluster against the rest

unknowns.from.grape <- function(directory){
  list.of.files <- get.list.of.files(directory)
  unknown.matrix <- get.unknowns.from.files(list.of.files)
  return(unknown.matrix)
  
}

#takes a directory and returns list of all files
get.list.of.files <- function(directory){
  list.of.files <- list.files(path = directory, pattern = ".json", full.names=T, recursive=FALSE)
  return(list.of.files)
}

#Loop through the files and build a matrix
get.unknowns.from.files <- function(list.of.files){
  unknown.matrix <- matrix()
  for(file in list.of.files){
    unknown.matrix <- add.to.matrix(file, unknown.matrix)
  }
  return(unknown.matrix)
}


#adds all the results for a single file to the matrix
#will add one row and 0-n columns
add.to.matrix <- function(file1, unknown.matrix){
  unknown.results <- fromJSON(file = file1)
  for(i in 1:length(unknown.results[[1]]$fragments))
    current.query <- unknown.results[[1]]$fragments[[i]]
  for(j in 1:length(current.query))
    subquery <- current.query[[j]]
  if(is.character(subquery$smiles) & !is.null(subquery$smiles) & subquery$type == "UNKNOWN_OTHER"){
    unknown.matrix <- rbind(unknown.matrix, subquery$smiles)
  } 
  else{
    for(h in 1: length(subquery)) 
    sub.subquery <- subquery[[h]]
    if(is.list(sub.subquery)){
      unknown.matrix <- get.unknown.step.2(sub.subquery, unknown.matrix)
    }
  }
  return(unknown.matrix)
}
    get.unknown.step.2 <- function(sub.subquery, unknown.matrix){
    if(is.character(sub.subquery$smiles) & !is.null(sub.subquery$smiles) & sub.subquery$type == "UNKNOWN_OTHER"){
    unknown.matrix <- rbind(unknown.matrix, sub.subquery$smiles)}
    }
    
    
    