len_unique <- function(x) {
  #Length of unique elements in a vector
  return(length(unique(x)))
}

len_which <- function(x) {
  #Length of conditionally satisfied vector
  return (length(which(x)))
}
