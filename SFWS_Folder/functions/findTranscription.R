#script to extract carrying transcriptions of segments from TextGrids
#Author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au
#Last date updated: 20 June 2019

findTranscription = function(data = NULL, segNumber = NULL, tierLabel = NULL){
  finBegin = data$phonetic$t1[segNumber]
  finEnd = data$phonetic$t2[segNumber]
  
  allInds = which(data$word$t1 <= finBegin)
  
  uniqueIndex = allInds[length(allInds)]
  
  wordTranscription = paste(findSegments(data = data, wordNumber = uniqueIndex), collapse = ' ')
  
  return(wordTranscription)
}