getData = function(file){
  data = read.csv(paste("./data/", file, ".csv", sep = ""), sep = "\t", header = T)
  names(data) = c("id", "title", "point", "comment", "time", "author", "content")
  data$title = iconv(as.character(data$title), "latin1", "UTF-8")
  charPoint = as.character(data$point)
  data$point = as.integer(gsub(",", "", charPoint))
  charComment = as.character(data$comment)
  data$comment = as.integer(gsub(",", "", charComment))
  data$time = as.integer(data$time)
  data$author = iconv(as.character(data$author), "latin1", "UTF-8")
  data$content = iconv(as.character(data$content), "latin1", "UTF-8")
  
  return (data)
}

getDate = function(time){
  return (paste(substr(time, 1, 4), "-", substr(time, 5, 6), "-", 
    substr(time, 7, 8), sep = ""))
}
