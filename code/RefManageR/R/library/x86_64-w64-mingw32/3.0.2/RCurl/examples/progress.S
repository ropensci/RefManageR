library(RCurl)
getURL("http://www.omegahat.org/RCurl/index.html",  progressfunction = function(down, up) {print(down); print(up)}, noprogress = FALSE)
