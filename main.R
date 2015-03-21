library(RCurl)
library(data.table)
library(rjson)
library(igraph)
library(Matrix)
library(plyr)
#
# get friends from vk api of current user
#
source("config.R")
getFriends <- function (uidList) {
  print(length(uidList))
  basic.data <- getURL(paste0("https://api.vk.com/method/execute.fastFriends?access_token=", accessToken, "&v=5.28&targets=",
                              paste(compact(uidList), collapse=",")), ssl.verifypeer = FALSE)
  basic.data <- fromJSON(basic.data)$response
  return(basic.data)
}

#
# replace user vk id -> user local id
#

replacebyHM <- function(hm, vec){
  return(hm[hm$uid %in% vec]$id)
}

#
# list of vk ids
#
botids <- list()
botids[[1]] <- "68174509"
botids[[2]] <- "257615752"
#botids[[3]] <- "147162672"
#botids[[4]] <- "12876"


#
# get friends of current bots
#

friendList <- list()
for (i in 1:ceiling(length(botids)/25)) {
  lower = (i - 1) * 25
  upper = lower + 25
  friendResult <- getFriends(botids[lower:upper])
  for(j in 1:length(friendResult)) {
    friendList[lower + j] = friendResult[j]
  }
  print(i)
}

# globalList - linearized friends list
# globalHM - table: id - user local id;
#                   uid - user vk id
# globalFriends - list of friends from globalList

globalList <- as.character(sort(c(as.integer(botids), unlist(friendList))))
globalHM <- data.table(id=c(1:length(globalList)), uid=globalList)
globalFriends <- list(id=c(1:length(globalList)),
                      friends=rep(list(), length(globalList)))

#
## generating friends list for globalList
#

for (i in 0:(ceiling(length(globalList)/25) - 1)){
  lower = i * 25
  friends <- getFriends(globalList[lower:(lower + 24)])
  if (length(friends) != 0)
    for(j in 1:length(friends)) {
      globalFriends$friends[(lower + j)] <- list(replacebyHM(globalHM, friends[j][[1]]))
    }
  print(i)
}

#
# build adjacency matrix
#
graphList <- list()
for (k in 1:length(botids)){
  # all pairs of of ids in current friendList
  upairs <- combn(replacebyHM(globalHM, friendList[[k]]), 2)
  pairList <- globalFriends$friends[upairs]
  tupair <- data.table(t(upairs))
  setnames(tupair, c("user1", "user2"))
  tupair$mutual <- rep(0, nrow(tupair))
  f1 <- seq(1, 2 * ncol(upairs), 2)
  f2 <- seq(2, 2 * ncol(upairs), 2)
  mutual <- sapply(c(1:nrow(tupair)), function(x){
    res <- length(intersect(pairList[[f1[[x]]]], pairList[[f1[[x]]]]))
    return(res)
  })
  tupair$mutual <- mutual
  m <- sparseMatrix(i=tupair$user1,
                    j=tupair$user2,
                    x = tupair$mutual,
                    dims = c(length(globalList), length(globalList)))
  # select subgraph of friends
  mind <- replacebyHM(globalHM, friendList[[k]])
  m <- m[mind, mind]
  g <- graph.adjacency(as.matrix(m), mode = "undirected")
  # delete isolated vertices
  g <- delete.vertices(g, which(!as.logical(degree(g))))
  #write.graph(g, file = paste0("res/", botids[[k]], ".gml"), format = "gml")
  graphList[[k]] <- g
}