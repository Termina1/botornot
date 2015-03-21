library(RCurl)
library(data.table)
library(rjson)
library(igraph)
library(Matrix)
#
# get friends from vk api of current user
#
getFriends <- function (uid) {
  basic.data <- getURL(paste0("api.vk.com/method/friends.get?user_id=",
                              uid), ssl.verifypeer = FALSE)
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
botids[[3]] <- "147162672"
botids[[4]] <- "12876"


#
# get friends of current bots
#

friendList <- list()
for (i in 1:length(botids)){
  friendList[[i]] <- getFriends(botids[[i]])
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

for (i in 1:length(globalList)){
  friends <- getFriends(globalList[i])
  if (length(friends) != 0)
    globalFriends$friends[i] <- list(replacebyHM(globalHM, friends))
  print(i)
}

#
# build adjacency matrix
#
graphList <- list()
for (k in 1:length(botids)){
  for (i in 1:length(friendList[[k]])){
    # all pairs of of ids in current friendList
    upairs <- combn(replacebyHM(globalHM, friendList[[k]]), 2)
    # adj matrix
    m <- matrix(data = 0,
                ncol=length(globalList),
                nrow=length(globalList))
    for (j in 1:ncol(upairs)){
      curpair <- sort(upairs[,j])
      friends <- globalFriends$friends[curpair]
      m[curpair[1], curpair[2]] <- length(intersect(friends[[1]], friends[[2]]))
    }
    print(i)
    # undirected graph => 
    m <- forceSymmetric(m)
    # select subgraph of friends
    mind <- replacebyHM(globalHM, friendList[[k]])
    m <- m[mind, mind]
    g <- graph.adjacency(as.matrix(m), mode = "undirected")
    # delete isolated vertices
    g <- delete.vertices(g, which(!as.logical(degree(g))))
    #write.graph(g, file = paste0("res/", botids[[k]], ".gml"), format = "gml")
  }
  graphList[[k]] <- g
}