rm(list=ls());gc();
library(igraph)


makeBiPartite = function(edgeList) {
  nodesSet1 = as.numeric(as.factor(unique(edgeList[,1])))
  nodesSet2 = as.numeric(as.factor(unique(edgeList[,2])))
  
  edgeList$S1 = as.numeric(as.factor(edgeList[,1]))
  edgeList$S2 = as.numeric(as.factor(edgeList[,2]))
  # first we give prefixes to the nodes to discern the two partition
  g <- graph.empty()
  g <- add.vertices(g,nv=length(nodesSet1),attr=list(name=paste0('A',nodesSet1),
                                                     type=rep(TRUE,length(nodesSet1))))
  g <- add.vertices(g,nv=length(nodesSet2),attr=list(name=paste0('B',nodesSet2),
                                                     type=rep(FALSE,length(nodesSet2))))
  
  # we need to turn edgeList into a vector (and using names instead of indexes)
  edgeListVec <- as.vector(t(as.matrix(data.frame(S1=paste0('A',edgeList$S1),
                                                  S2=paste0('B',edgeList$S2)))))
  g <- add.edges(g,edgeListVec)
  
  # check if is recognized as bipartite
  is.bipartite(g)
  
  # return
  g
}


randomize_bipartite = function(edgeList, boots=100) {
  for(i in 1:boots) {
    j1 = sample(nrow(edgeList),1)
    j2 = sample(nrow(edgeList),1)
    
    
    
    while(j1==j2){
      j2 = sample(nrow(edgeList),1)
    }
    
    V1A=edgeList[j1,1]
    V1B=edgeList[j1,2]
    V2A=edgeList[j2,1]
    V2B=edgeList[j2,2]
    
    areConnected = function(V1A, V1B, V2A, V2B, edgeList) {
      ## CHECK THAT V1A isn't already connected to V2B
      if(as.character(V2B) %in% as.character(edgeList[edgeList$S1==V1A,2])) {
        return(T)    
      }
      
      ## AND THAT V2B isn't already connected to V1B
      if (as.character(V1B) %in% as.character(edgeList[edgeList$S1==V2A,2])){
        return(T)
      }
      
      return(F)
    }
    
    
    if(!areConnected(V1A, V1B, V2A, V2B, edgeList)) {
      edgeList[j2,2] = V1B
      edgeList[j1,2] = V2B
    }
  }
  edgeList
}


makeRandomBipartite = function(N,k=4,l=4) {
  
  E=do.call(rbind,lapply(1:N, FUN=function(i){
    cbind(i,i:(i+(k-1)))
  }))
  E[E[,2]>N,2]=E[E[,2]>N,2]-N
  
  edgeList =data.frame(S1=paste0("A",E[,1]), S2=paste0("B",E[,2]))
  
  
  
  simplify(as.undirected(makeBiPartite(randomize_bipartite(edgeList, boots=1000))))
}



g = makeRandomBipartite(40, 3)

plot(g)
table(degree(g))

is.simple(g)


g1 = bipartite_projection(g, types=V(g)$type) $proj1
g2 = bipartite_projection(g, types=V(g)$type) $proj2

plot(g1)
plot(g2)

table(degree(g1))
table(degree(g2))

