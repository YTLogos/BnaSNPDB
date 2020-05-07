test <- as.matrix(extractsnp()[[1]])
t <- t(test)
stree <- nj(dist.gene(t))
p <- ggtree(stree, layout="circular", branch.length="none", size=0.1) + ggtitle("")+ theme_void()
p <- gheatmap(p, all.tree.info, offset = 2, width=0.1, colnames = FALSE, color=NULL) +
    scale_fill_manual(breaks=c("Winter", "Spring", "Unknown", "Semi-winter"), 
                      values=c("blue", "red", "black", "purple"))




library(future.apply)
plan(multiprocess)

t$eff <- future.apply::future_sapply((seq_along(1:nrow(t)),FUN = function(x){
  if(t[x,6]==0){
    t[x,6] <- paste(t[x,2],t[x,2],sep = "/")
  }else if (t[x,6]==1) {
    t[x,6] <- paste(t[x,2],t[x,3],sep = "/")
  }else{
    t[x,6] <- paste(t[x,3],t[x,3],sep = "/")
  }
})
