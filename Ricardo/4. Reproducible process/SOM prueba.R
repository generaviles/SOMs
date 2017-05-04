#SOM example using wines data set
library(kohonen)
data(wines)
set.seed(7)

#create SOM grid
sommap <- som(scale(wines),
              grid = somgrid(4, 4, "hexagonal"),
              rlen = 100,
              alpha = c(0.05, 0.01),
              keep.data = TRUE)

## use hierarchical clustering to cluster the codebook vectors
groups<-3
codes <- matrix(unlist(sommap$codes), ncol = 13, byrow = FALSE)
colnames(codes) <- c("alcohol", "malic acid", "ash", "ash alkalinity", "magnesium", "tot.phenols", "flavonoids", "non-flav. phenols", "proanth", "col.int.", "col.hue", "OD ratio", "proline")
rownames(codes) <- c("V1", "V2", "V3", "V4")

som.hc <- cutree(hclust(dist(codes)), groups)

#plot
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(sommap, som.hc)