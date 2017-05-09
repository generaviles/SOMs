#EXTRAYENDO LOS DATOS

#Table de sum_cluster y obtencion de datos
table(som_cluster)

cluster3 <- som_cluster[som_cluster==3] #Nos da las neuronas del cluster 3 (el mas pequeño)
cluster2 <- som_cluster[som_cluster==2] #Nos da las neuronas del cluster 2 (el mediano)
cluster1 <- som_cluster[som_cluster==1] #Nos da las neuronas del cluster 1 (el mas grande/fondo)

#EXTAYENDO DATOS DE LAS NEURONAS DEL CLUSTER 3
codes <- as.data.frame(som_model3$codes)

clusterCode3 <- codes[names(cluster3),]

write.csv(clusterCode3, file = "cluster3.csv",row.names=FALSE)

#EXTRAYENDO DATOS DE LAS NEURONAS DEL CLUSTER 2

clusterCode2 <- codes[names(cluster2),]

write.csv(clusterCode2, file = "cluster2.csv",row.names=FALSE)

#EXTRAYENDO DATOS DE LAS NEURONAS DEL CLUSTER 1

clusterCode1 <- codes[names(cluster1),]

write.csv(clusterCode1, file = "cluster1.csv",row.names=FALSE)

##################################################################################

data(wines)
set.seed(7)
kohmap <- xyf(scale(wines), vintages,
              grid = somgrid(5, 5, "hexagonal"), rlen=100)
plot(kohmap, type="changes")
counts <- plot(kohmap, type="counts", shape = "straight")

## show both sets of codebook vectors in the map
par(mfrow = c(1,2))
plot(kohmap, type="codes", main = c("Codes X", "Codes Y"))
par(mfrow = c(1,1))

similarities <- plot(kohmap, type="quality", palette.name = terrain.colors)

12 predict.kohonen
plot(kohmap, type="mapping",
     labels = as.integer(vintages), col = as.integer(vintages),
     main = "mapping plot")

## add background colors to units according to their predicted class labels
xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap, type="mapping", col = as.integer(vintages),
     pchs = as.integer(vintages), bgcol = bgcols[as.integer(xyfpredictions)],
     main = "another mapping plot", shape = "straight", border = NA)

## Show 'component planes'
set.seed(7)
sommap <- som(scale(wines), grid = somgrid(6, 4, "hexagonal"))
plot(sommap, type = "property", property = getCodes(sommap, 1)[,2],
     main = colnames(getCodes(sommap, 1))[2])

## Show the U matrix
Umat <- plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")

## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(object.distances(sommap, "codes")), 5)
add.cluster.boundaries(sommap, som.hc)

## and the same for rectangular maps
set.seed(7)
sommap <- som(scale(wines),grid = somgrid(6, 4, "rectangular"))
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")

## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(object.distances(sommap, "codes")), 5)
add.cluster.boundaries(sommap, som.hc)
