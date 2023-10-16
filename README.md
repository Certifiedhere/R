
# Practical No01
##  Write a program to compute the following for a given a network:

-  (i) number of edges, 
- (ii) number of nodes;
- (iii) degree of node;
- (iv) node with lowest degree;
- (v)the adjacency list;
- (vi) matrix of the graph

```R
library(igraph)
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,4-7, 5-6, 6-7)

# Name of Edges & Nodes
V(g)
E(g)

# Plotting of graph
plot(g)

# Number of vertices/node:
vcount(g)
[1] 7

# Number of edges/dyad/ties:
ecount(g)

#In-degree
degree(dg,mode="in")

#Out-degree
degree(dg,mode="out")

#Node with lowest degree
V(dg)$name[degree(dg)==min(degree(dg))]

#Node with highest degree
V(dg)$name[degree(dg)==max(degree(dg))]

#Adjacency List:
get.adjlist(dg)

#adjanceny Matrix
get.adjacency(dg)

```

- directed graph

```R
dg <- graph.formula(1-+2, 1-+3, 2++3)
```

- Graph with name

```R
dg1 <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
plot(dg1)
```

# Practical No02

## Problem Statement

- (i) View data collection forms and/or import one-mode/ two-mode datasets;
- (ii) Basic Networks matrices transformations.

```R
#Here we set file path
getwd()
setwd()
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
head(nodes)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(links)

net <- graph.data.frame(d=links, vertices=nodes, directed=T)
m=as.matrix(net)
get.adjacency(net)
plot(net)

read.table("mydata.txt")
```

# Practical No 03

Compute the following node level measures:
(i) Density;
(ii) Degree;
(iii) Reciprocity;
(iv) Transitivity;
(v) Centralization;
(vi) Clustering.

```R
#Density;
library(igraph)
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,4-7, 5-6, 6-7)

vcount(g)
ecount(g)
ecount(g)/(vcount(g)*(vcount(g)-1))
plot(g)

#Degree
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
net <- graph.data.frame(d=links, vertices=nodes, directed=T)
m=as.matrix(net)
degree(net)

#Reciprocity
dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)
reciprocity(dg)
dyad.census(dg)
2*dyad.census(dg)$mut/ecount(dg)

#Transitivity
library(igraphdata)
kite <- graph.famous("Krackhardt_Kite")
atri <- adjacent.triangles(kite)
plot(kite, vertex.label=atri)

transitivity(kite, type="local")
adjacent.triangles(kite) / (degree(kite) * (degree(kite)-1)/2)

# Centralization
# 1 Degree of centrality
 centralization.degree(net, mode="in", normalized=T)

# 2 Closeness Centralization
closeness(net, mode="all", weights=NA)
centralization.closeness(net, mode="all", normalized=T)

# 3 Betweeness Centrality
betweenness(net, directed=T, weights=NA)
edge.betweenness(net, directed=T, weights=NA)
centralization.betweenness(net, directed=T, normalized=T)

# 4 Eigenvector centrality
centralization.evcent(net, directed=T, normalized=T)


# Clustering.
plot(kite)
get.adjedgelist(kite, mode = c("all", "out", "in", "total"))

g2 <- barabasi.game(50, p=2, directed=FALSE)
g1 <- watts.strogatz.game(1, size=100, nei=5, p=0.5)
g <- graph.union(g1, g2)
g <- simplify(g)
ebc <- edge.betweenness.community(g, directed=FALSE)
# loop
mods <- sapply(0:ecount(g), function(i) {
g2 <- delete.edges(g, ebc$removed.edges[seq(length = i)])
c1 <- clusters(g2)$membership
modularity(g, c1)
})
plot(mods, pch=20)

g2 <- delete.edges(g, ebc$removed.edges[seq(length = which.max(mods) - 1)])
V(g)$color = clusters(g2)$membership
g$layout <- layout.fruchterman.reingold
plot(g, vertex.label = NA)

fc <- fastgreedy.community(g)
com <- as.vector(membership(fc))
g$layout <- layout.fruchterman.reingold
plot(g, vertext.label=NA)

```

# Pract 04

## For a given network find the following:

(i) Length of the shortest path from a given
node to another node;

(ii) the density of the graph

```R
library(igraph)
matt <- as.matrix(read.table(text=
  "node  R  S  T  U
   R  7  5  0  0
   S  7  0  0  2
   T  0  6  0  0
   U  4  0  1  0", header=TRUE))

nms <- matt[,1]
matt <- matt[,-1]
colnames(matt) <- rownames(matt) <- nms
matt[is.na(matt)] <- 0
g <- graph.adjacency(matt, weighted=TRUE)
plot(g)

```

- The density of the graph
- The density of a graph is the ratio of the number of edges and the number of possible edges.

```R
library(igraph)
dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)
graph.density(dg, loops=TRUE)
```

# Practical No. 5

## Write a program to distinguish between:

1 - a network as a sociogram (or “network graph”)

2 - a network as a matrix,&

3 - a network as an edge list.

- using 3 distinct networks representatives of each.

```R
library(igraph)
ng <- graph.formula(Andy++Garth,Garth-+Bill,Bill-+Elena,Elena++Frank,Carol-+Andy,Carol-+Elena,Carol++Dan,Carol++Bill,Dan++Andy,Dan++Bill)
plot(ng)

get.adjacency(ng)
E(ng)
get.adjedgelist(ng,mode="in")

```

# Practical No. 6

## Write a program to exhibit

- structural equivalence,
- automatic equivalence, &
- regular equivalence from a network.

```R
install.packages("sna")
library(sna)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
eq <- equiv.clust(links2)
plot(eq)

g.se <- sedist(links2)
plot(cmdscale(as.dist(g.se)))

b <- blockmodel(links2, eq, h=10)
plot(b)
```

# Practical No. 7

## Calculate Hamming distance

```R
install.packages("e1071")
library(e1071)

x <- c(0,0,0,0)
y <- c(0,1,0,1)
z <- c(1, 0, 1, 1)
w <- c(0, 1, 1, 1)

hamming.distance(x, y)
hamming.distance(y,z)
hamming.distance(y,w)
hamming.distance(z,w)
hamming.distance(x, w)
hamming.distance(x, z)

```

# Practical No. 8

## Perform SVD analysis of a network.

```R
library(igraph)
a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)
print(a)
svd(a)
```

# Practical No. 9

## Displaying Bipartite network in the graph format.

```R
davis <- read.csv(file.choose("Copy as path and past with //"), header=FALSE)
g <- graph.data.frame(davis, directed=FALSE)
plot(g)

V(g)$type <- bipartite_mapping(g)$type
plot(g)
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")

V(g)$label.color <- "black"
V(g)$label.cex <- 1
V(g)$frame.color <-  "gray"
V(g)$size <- 18
plot(g, layout = layout_with_graphopt)
plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)

```
