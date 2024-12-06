
###################################
# This doc is adapted from NetSciX 2016 School of Code Workshop, Wroclaw, Poland (https://kateto.net/netscix2016.html)
library(igraph)
# Reading network data from files
setwd("/Users/pingjingyang/Documents/is527-network-science/is527")

# one-mode
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)

links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)

head(links)

nrow(nodes); length(unique(nodes$id))

nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)

links <- links[order(links$from, links$to),]

colnames(links)[4] <- "weight"

rownames(links) <- NULL

# two-mode
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)

links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
head(nodes2)

head(links2)
links2 <- as.matrix(links2)

dim(links2)

dim(nodes2)

###################################
#Turning networks into igraph objects

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

class(net)

net

E(net)       # The edges of the "net" object

V(net)       # The vertices of the "net" object

E(net)$type  # Edge attribute "type"

V(net)$media # Vertex attribute "media"

plot(net, edge.arrow.size=.4,vertex.label=NA)

net <- simplify(net, remove.multiple = F, remove.loops = T) 

as_edgelist(net, names=T)

as_adjacency_matrix(net, attr="weight")

as_data_frame(net, what="edges")

as_data_frame(net, what="vertices")

# dataset 2

head(nodes2)
head(links2)
net2 <- graph_from_incidence_matrix(links2)

table(V(net2)$type)

net2.bp <- bipartite.projection(net2)
as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2)) 

t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1, vertex.size=7, vertex.label=nodes2$media[ is.na(nodes2$media.type)])


#####################################
#Plotting networks with igraph
#Plot with curved edges (edge.curved=.1) and reduce arrow size:

plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to gray, and the node color to orange. 

# Replace the vertex label with the node names stored in "media"

plot(net, edge.arrow.size=.2, edge.curved=0,
     
     vertex.color="orange", vertex.frame.color="#555555",
     
     vertex.label=V(net)$media, vertex.label.color="black",
     
     vertex.label.cex=.7) 



# Generate colors based on media type:

colrs <- c("gray50", "tomato", "gold")

V(net)$color <- colrs[V(net)$media.type]



# Set node size based on audience size:

V(net)$size <- V(net)$audience.size*0.7



# The labels are currently node IDs.

# Setting them to NA will render no labels:

V(net)$label.color <- "black"
  
V(net)$label <- NA


# Set edge width based on weight:

E(net)$width <- E(net)$weight/6


#change arrow size and edge color:

E(net)$arrow.size <- .2

E(net)$edge.color <- "gray80"
  
E(net)$width <- 1+E(net)$weight/12

plot(net, edge.color="orange", vertex.color="gray50")  # with specific format

plot(net) 

legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media,  vertex.label.font=2, vertex.label.color="gray40", vertex.label.cex=.7, edge.color="gray85")

edge.start <- ends(net, es=E(net), names=F)[,1]

edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)  


net.bg <- sample_pa(80) 

V(net.bg)$size <- 8

V(net.bg)$frame.color <- "white"
  
V(net.bg)$color <- "orange"
  
V(net.bg)$label <- "" 

E(net.bg)$arrow.mode <- 0

plot(net.bg)


plot(net.bg, layout=layout_randomly)


###########################################
# Network and node descriptives
# density - ratio of edges of all possible edges
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) #for a directed network

# reciprocity
reciprocity(net)

dyad_census(net) # Mutual, asymmetric, and nyll node pairs

2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

# transitivity
# global - ratio of triangles (direction disregarded) to connected triples.
# local - ratio of triangles to connected triples each vertex is part of.
transitivity(net, type="global")  # net is treated as an undirected network

transitivity(as.undirected(net, mode="collapse")) # same as above

transitivity(net, type="local")

triad_census(net) # for directed networks 

# Triad types (per Davis & Leinhardt):
# 003 A, B, C, empty triad.
# 012 A->B, C
# 102 A<->B, C
# 021D A<-B->C
# 021U A->B<-C
# 021C A->B->C
# 111D A<->B<-C
# 111U A<->B->C
# 030T A->B<-C, A->C
# 030C A<-B<-C, A->C.
# 201 A<->B<->C.
# 120D A<-B->C, A<->C.
# 120U A->B<-C, A<->C.
# 120C A->B->C, A<->C.
# 210 A->B<->C, A<->C.
# 300 A<->B<->C, A<->C, completely connected.

# Diameter
# the shortest distance between two most distant nodes in the network
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diam <- get_diameter(net, directed=T)
diam

class(diam)
as.vector(diam)
vcol <- rep("gray40", vcount(net))

vcol[diam] <- "gold"
  
ecol <- rep("gray80", ecount(net))

ecol[E(net, path=diam)] <- "orange" 
  
# E(net, path=diam) finds edges along a path, here 'diam'

plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

# node degrees
deg <- degree(net, mode="all")

plot(net, vertex.size=deg*3)

hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# degree distribution

deg.dist <- degree_distribution(net, cumulative=T, mode="all")

plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      
      xlab="Degree", ylab="Cumulative Frequency")

# Centrality & centralization
degree(net, mode="in")

centr_degree(net, mode="in", normalized=T)

closeness(net, mode="all", weights=NA) 

centr_clo(net, mode="all", normalized=T) 

eigen_centrality(net, directed=T, weights=NA)

centr_eigen(net, directed=T, normalized=T) 

betweenness(net, directed=T, weights=NA)

edge_betweenness(net, directed=T, weights=NA)

centr_betw(net, directed=T, normalized=T)



# Distances and paths

# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(net, directed=F)
mean_distance(net, directed=T)

# We can also find the length of all shortest paths in the graph:
distances(net) # with edge weights
distances(net, weights=NA) # ignore weights

# We can extract the distances to a node or set of nodes we are interested in.
# Here we will get the distance of every media from the New York Times.
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, vertex.label.color="white")

# We can also find the shortest path between specific nodes.
# Say here between MSNBC and the New York Post:
news.path <- shortest_paths(net, from = V(net)[media=="MSNBC"], to  = V(net)[media=="New York Post"], output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"

# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4

# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"
  
plot(net, vertex.color=vcol, edge.color=ecol, edge.width=ew, edge.arrow.mode=0)

# Identify the edges going into or out of a vertex, for instance the WSJ.
# For a single node, use 'incident()', for multiple nodes use 'incident_edges()'
inc.edges <- incident(net, V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"
plot(net, vertex.color=vcol, edge.color=ecol)

# We can also easily identify the immediate neighbors of a vertex, say WSJ.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s).

neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=vcol)

# Special operators for the indexing of edge sequences: %--%, %->%, %<-%
# E(network)[X %--% Y] selects edges between vertex sets X and Y, ignoring direction
# E(network)[X %->% Y] selects edges from vertex sets X to vertex set Y
# E(network)[X %->% Y] selects edges from vertex sets Y to vertex set X

# For example, select edges from newspapers to online sources:
E(net)[ V(net)[type.label=="Newspaper"] %->% V(net)[type.label=="Online"] ]

# Cocitation (for a couple of nodes, how many shared nominations they have)
cocitation(net)

      
# ================ Subgroups and communities ================
      
# Converting 'net' to an undirected network.
# There are several ways to do that: we can create an undirected link between any pair
# of connected nodes (mode="collapse), or create an undirected link for each directed
# one (mode="each"), or create an undirected link for each symmetric link (mode="mutual").
# In cases when A -> B and B -> A are collapsed into a single undirected link, we
# need to specify what to do with the edge attributes. Here we have said that
# the 'weight' of links should be summed, and all other edge attributes ignored.

net.sym <- as.undirected(net, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))


#  ------->> Communities --------
        
# A number of algorithms aim to detect groups that consist of densely connected nodes
# with fewer connections across groups. 

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, net)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.

# Assortativity and Homophily

# Assortativity (homophily)
# The tendency of nodes to connect to others who are similar on some variable.
# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees
        
V(net)$type.label
V(net)$media.type

assortativity_nominal(net, V(net)$media.type, directed=F)

assortativity(net, V(net)$audience.size, directed=F)

assortativity_degree(net, directed=F)
