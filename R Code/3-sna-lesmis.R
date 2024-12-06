################################################################################
## Script Name: SNAonR with Les Mis.R
## Author: Pingjing Yang (adapted from Ly Dinh)
## Purpose: Demonstration of how to pre-process, integrate, and analyze social network data, specifically Les Miserables dataset
################################################################################

## Data Sources
## Network data: Les Miserables  
## Nodes: characters in novel (77 in total)
## Edges: if 2 characters appear in the same chapter of novel (254 in total)
## Attribute Data: No attribute data

################################################################################
## Data Pre-Processing
## Set working directory
## Install appropriate network & statistics packages
## Converting data files from .txt to graph data frame
## Ensure no nodes or edges are missing

################################################################################
## Data analysis
## Run basic metrics & centrality measures at network-level and other levels of analysis if need be
## Plot degree distributions, and correlation among centrality measures 

################################################################################

#clear all in the 'global environment' 
rm(list=ls())
getwd()

#Set working directory

setwd("/Users/pingjingyang/Documents/is527-network-science/is527")

# install.packages("igraph")
# install.packages("plyr")
# install.packages("calibrate")
# install.packages("car") # for plots, histograms, box plots

library(igraph)
library(plyr) ##Tools for Splitting, Applying and Combining Data
library(calibrate) ##also for formatting 
library(car)

## Load les miserables dataset into working directory
lesmis = read.table("lesmis.txt", header = FALSE, sep = "\t")

# View dataset, and some basic summary 
lesmis
# rename the 3 column names 
colnames(lesmis) = c('From', 'To', 'Weight')

#check if the column names are changed
head(lesmis)

# convert the file from an edgelist to a graph dataframe: 
lesmis_gdf = simplify(graph.data.frame(lesmis, directed=FALSE)) 

# Counts for number of nodes
vcount(lesmis_gdf)
# Counts for number of edges
ecount(lesmis_gdf)

# Plot the network using the plot() function, of graph dataframe
plot(lesmis_gdf, vertex.size=1)
# can always put ? in front of any functions you want to know more about 
?plot


##### Basic analysis
# Inspect vertex degree and use the degree for vertex size
hist(degree(lesmis_gdf))

# retrieve vertex ids:
# vertex names are usually stored in a vertex attribute named name in igraph. use V(g)$name to retrieve the names of all the vertices.
match("JeanValjean", V(lesmis_gdf)$name)
match("Gavroche", V(lesmis_gdf)$name)

## Get vertex ids of ALL actors:
V(lesmis_gdf)$name

## Number of paths between one vertex (Gavroche) and another vertex (JeanValjean)
edge.disjoint.paths(lesmis_gdf, 6, 55)


# I can now test the various centrality measures to have a feel of the whole network centralization
# betweenness centrality
between = betweenness(lesmis_gdf)
#closeness centrality
closeness = closeness(lesmis_gdf)
#local clustering coefficient
transitivity(lesmis_gdf, type="local")
#eigenvector centrality
eigenvector = evcent(lesmis_gdf)$vector

#summary for each centrality
summary(betweenness(lesmis_gdf))
summary(closeness(lesmis_gdf))
summary(degree(lesmis_gdf))

## SORTING function highest , and view top 10 
sort(betweenness(lesmis_gdf), decreasing = TRUE)[1:10]

sort(degree(lesmis_gdf), decreasing = TRUE)[1:10]

sort(evcent(lesmis_gdf)$vector, decreasing = TRUE)[1:10]

## more types of analysis we can compare
graph.density(lesmis_gdf)
triangles(lesmis_gdf)
diameter(lesmis_gdf)

##output
write.table(degree(lesmis_gdf), sep=",",  col.names=T)

## generate random graphs according to the G(n,m) Erdos-Renyi model. n = number of vertices , m = number of edges
randomgraph = sample_gnm(n = vcount(lesmis_gdf), m = ecount(lesmis_gdf), directed=FALSE)
degree_distribution(randomgraph)

diameter(randomgraph)
graph.density(randomgraph)
triangles(randomgraph)
summary(betweenness(randomgraph))

plot(degree_distribution(randomgraph))

## generate scale-free graphs according to the Barabasi-Albert model. n = number of vertices , m = number of edges added on each time step
pa = sample_pa(vcount(lesmis_gdf), m=2, directed = FALSE)

plot(degree_distribution(pa))

diameter(pa)
graph.density(pa)
triangles(pa)
summary(betweenness(pa))

##  PLOTS & visualization
# I can also plot the degree distribution to see how spread out are the differences in the number of connections each node has
plot(degree.distribution(lesmis_gdf), xlab="node degree")


# Basic visualization with plot.igraph, a step from simple plot function
plot.igraph(lesmis_gdf,layout=layout.fruchterman.reingold, vertex.color='red',vertex.size=degree(lesmis_gdf), vertex.label=V(lesmis_gdf)$name)

# or, if we want to remove the vertex labels --- vertex.label=NA
plot.igraph(lesmis_gdf,vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color='red',vertex.size=degree(lesmis_gdf), vertex.label=lesmis)

## try different layouts  layout=layout.fruchterman.reingold
plot.igraph(lesmis_gdf, layout=layout.circle, vertex.color='red',vertex.size=degree(lesmis_gdf), vertex.label=V(lesmis_gdf)$name)


# One nice thing for 'plot' is that I can plot centrality measures against each other to see correlations among various metrics, and identify overlapping central actors.

## Why plot eigenvector with betweenness 
## High betweenness and low eigenvector centrality may be an important gatekeeper to a central actor.
# Low betweeness and high eigenvector centrality may have unique access to central actors.

# plot eigenvector with betweenness 
plot(evcent(lesmis_gdf)$vector, betweenness(lesmis_gdf), xlab='eigenvector', ylab='betweenness', xlim=c(0,1), cex=1.5)

## check correlation
centralities <- data.frame(between, closeness, eigenvector)
pearson_cor <- cor(centralities, method = "pearson") # Pearson correlation matrix
pearson_cor
spearman_cor = cor(centralities, method = "spearman") # Spearman correlation matrix
spearman_cor

# Basic scatterplot
pairs(~between + closeness + eigenvector, data=centralities, main="Simple Scatterplot Matrix")


# scatterplot with line of best fit
scatterplotMatrix(~between + closeness + eigenvector, data=centralities, main="Simple Scatterplot Matrix")

