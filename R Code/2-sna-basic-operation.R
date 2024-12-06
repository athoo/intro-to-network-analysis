# install.packages("igraph")
rm(list = ls()) # Remove all the objects we created so far.
library(igraph)          # load a package

###############################################
#Create networks 
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
plot(g1) # A simple plot of the network - we'll talk more about plots later

class(g1)
g1

# Now with 10 vertices, and directed by default:
g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10 )
plot(g2)   

g2

g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
# When the edge list has vertex names, the number of nodes is not needed
plot(g3)

g3

g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  

# In named graphs we can specify isolates by providing a list of their names.
plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 

# Small graphs can also be generated with a description of this kind: - for undirected tie, +- or -+ for directed ties pointing left & right, ++ for a symmetric tie, and “:” for sets of vertices.
plot(graph_from_literal(a---b, b---c)) # the number of dashes doesn't matter
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))
gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(gl)

###############################################
# Edge, vertex, and network attributes
E(g4) # The edges of the object
V(g4) # The vertices of the object

g4[]
g4[1,] 

# add attributes
V(g4)$name # automatically generated when we created the network.

V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")

E(g4)$type <- "email" # Edge attribute, assign "email" to all edges

E(g4)$weight <- 10    # Edge weight, setting all existing edges to 10

edge_attr(g4)

vertex_attr(g4)

graph_attr(g4)

g4 <- set_graph_attr(g4, "name", "Email Network")

g4 <- set_graph_attr(g4, "something", "A thing")



graph_attr_names(g4)

graph_attr(g4, "name")

graph_attr(g4)

g4 <- delete_graph_attr(g4, "something")

graph_attr(g4)

plot(g4, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     
     vertex.color=c( "pink", "skyblue")[1+(V(g4)$gender=="male")] ) 

g4s <- simplify( g4, remove.multiple = T, remove.loops = F, 
                 
                 edge.attr.comb=c(weight="sum", type="ignore") )

plot(g4s, vertex.label.dist=1.5)

g4s


###############################################
# Specific graphs and graph models
# empty graph
eg <- make_empty_graph(40)

plot(eg, vertex.size=10, vertex.label=NA)

# full graph
fg <- make_full_graph(40)

plot(fg, vertex.size=10, vertex.label=NA)

# simple star graph
st <- make_star(40)

plot(st, vertex.size=10, vertex.label=NA) 

# tree graph

tr <- make_tree(40, children = 3, mode = "undirected")

plot(tr, vertex.size=10, vertex.label=NA) 


# ring graph
rn <- make_ring(40)

plot(rn, vertex.size=10, vertex.label=NA)

# Erdos-Renyi random graph model
er <- sample_gnm(n=100, m=40) 

plot(er, vertex.size=6, vertex.label=NA)  

# Watts-Strogatz small-world model
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)

plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

# Barabasi-Albert preferential attachment model for scale-free graphs
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)

plot(ba, vertex.size=6, vertex.label=NA)







