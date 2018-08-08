# 1. (100 points) Network analysis using igraph and igraphdata.
# ??? Load the karate network in R, and describe the dataset in your own words, in 2-3 lines. Feel free to use help(karate).
rm(list=ls())
install.packages("igraphdata")
install.packages("igraph")
library(igraph)
library(igraphdata)
data("karate")
data("igraphdata")

# The structure of this network consists of name, citation, and author which is led by John A and Mr. Hi. The vertecise of
# each graph consist of name, faction, color, and label with edges attribute to the weight. There are 34 vertices and 78 undirected edges.


# ??? Is this a directed network? How many vertices and edges are there? 


kar = karate
is.directed(kar)
vcount(kar)
ecount(kar)
plot(kar)

# ??? Plot the network using the circle layout and the graphopt layout. 
# - Circle layout, with links colored according to type of connection (hyperlink/ mention)
# - Optimized layout (with graphopt), with links colored according to type of connection (hyperlink/ mention)

kar <- simplify(kar, remove.multiple = F, remove.loops = T) 
vcount(kar)
ecount(kar)
plot(kar, edge.arrow.size=.4,vertex.label=NA)
par(mfrow=c(1,2))
plot(kar,layout=layout_in_circle,edge.arrow.size=.4,vertex.label=NA)
plot(kar,layout=layout_with_graphopt,edge.arrow.size=.4,vertex.label=NA)
par(mfrow=c(1,1))

# ??? What are the vertex attributes for this network? Plot the network using the graphoptlayout and different vertex colors
#   according to the vertex attribute Faction.

vertex_attr(kar)
table(V(kar)$Faction)
plot(kar,layout=layout_with_graphopt,edge.arrow.size=.2,vertex.label=NA,
     vertex.color=c("red","blue")[V(kar)$Faction])
# There are Faction, name, label, and color vertex attributes for the network.
# The vertex sequence creates total number of 34 name vertices of the graph and among the Faction attribute, there
# are two catgories divided for the vertex attribute Faction.


# ??? Is it meaningful to calculate co-citation for this network? Why or why not? 
# Yes, there are total number of 34 correspondents that we are examining to see the relationship of each. The get.adjacency method creates
# an undirected matrix(symmetrical) of relationships between 1 and other nodes. This is just the standard representation of the karate adjacencies which
# doesn't prove much. The cocitation calculates cocitation counts if there is another vertex citing the two verices, Mr Hi and John A. Both adjacency and
# cocitation methods can be visually represented by using the heatmap which is very useful seeing the relationships.

degree(kar)
hist(degree(kar))
adj=get.adjacency(kar)
mat=as.matrix(adj)
heatmap(mat)

bib=bibcoupling(kar)
cocit=cocitation(kar)
heatmap(bib)
heatmap(cocit)


# ??? Calculate the geodesic distance between all pairs of nodes. Construct a heatmap showing geodesic distances between node pairs. 
#   Also construct a histogram of geodesic distances. What is the average geodesic distance in the network? 

geo = distances(kar)
heatmap(geo)
hist(geo)
mean(geo)




# ??? Calculate
# i) degree centrality,
# ii) eigenvector centrality,
# iii) pagerank,
# iv) closeness centrality, and
# v) betweenness centrality for all nodes in the network.
# Construct four plots, with degree centrality in the x-axis, and the other four centrality measures in the y-axis.
# What is your interpretation of these plots?

#centrality scores
c.deg = centr_degree(kar)$res
c.eigen = centr_eigen(kar)$vector
c.pagerank = page.rank(kar)$vector
c.close = centr_clo(kar)$res
c.btn = centr_betw(kar)$res

op=par(mfrow=c(2,2))
plot(c.deg,c.eigen)
plot(c.deg,c.close)
plot(c.deg,c.btn)
plot(c.deg,c.pagerank)
par(mfrow=c(1,1))


# Definition: Centralization is a method for creating a graph level centralization measure from the centrality scores of the vertices.
# Plot for the Eigenvector centrality shows increasing trend as both c.eigen and c.deg increases. Each vertex is proportional to the
# sum of the scores of its neighbors. So the central vertices are those with larger central neighbors with degrees depended.
# Closeness centrality is more applicable in this case since at low degree, there are higher number of closeness centralities with
# increasing plot. This implies that the many vertices at low degree have short average distance linking to the other vertices.
# Betweenness centrality by definition, it measures the extent to which a vertex lies on paths between other vertices.
# Vertices with higher betweenness centrality have more influence on the network, but in this case, overall betweeness centrality is
# very low. Pagerank centrality is very much like the Eigenvector centrality plot in which depends heavily on the degree of vertex.
# In this plot however, centrality points derived from neighbors and their out degree aren't so high, so the graph shows more fixed trend
# overall.



# ??? For each centrality measure, report the five most important vertices according to that measure. Using the vertex
#   attribute name, identify these important vertices by name. Overall, which two vertices do you think are most important
#   in the network?


V(kar)$name[order(-c.deg)[1:5]]
V(kar)$name[order(-c.eigen)[1:5]]
V(kar)$name[order(-c.close)[1:5]]
V(kar)$name[order(-c.pagerank)[1:5]]

# According to the output values from the centrality measure, Actor 3 and Actor 33 are most important in the network.
