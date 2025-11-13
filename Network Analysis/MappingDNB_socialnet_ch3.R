###Load and format SNA data

install.packages(c("igraph","ggplot2", "dplyr", "gridExtra", "tidyverse","ggpubr", "visNetwork", "ggpubr", "proxy","corrplot","dendextend"))

library(igraph)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyverse)
library(ggpubr)
library(visNetwork)
library(ggpubr)
library(proxy)
library(corrplot)
library(dendextend)

###1. Language Network
#nodes: languages (unique)
#edges: title counts (sum)

##Load df with counts of titles per language from ch.2.1 results
lang <- read.csv("data/alldnb_languages_freq_ch2.1_v2.csv")
lang$x <- NULL

##Load df with author per language (just rows, no frequencies)
author_lang_freq <- read.csv("data/alldnb_author_language_clean_ch2.1_v2.csv")

##load table with title counts per language per author
author_lang_table <- read.csv("data/alldnb_author_language_count_ch2.1_v2.csv")

##1.Create language per author matrix

##1.1create adjacency matrix with binary classifier if author has translations in a given language

author_lang_matrix<- author_lang_table %>% mutate_if(is.numeric, ~1 * (. > 0))

#delete author column
lang_lang_matrix <- author_lang_matrix
lang_lang_matrix[1] <- NULL
lang_lang_matrix[1] <- NULL
lang_lang_matrix <- as.matrix(lang_lang_matrix)

lang_lang_matrix<-t(lang_lang_matrix)%*%as.matrix(lang_lang_matrix)
write.csv(lang_lang_matrix, file="alldnb_lang_lang_matrix_authorcount_ch2.2_v2.csv")

#set diagonal to zero (only sums of each language)
diag(lang_lang_matrix)<-0
# Mean of the each row of the matrix
lang_lang_matrix_mean<-rowMeans(lang_lang_matrix)              
sum(lang_lang_matrix)

##Make graph object
g2 <- graph_from_adjacency_matrix(lang_lang_matrix, weighted=TRUE, mode = "undirected")
##The number of shared authors will be the weights
E(g2)$weight
#E(g2)$width <- 1+E(g2)$weight/12

##Export as edgelist
lang_lang_author_edges <- get.data.frame(g2)

##has a lot of na_count values (???)
#delete na_count values
lang_lang_author_edges <- lang_lang_author_edges %>% 
  filter(!str_detect(to, 'na_count'))

write.csv(lang_lang_author_edges, file="alldnb_lang_lang_author_edges_ch2.2_v2.csv")

##Create nodelist
nodes <- NULL
nodes$id <- lang$Language
nodes <- as.data.frame(nodes)

write.csv(nodes, file="alldnb_lang_lang_author_nodes_ch2.2_v2.csv")


##subset for only ties above 100
lang_lang_author_edges_above100 <- lang_lang_author_edges %>% filter_all(all_vars(. > 100))
write.csv(lang_lang_author_edges_above100, file="alldnb_lang_lang_author_edges_above100_ch2.2_v2.csv")

##subset for only ties above 200
lang_lang_author_edges_above200 <- lang_lang_author_edges %>% filter_all(all_vars(. > 200))
write.csv(lang_lang_author_edges_above200, file="alldnb_lang_lang_author_edges_above200_ch2.2_v2.csv")

##Visualize static language network by shared authors
g2_above100 <- lang_lang_author_edges_above100
names(g2_above100)[3] <- "weight"
g2_above100 <- graph_from_data_frame(g2_above100, directed = F, vertices = NULL)
l<-layout_with_fr(g2_above100)
plot(g2_above100, vertex.size=1,vertex.shape="none",
     edge.width=(edge_attr(g2_above100)$weight)/100,
     edge.curved=0.3,
     layout=l*3.5,
     main="Language Network weighted\n by sums of shared authors (>100 authors)")

g2_above200 <- lang_lang_author_edges_above200
names(g2_above200)[3] <- "weight"
g2_above200 <- graph_from_data_frame(g2_above200, directed = F, vertices = NULL)
l<-layout_with_dh(g2_above200)
plot(g2_above200, vertex.size=1,vertex.shape="none",
     edge.width=((edge_attr(g2_above200)$weight)/100),
     layout=l*3.5,
     main="Language Network weighted\n by sums of shared authors (>200 authors)")

###Clean up the network graph (delete obscure vertices etc.) and visualize the complete network
backup_g2 <- g2

g2 <- delete_vertices(g2, c(1,5)) %>%
  delete_vertices("xxx")
g2 <- delete_vertices(g2, c(1,5)) %>%
  delete_vertices("na_count")

#first, confirm how many nodes and edges you have
g2 #gives you overview of your graph
length(V(g2)) #V(g) = vectors for your graph object g
length(E(g2)) #E(g) = edges for your graph object g

###Save the network
save(g2, file="CentalityNetwork.Rda")

###Plot the complete network with node labels
##With Kamada Kawai Layout
plot(g2, layout = layout.kamada.kawai(g2), vertex.size = 3, vertex.frame.color = NULL, 
     vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5, vertex.label=g2$names)

#Export as: lang_lang_author_network_all_kamada.pdf

###Other plotting options

##With fruchterman.reingold

#plot(simplify(g2), vertex.label=g2$names, vertex.label.dist=2, vertex.size= 1,edge.arrow.size=0.001,vertex.frame.color = adjustcolor("white", alpha.f = 0),vertex.color = adjustcolor("white", alpha.f = 0),edge.color=adjustcolor(1, alpha.f = 0.15),display.isolates=FALSE, layout=layout.kamada.kawai(g2), vertex.label.font=2, vertex.label.color=c("blue"))

lo <- layout.fruchterman.reingold(g2, repulserad = vcount(g2)^2.8, 
                                  area = vcount(g2)^2.3, niter = 1000)

plot(g2, layout = lo, vertex.size = 3, vertex.frame.color = NULL, 
     vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5, vertex.label=g2$names)

##without isolates
iso <- V(g2)[degree(g2)==0]
g2_noiso <- delete.vertices(g2, iso)

plot(g2_noiso, layout = lo, vertex.size = 3, vertex.frame.color = NULL, 
     vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5, vertex.label=g2$names)

#Export as: lang_lang_author_network_all_fruchtermann.pdf

###Describe the network
#transitivity (how connected is my network?)
#this tells you how many closed loops there are as a percentage of length 2 paths
transitivity(g2, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

## Diameter
## make sure to use g.dist if you are measuring document similarities
#diameter(g2, directed = FALSE, weights=E(g2)$weight)
## tells you which are the two furthest nodes from each other
#farthest.nodes(g2, weights=E(g2)$weight)

## Degree Distribution/Degree Centrality: the # of neighbors a given node has
#this will tell you the min/max # of edges, avg values, etc.
#its a good way of seeing the distribution of your edges
#are there a few highly connected nodes or are the connections even distributed in your network?
summary(degree(g2))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   40.00   58.00   49.75   66.00   73.00

# degree = how many edges the node has
View(sort(degree(g2), TRUE))
#if you want to save this as a table, do this
deg<-sort(degree(g2), TRUE)
write.csv(deg, file="alldnb_lang_lang_author_degree_ch2.2_v2.csv")

#Plot histogram of node degree
hist(deg, breaks=1:vcount(g2)-1, main="Histogram of node degree")

##Including rownames as axis labels
deg_df <- as.data.frame(deg)
deg_df <- tibble::rownames_to_column(deg_df, "lang")

ggplot(deg_df, aes(x=reorder(lang, -deg), y=deg)) + 
  geom_bar(stat="identity") + xlab("Languages") + ylab("Degree")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + ggtitle("Degree (number of edges or shared authors) between languages)") +coord_flip()+theme_bw()

#Export as: alldnb_lang_lang_author_degree_ch2.2_v2.pdf

##Plot as network

plot(g2_noiso, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2_noiso), 
     edge.width=E(g2_noiso)$size*0.5,
     vertex.size= degree(g2_noiso)*0.25, 
     main="Language Network \nby shared authors", sub= "Degree Centrality")

#Export as: lang_lang_author_network_degree.pdf

#degree distribution
deg.dist <- as.data.frame(degree_distribution(g2, cumulative=T, mode="all"))
deg.dist <- cbind(deg.dist, (deg_df[1:74,]))
colnames(deg.dist)[1] <- "deg_dist"

write.csv(deg.dist, file="alldnb_lang_lang_author_degreedist_ch2.2_v2.csv")

#calculate and visualize for subset with language above 100 authors
deg_g2_above100 <- sort(degree(g2_above100), TRUE)
V(g2_above100)$size <- (deg_g2_above100) #size the network nodes by their node degree 

summary(degree(g2_above100))

write.csv(deg_g2_above100, file="alldnb_lang_lang_author_degree_above100_ch2.2_v2.csv")

##degree distribution histogram
deg_g2_above100_df <- as.data.frame(deg_g2_above100)
deg_g2_above100_df <- tibble::rownames_to_column(deg_g2_above100_df, "lang")
names(deg_g2_above100_df)[2] <- "deg"

ggplot(deg_g2_above100_df, aes(x=reorder(lang, -deg), y=deg)) + 
  geom_bar(stat="identity") + xlab("Languages") + ylab("Degree")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + ggtitle("Degree (number of edges or shared authors)\n for languages with more than 100 authors") +coord_flip()+theme_bw()

#Export as: alldnb_lang_lang_author_above100_degree_ch2.2_v2.pdf

##network graph for degree
##the graph attribute "size" needs to be adjusted for each centrality measure

degree_g2_above100 <- degree(g2_above100)
V(g2_above100)$size <- (degree_g2_above100) #size the network nodes by their node degree 

plot(g2_above100, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2_above100),
     main="Language Network \nby shared authors (>100)", sub= "Degree Centrality")

#Export as: lang_lang_author_network_above100_degree.pdf

###Only plot peripheral languages (<100 authors)
#subset
lang_lang_author_edges_under100 <- filter(lang_lang_author_edges, weight < 100)
write.csv(lang_lang_author_edges_under100, file="alldnb_lang_lang_author_edges_under100_ch2.2_v2.csv")

g2_under100 <- lang_lang_author_edges_under100
names(g2_under100)[3] <- "weight"
g2_under100 <- graph_from_data_frame(g2_under100, directed = F, vertices = NULL)

#calculate and visualize for subset with language above 100 authors
deg_g2_under100 <- sort(degree(g2_under100), TRUE)
V(g2_under100)$size <- (deg_g2_under100) #size the network nodes by their node degree 

summary(degree(g2_under100))

write.csv(deg_g2_under100, file="alldnb_lang_lang_author_degree_under100_ch2.2_v2.csv")

##degree distribution histogram
deg_g2_under100_df <- as.data.frame(deg_g2_under100)
deg_g2_under100_df <- tibble::rownames_to_column(deg_g2_under100_df, "lang")
names(deg_g2_under100_df)[2] <- "deg"

ggplot(deg_g2_under100_df, aes(x=reorder(lang, -deg), y=deg)) + 
  geom_bar(stat="identity") + xlab("Languages") + ylab("Degree")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + ggtitle("Degree (number of edges or shared authors)\n for languages with more than 100 authors") +coord_flip()+theme_bw()

#Export as: alldnb_lang_lang_author_under100_degree_hist_ch2.2_v2.pdf

###Plot as network

plot(g2_under100, edge.arrow.size = .5, vertex.color = "gold",
     vertex.size= degree(g2_under100)*0.25,
     layout=layout.kamada.kawai(g2_under100),
     main="Language Network \nby shared authors (<100)", sub= "Degree Centrality")

#Soom and save as: lang_lang_author_network_under100_degree_ch2.2_v2.pdf

#transitivity (how connected is my network?)
#this tells you how many closed loops there are as a percentage of length 2 paths
transitivity(g2_under100, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))


###2.2.2.
# Eigenvector centrality
# your centrality is based on the degree of your neighbors (how connected your neighbors are)
evcent(g2, directed=FALSE)$vector # if a directed graph, set to TRUE
eigen<-sort(evcent(g2)$vector, TRUE)
write.csv(eigen, file="alldnb_lang_lang_author_eigenvec_ch2.2_v2.csv")

V(g2)$size <- (eigen) #size the network nodes by their node degree 
plot(g2, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2), vertex.size= evcent(g2)$vector*20,
     main="Language Network \nby shared authors (>100)", sub= "Eigenvector Centrality")

#Save as: lang_lang_author_network_eigen

#visualize for subset
eigen_g2_above100 <- sort(evcent(g2_above100)$vector, TRUE)
write.csv(eigen_g2_above100, file="alldnb_lang_lang_author_above100_eigenvec_ch2.2_v2.csv")

V(g2_above100)$size <- (eigen_g2_above100) #size the network nodes by their node degree 

plot(g2_above100, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2_above100), vertex.size= evcent(g2_above100)$vector*20,
     main="Language Network \nby shared authors (>100)", sub= "Eigenvector Centrality")

#Save as: lang_lang_author_network_above100_eigen.pdf


# Betweenness
#the number of times a node acts as a bridge along the shortest path between two other nodes.
betweenness(g2)
sort(betweenness(g2), TRUE)
bet<-sort(betweenness(g2), TRUE)
write.csv(bet, file="alldnb_lang_lang_author_betweenness_ch2.2_v2.csv")

#Plot for complete network
V(g2)$size <- (bet) #size the network nodes by their node degree 
plot(g2, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2), vertex.size=betweenness(g2)*0.05,
     main="Language Network \nby shared authors", sub= "Betweenness Centrality")

##Zoom and save as: alldnb_lang_lang_author_betweenness_ch2.2_v2.png

#visualize for subset
bet_g2_above100 <- as.data.frame(sort(betweenness(g2_above100)))
write.csv(bet_g2_above100, file="alldnb_lang_lang_author_above100_betweenness_ch2.2_v2.csv")

V(g2_above100)$size <- (bet_g2_above100) #size the network nodes by their node degree
plot(g2_above100, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2_above100), vertex.size= betweenness(g2_above100),
     main="Language Network \nby shared authors (>100)", sub= "Betweenness Centrality")

#Zoom and save as: lang_lang_author_network_above100_betweenness.png

###For describing the data and examples: Look at which languages share which authors 
#Let's check which languages the one with the highest betweenness score has connections with in alldnb_lang_lang_author_edges_ch2.2_v2.csv
lang_lang_author_edges %>% 
  filter_all(any_vars(str_detect(., "fry")))

#Which authors do they share?
#Check alldnb_author_language_count_ch2.1_v2.csv or author_lang_table
View(as.data.frame(author_lang_table[,c('pol')]))
#Select all columns with value greater than 0, equal to 1 or greater than 1
View(filter(author_lang_table, pol > 0))

##See which languages Polish shares the most authors with other languages
author_lang_match_matrix <- author_lang_matrix
##create matrix with binary classifier if author has translations in a given language
author_lang_match_matrix<- author_lang_table %>% mutate_if(is.numeric, ~1 * (. > 0))
#Make sure all vectors are numeric
author_lang_match_matrix <- tibble::column_to_rownames(author_lang_match_matrix, "author")
#delete author column
author_lang_match_matrix[1] <- NULL
author_lang_match_matrix[1] <- NULL
author_lang_match_matrix$na_count <- NULL
##Sum each row of the table and add to column
author_lang_match_matrix_authorcount<-rowSums(author_lang_match_matrix)
author_lang_match_matrix$authorcount <- author_lang_match_matrix_authorcount

write.csv(author_lang_match_matrix, file="alldnb_lang_lang_authorsum_ch2.2_v2.csv")

##Correlation
###See which languages are correlated by the number of common authors
author_lang_match_cor<- author_lang_match_matrix
author_lang_match_cor$authorcount <- NULL
author_lang_match_cor = cor(author_lang_match_cor)

write.csv(author_lang_match_cor, file="alldnb_lang_lang_author_cor_ch2.2_v2.csv")

#Correlation plot (not very readable)
# corrplot(author_lang_match_cor, method = 'color')
# corrplot(author_lang_match_cor, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
# corrplot(author_lang_match_cor, order = 'hclust', addrect = 2)

# ## Additional: Sort author_lang_match_cor descending by column "pol" and see which languages rank highest, meaning which languages polish shares most authors with
# pol_authors_cor <- author_lang_match_matrix[order(author_lang_match_matrix$pol),c("pol","tur","hrv","nor","gre","rus","spa")]
# 
# ##Subset for authors that only the half top of languages share by betweenness score (WIP)
# pol_authors_cor_authorcount<-rowSums(pol_authors_cor)
# pol_authors_cor$authorcount <- pol_authors_cor_authorcount

##To check title counts per author and compare for polish with highest correlated languages
pol_authors_cor_titlesums <- author_lang_table
pol_authors_cor_titlesums <- tibble::column_to_rownames(pol_authors_cor_titlesums, "author")
#delete author column
pol_authors_cor_titlesums[1] <- NULL
pol_authors_cor_titlesums$na_count <- NULL

pol_authors_cor_titlesums <- pol_authors_cor_titlesums[order(pol_authors_cor_titlesums$pol),c("pol","tur","hrv","nor","gre","rus","spa")]
pol_authors_authorcount<-rowSums(pol_authors_cor_titlesums)
pol_authors_cor_titlesums$authorcount <- pol_authors_authorcount

write.csv(pol_authors_cor_titlesums, file="alldnb_lang_lang_authortitlesum_polish_ch2.2_v2.csv")

##Check which authors it shares mostly and which ones it does not!
author_lang_table_rownames <- author_lang_table
author_lang_table_rownames <- tibble::column_to_rownames(author_lang_table_rownames, "author")
#delete author column
author_lang_table_rownames[1] <- NULL
author_lang_table_rownames$na_count <- NULL
##View the title counts for those authors per language
View(author_lang_table_rownames[c("Link,Charlotte", "Courths-Mahler,Hedwig", "Vandenberg,Patricia"),])

###Structural holes, measuring constraint
const <- sort(constraint(g2, nodes = V(g2), weights = NULL))
##Brokerage potential
invConstraint <- 1.125 - const
invConstraint

write.csv(const, file="alldnb_lang_lang_author_constraint_ch2.2_v2.csv")

###For examples: Check in edgelist which languages the highest ranking share edges with 
###Check in author-frequency per language matrix (alldnb_author_language_count_ch2.1_v2.csv) which authors they are connected by


###Compare centrality scores

centralities <- cbind(deg, eigen, bet)

# Save it to your computer as a spreadsheet
write.csv(centralities, file="alldnb_lang_lang_author_centralities_ch2.2_v2.csv")

###Check correlation between different centralities

centralities_cor <- round(cor(centralities), 2)

write.csv(centralities_cor, file="alldnb_lang_lang_author_centralities_correlation_ch2.2_v2.csv")

###Author-author network communities

###3. Community detection to group languages

###3. 1. Group big graph by community

#Choose your favorite algorithm to find communities.  The algorithm below is great for large networks but only works with undirected graphs
g2_community <- fastgreedy.community(g2)

#short summary
print(g2_community)
#The length generic function call be called on communities and returns the number of communities.
length(g2_community)
#The sizes function returns the community sizes, in the order of their ids.
sizes(g2_community)
#membership gives the division of the vertices, into communities. It returns a numeric vector, one value for each vertex, the id of its community. Community ids start from one. 
membership(g2_community)

###Plot as dendogram
plot((as.dendrogram(g2_community)), labels = NULL, cex = 5, sub = NULL,
     xlab = NULL, ylab = "Height")

#Export as: lang_lang_author_network_community_dendogram.pdf

#with colored branches
dend <- as.dendrogram(g2_community)
plot(color_branches(dend, 6, groupLabels = TRUE, ylab = NULL))

#Export as: lang_lang_author_network_community_dendogram-color.pdf

###Subset by detected central community instead of above 100 authors
##Community 2 seems to have the most central languages
lang_membership <- as.vector(membership(g2_community))
lang_list <- as.vector(g2_community$names)
lang_membership <- data.frame(lang_membership, lang_list)
lang_membership_central <- filter(lang_membership, lang_membership == "2")
lang_membership_peripheral <- filter(lang_membership, lang_membership == "1")

lang_peripheral <- c(lang_membership_peripheral$lang_list)
lang_central <- c(lang_membership_central$lang_list)

write.csv(lang_peripheral, file="alldnb_lang_lang_author_community_lang_peripheral_ch2.2_v2.csv")
write.csv(lang_central, file="alldnb_lang_lang_author_community_lang_central_ch2.2_v2.csv")
write.csv(lang_membership, file="alldnb_lang_lang_author_community_lang_membership_ch2.2_v2.csv")


###3. 2. ###Casestudy: Are central languages more likely to share canonical (overrepresented) authors?

###subset edges by community of peripheral languages or add as edge attribute and count the author edges

##filter by community
peripheral_authors <- author_lang_table_rownames[,lang_peripheral]

##Which authors have the most edges per language? (most represented)
##Sum each row of the table and add to column
peripheral_authors_sum<-rowSums(peripheral_authors)
peripheral_authors$authorcount <- peripheral_authors_sum
peripheral_authors_top <- peripheral_authors %>% filter(authorcount > 10)

##which authors are the most represented across peripheral languages?
peripheral_authors$author_lang_count <- rowSums(peripheral_authors != 0)
peripheral_authors_top$author_lang_count <- rowSums(peripheral_authors_top != 0)

write.csv(peripheral_authors, file="alldnb_lang_lang_peripheral_authorsum_ch2.2_v2.csv")
write.csv(peripheral_authors_top, file="alldnb_lang_lang_peripheral_authorsum_top_ch2.2_v2.csv")

##Look at distribution of authors amongst peripheral languages
View(peripheral_authors_top)
#rank by the author_lang_count column to see which author is shared by the most peripheral languages

##Compare with central languages
##filter by community
central_authors <- author_lang_table_rownames[,lang_central]

##Which authors have the most edges per language? (most represented)
##Sum each row of the table and add to column
central_authors_sum<-rowSums(central_authors)
central_authors$authorcount <- central_authors_sum
central_authors_top <- central_authors %>% filter(authorcount > 10)

##which authors are the most represented across peripheral languages?
central_authors$author_lang_count <- rowSums(central_authors != 0)
central_authors_top$author_lang_count <- rowSums(central_authors_top != 0)

View(central_authors)

write.csv(central_authors, file="alldnb_lang_lang_central_authorsum_ch2.2_v2.csv")

View(central_authors_top)

###Correlation: See which authors or languages are most and least correlated

#language correlation
View(cor(peripheral_authors_top))

##transpose to get author correlation
peripheral_authors_top_t <- t(peripheral_authors_top)
View(cor(peripheral_authors_top_t))
peripheral_authors_top_cor<- as.data.frame(cor(peripheral_authors_top_t))

peripheral_authors_top_cor <- tibble::rownames_to_column(peripheral_authors_top_cor, "author")
#correlation coefficient between authorcount and author_lang_count
cor(peripheral_authors_top$authorcount, peripheral_authors_top[,55])

##find most correlated authors according to languages they have in common
peripheral_authors_top_cor_ranked <- cor(peripheral_authors_top_t) %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)

View(peripheral_authors_top_cor_ranked)

write.csv(peripheral_authors_top_cor_ranked, file="alldnb_lang_lang_peripheral_author_top_cor_ch2.2_v2.csv")

###Identify which authors are shared between central and peripheral languages

##Create two adjacency matrices with author-author, one for peripheral and one for cenral languages and then measure distance between
#Create two author-author adjacency matrices
central_authors_m <- central_authors
central_authors_m$authorcount <- NULL
central_authors_m$author_lang_count <- NULL

central_authors_m <- as.matrix(t(central_authors_m))
central_authors_m<-t(central_authors_m)%*%as.matrix(central_authors_m)

write.csv(central_authors_m, file="alldnb_author_author_central_matrix_ch2.2_v2.csv")

#set diagonal to zero (only sums of each language)
diag(central_authors_m)<-0
# Mean of the each row of the matrix
central_authors_m_mean<-rowMeans(central_authors_m)              
sum(central_authors_m)

peripheral_authors_m <- peripheral_authors
peripheral_authors_m$authorcount <- NULL
peripheral_authors_m$author_lang_count <- NULL

peripheral_authors_m <- peripheral_authors
peripheral_authors_m$authorcount <- NULL
peripheral_authors_m$author_lang_count <- NULL

peripheral_authors_m <- as.matrix(t(peripheral_authors_m))
peripheral_authors_m<-t(peripheral_authors_m)%*%as.matrix(peripheral_authors_m)

write.csv(peripheral_authors_m, file="alldnb_author_author_peripheral_matrix_ch2.2_v2.csv")

#set diagonal to zero (only sums of each language)
diag(peripheral_authors_m)<-0
# Mean of the each row of the matrix
central_authors_m_mean<-rowMeans(peripheral_authors_m)              
sum(peripheral_authors_m)

### Calculate common authors between center and periphery by intersection

##transfer adjacency matrix to graphs
cent_g <- graph_from_adjacency_matrix(central_authors_m, weighted=TRUE, mode = "undirected")
per_g <- graph_from_adjacency_matrix(peripheral_authors_m, weighted=TRUE, mode = "undirected")

###Save the networks
save(cent_g, file="central_network.Rda")
save(per_g, file="peripheral_network.Rda")

##graph.intersection might crash if there is too much in the working environment, so it is best to empty it and load the data
#load("central_network.Rda")
#load("peripheral_network.Rda")

##number of common edges, Common part of two social networks
int <- graph.intersection(cent_g,per_g)
##how many edges they share
ecount(per_g)+ecount(cent_g)-2*ecount(int)

#first, confirm how many nodes and edges you have
int #gives you overview of your graph
length(V(int)) #V(g) = vectors for your graph object g
length(E(int)) #E(g) = edges for your graph object g

###Save the network
save(int, file="cent_per_intersection.Rda")

cent_per_int <- as.data.frame(degree(int))
View(cent_per_int)

write.csv(cent_per_int, file="alldnb_author_author_center_periphery_intersection_ch2.2_v2.csv")

##look at top 50  authors interesting which ones are non-canonical, collect examples
View(cent_per_int)

alldnb<-read.csv("alldnb_2021_2.csv", header=T,row.names=NULL,sep=",")

suskind <- alldnb %>% 
  filter(str_detect(creator, 'Süskind, Patrick'))

View(table(suskind$language))

schulze <- alldnb %>% 
  filter(str_detect(creator, 'Schulze, Ingo'))

View(table(schulze$language))

write.csv(schulze, file="alldnb_Ingo_Schulze_titles_ch2.2_v2.csv")

zeh <- alldnb %>% 
  filter(str_detect(creator, 'Zeh, Juli'))

View(table(zeh$language))

write.csv(zeh, file="alldnb_Juli_Zeh_titles_ch2.2_v2.csv")

walser <- alldnb %>% 
  filter(str_detect(creator, 'Walser, Robert'))

View(table(walser$language))


#compare with canonical

holderlin <- alldnb %>% 
  filter(str_detect(creator, 'Hölderlin, Friedrich'))

View(table(holderlin$language))

write.csv(holderlin, file="alldnb_IFriedrich_Holderlin_titles_ch2.2_v2.csv")

erpenbeck <- alldnb %>% 
  filter(str_detect(creator, 'Erpenbeck, Jenny'))

View(table(erpenbeck$language))

write.csv(erpenbeck, file="alldnb_Juli_Zeh_titles_ch2.2_v2.csv")

##Subset by authors that don't have shared edges (0-values) and see if they are in central or peripheral group
notcommon_authors <- tibble::rownames_to_column(cent_per_int, "author")
names(notcommon_authors)[2] <- "degree_int"
notcommon_authors <- filter(notcommon_authors, degree_int == 0)

notcommon_authors_v <- notcommon_authors
notcommon_authors_v[2] <- NULL
notcommon_authors_v <- as.vector(notcommon_authors_v$author)

##see how many authors with no connecting edges between centre and periphery are in the peripheral language only

#subset df by notcommon authors
notcommon_authors_lang <- author_lang_freq[author_lang_freq$author %in% notcommon_authors_v, ]  

#subset by peripheral languages
notcommon_authors_central <- notcommon_authors_lang[notcommon_authors_lang$language %in% lang_central$x, ]  
notcommon_authors_peripheral <- notcommon_authors_lang[notcommon_authors_lang$language %in% lang_peripheral$x, ] 

write.csv(notcommon_authors_central, file="20220627_alldnb_notcommon_authors_central_ch2.2_v2.csv")
write.csv(notcommon_authors_peripheral, file="20220627_alldnb_notcommon_authors_peripheral_ch2.2_v2.csv")

notcommon_authors_central_unique <- unique(notcommon_authors_central$author)
notcommon_authors_peripheral_unique <- unique(notcommon_authors_peripheral$author)

##Ratio of central versus peripheral

length(notcommon_authors_central_unique)+length(notcommon_authors_peripheral_unique)
##3176 unique authors not shared
length(notcommon_authors_v)
#in percentages
length(notcommon_authors_central_unique)/((length(notcommon_authors_central_unique)+length(notcommon_authors_peripheral_unique)))
#78.5% are in central language group, 31.5% only in peripheral group
#language distributions
table(notcommon_authors_central$language)
table(notcommon_authors_peripheral$language)


##Subset by authors that have shared edges (non-0-values) and see if they are in central or peripheral group

common_authors <- tibble::rownames_to_column(cent_per_int, "author")
names(common_authors)[2] <- "degree_int"
common_authors <- filter(common_authors, degree_int != 0)

write.csv(common_authors, file="alldnb_common_authors_centralandperipheral_ch2.2_v2.csv")

common_authors_v <- common_authors
common_authors_v[2] <- NULL
common_authors_v <- as.vector(common_authors_v$author)

##see how many authors with connecting edges between centre and periphery are in the peripheral language only

#subset df by common authors
common_authors_lang <- author_lang_freq[author_lang_freq$author %in% common_authors_v, ] 

#subset by peripheral languages
common_authors_central <- common_authors_lang[common_authors_lang$language %in% lang_central$x, ]  
common_authors_peripheral <- common_authors_lang[common_authors_lang$language %in% lang_peripheral$x, ] 

write.csv(common_authors_central, file="20220627_alldnb_common_authors_central_ch2.2_v2.csv")
write.csv(common_authors_peripheral, file="20220627_alldnb_common_authors_peripheral_ch2.2_v2.csv")

##Get percentage of shared authors that are mostly central vs peripheral
common_authors_central_unique <- unique(common_authors_central$author)
length(common_authors_central_unique)
##805 shared authors are predominantly in the central language group
common_authors_peripheral_unique <- unique(common_authors_peripheral$author)
length(common_authors_peripheral_unique)
##805 shared authors are predominantly in the peripheral language group
##Ratio of central versus peripheral

length(common_authors_central_unique)+length(common_authors_peripheral_unique)
##n=1610 shared authors
length(common_authors_v)
##50/50


###Compare two languages (English and HUngarian) and see if translated authors match
eng_hun_match <- ifelse(author_lang_table[['eng']] > author_lang_table[['hun']], 'greater',
                        ifelse(author_lang_table[['eng']] < author_lang_table[['hun']], 'lower', 'same'))

eng_hun_match <- as.data.frame(eng_hun_match)

#add authors
eng_hun_match <- cbind(eng_hun_match, author = author_lang_table$author)

##Add frequency values to eng_hun_match

##Subset by English
author_freq_eng <- as.data.frame(author_lang_table[, "eng"])
author_freq_hun <- as.data.frame(author_lang_table[, "hun"])

eng_hun_match <- as.data.frame(cbind(author_freq_eng$`author_lang_table[, "eng"]`, eng_hun_match))
eng_hun_match <- as.data.frame(cbind(author_freq_hun$`author_lang_table[, "hun"]`, eng_hun_match))

colnames(eng_hun_match)[1] <- "hun"
colnames(eng_hun_match)[2] <- "eng"

write.csv(eng_hun_match, file="alldnb_lang_lang_author_degree_match_hunvseng_ch2.2_v2.csv")

##see how many authors match
View(table(eng_hun_match$eng_hun_match))

##Pick specific author and find value
author_lang_table[author_lang_table$author == "Kehlmann,Daniel", "eng"]

###Where is the border between translational and canonical? Community detection of authors
#for peripheral language authors
per_community <- fastgreedy.community(per_g)
#for central language authors
cent_community <- fastgreedy.community(cent_g)

#short summary
print(per_community)
#The length generic function call be called on communities and returns the number of communities.
length(per_community)
#The sizes function returns the community sizes, in the order of their ids.
sizes(per_community)
#membership gives the division of the vertices, into communities. It returns a numeric vector, one value for each vertex, the id of its community. Community ids start from one. 
View(membership(per_community))

per_membership <- as.vector(membership(per_community))
per_list <- as.vector(per_community$names)
per_membership <- data.frame(per_membership, per_list)

write.csv(per_membership, file="alldnb_author_membership_ch2.2_v2.csv")

###Look which communities translational authors are in
filter(per_membership, per_list == "Süskind,Patrick")
filter(per_membership, per_list == "Zeh,Juli")
filter(per_membership, per_list == "Erpenbeck,Jenny")
filter(per_membership, per_list == "Schulze,Ingo")
##Group 2 appears to be translational
##Check which group canonical authors are in
filter(per_membership, per_list == "Hölderlin,Friedrich")
filter(per_membership, per_list == "Goethe,JohannWolfgangvon")
##These authors appear to be in group 1, while some other canonical authors are in group 2
filter(per_membership, per_list == "Kafka,Franz")
filter(per_membership, per_list == "Böll,Heinrich")
filter(per_membership, per_list == "Mann,Thomas")

##We can see that the largest groups are 1-5 while the rest of groups only have one author each, which can be grouped together
per_membership_rest<- per_membership %>% filter(!str_detect(per_membership, '1|2|3|4|5'))

##Look at the authors in the largest groups
per_membership_1<- filter(per_membership, per_membership == "1")
per_membership_2 <- filter(per_membership, per_membership == "2")
per_membership_3 <- filter(per_membership, per_membership == "3")
per_membership_4 <- filter(per_membership, per_membership == "4")
per_membership_5 <- filter(per_membership, per_membership == "5")







###NOT INCLUDED BUT INTERESTING#################################################################################

# Strength
# This incorporates edge weights into the centrality calculation
# The stronger the edge weight the "stronger" the connection. So it's degree plus weight.
graph.strength(g2, vids=V(g2), mode = c("all"))
sort(graph.strength(g2, vids=V(g2), mode = c("all")), TRUE) #[1:10]
strong<-sort(graph.strength(g2, vids=V(g2), mode = c("all")), TRUE)
write.csv(strong, file="alldnb_lang_lang_author_strength_ch2.2_v2.csv")

### Closeness
###Did not use because it only works for connected graphs, but my network is disconnected
##Closeness centrality measures how many steps is required to access every other vertex from a given vertex.
closeness(g2, vids=V(g2), mode = c("all"), weights = NULL, normalized = FALSE)
close<-sort(closeness(g2, vids=V(g2), mode = c("all"), weights = NULL, normalized = FALSE), TRUE)
write.csv(close, file="alldnb_lang_lang_author_closeness_ch2.2_v2.csv")

#Plot for complete network
close_g2 <- closeness(g2, v = V(g2), mode = "all")
V(g2)$size <- (close_g2) #size the network nodes by their node degree 
plot(g2, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2), vertex.size= (closeness(g2, v = V(g2), mode = "all"))*5000,
     main="Language Network \nby shared authors", sub= "Closeness Centrality")

##Zoom and save as: alldnb_lang_lang_author_closeness_ch2.2_v2.png

#visualize for subset
close_g2_above100 <- closeness(g2_above100, v = V(g2_above100), mode = "all")
write.csv(close_g2_above100, file="alldnb_lang_lang_author_above100_closeness_ch2.2_v2.csv")

V(g2_above100)$size <- (close_g2_above100*10000) #size the network nodes by their node degree 
plot(g2_above100, edge.arrow.size = .5, vertex.color = "gold",layout=layout.kamada.kawai(g2_above100), vertex.size= close_g2_above100*100000,
     main="Language Network \nby shared authors (>100)", sub= "Closeness Centrality")

#Save as: lang_lang_author_network_above100_closeness.png


###Reach
#see: https://rstudio-pubs-static.s3.amazonaws.com/313180_80f7ae52790a44afb2acdc00a6e1be07.html
# Function for 2-step reach
reach2<-function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,2,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

# Function for 3-step reach
reach3<-function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,3,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

# Now, run the calculations.
Reach_2 <- reach2(g2)    # Note the differences between the object
Reach_3 <- reach3(g2)    #   names and the function names!

#Robustness
#this tells you how many random nodes need to be deleted before your graph breaks in 2
# we run it 1000 times and take the avg. (so it may take a few minutes)
robustness_vector<-vector()
for (j in 1:100) {
  random.nodes<-sample(V(g2)$name, length(V(g2)$name), replace = FALSE)
  g.robust<-as.undirected(g2)
  for (i in 1:length(V(g2))) {
    g.robust<-delete.vertices(g.robust, v=random.nodes[i])
    if (is.connected(g.robust)==FALSE)
      break
  }
  robustness_vector<-append(robustness_vector, i/length(V(g2)))
}
# it then prints the mean and standard deviation values
# the mean is the percentage of nodes that on average needed to be removed to split it in 2
# this is a measure of how densely connected it is
mean(robustness_vector) 
sd(robustness_vector)

# Vulnerability
# this is a similar process, but instead of removing random nodes
# we take out the strongest nodes in order of importance by degree
deg_vul<-names(sort(degree(g2),TRUE))
g2.vulnerable<-as.undirected(g2)
for (i in 1:length(V(g2))) {
  g2.vulnerable<-delete.vertices(g2.vulnerable, v=as.character(deg_vul[[i]]))
  if (is.connected(g2.vulnerable)==FALSE)
    break
}
#it prints the number of nodes needed to be removed
print(i)  
#and the percentage of the network
print(vulnerability<-i/length(V(g2)))

###Another option to cluster by community:
#Multilevel Community Detection
multilevel.community (g, weights = NULL)
comm1<-multilevel.community (g, weights = NULL)
#to get the modularity score (how well the communities are defined)
comm1$modularity
#to extract all the communities and add them to your node labels
mem1<-comm1$membership
write.csv(mem1, file="Sontag_CommunityLabels_Multilevel.csv")

#leading eigenvector method. usually better for very large graphs
leading.eigenvector.community(g2, steps = -1, start = NULL, options = igraph.arpack.default, callback = NULL, extra = NULL, env = parent.frame())
#if you want to save the membership list do this:
eigen_community<-leading.eigenvector.community(g2, steps = -1, start = NULL, options = igraph.arpack.default, callback = NULL, extra = NULL, env = parent.frame())
eigen_community$modularity
eigen_member<-eigen_community$membership

write.csv(mem2, file=".csv")

#Collapse the graph by communities.(DOES NOT WORK)  This insight is due to this post http://stackoverflow.com/questions/35000554/collapsing-graph-by-clusters-in-igraph/35000823#35000823
# res_g2_community <- simplify(contract(g2, membership(g2_community))) 
##The result of this process is the below figure, where the vertices' names represent community membership.
# plot(res_g2_community)
###I am not sure how to plot this, it does not seem to work

###Another option to detect communities (for comparison) too large communities does not work well
# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(g2) 
dendPlot(ceb, mode="hclust")
plot(ceb, g2) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, net)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is


##Option 2: Create a subset of authors by filtering by high betweenness and low constraint
#cbind const and bet and test correlation, look at top authors for least correlated

const_bet_lang <- cbind(bet, const)

##Since we need to test if there is a relation between the two centrality values, so we test if there is a relationship betwene the two with Chi-square
const_bet_lang_chisq <-chisq.test(const_bet_lang)

##Residuals: A residual is the difference between the observed and expected values for a cell. The larger the residual, the greater the contribution of the cell to the magnitude of the resulting chi-square obtained value.
round(const_bet_lang_chisq$residuals,2)
const_bet_lang_chisq_res <- as.data.frame(const_bet_lang_chisq$residuals)
View(const_bet_lang_chisq_res)

ggplot(const_bet_lang_chisq_res, aes(x = bet, y = const, label=rownames(const_bet_lang_chisq_res))) +  # Set up canvas with outcome variable on y-axis
  ggrepel::geom_text_repel(size = 2, box.padding = 0.05, max.overlaps = 100 )+
  ggtitle("Chi Square Test Residuals for Publisher \nand Title Count per Language")+
  labs(x = "Residuals for publisher count", y = "Residuals for title count")+
  theme_bw()



############### ADDITIONAL

#interactive graph
vis.nodes <- nodes
vis.nodes$label  <- vis.nodes$id
vis.nodes$title  <- vis.nodes$id
visNetwork(vis.nodes, lang_lang_author_edges, width="100%", height="400px")

vis.nodes <- unique(lang_lang_author_edges_above100$from)
vis.nodes <- cbind(unique(lang_lang_author_edges_above100$to))
vis.nodes <- as.data.frame(vis.nodes)
names(vis.nodes)[1] <- "id" 
vis.nodes$label  <- vis.nodes$id
vis.nodes$title  <- vis.nodes$id
names(lang_lang_author_edges_above100)[3] <- "value"
visNetwork(vis.nodes, lang_lang_author_edges_above100, width="100%", height="400px",
           main="Language Network", submain="Ties represent languages weighted by sums of shared authors")

##visualize all!too big!not stabilizing
lang_lang_author_edges_abovemean <- lang_lang_author_edges %>% filter_all(all_vars(. > cut.off))
vis.nodes <- unique(lang_lang_author_edges_abovemean$from)
vis.nodes <- cbind(unique(lang_lang_author_edges_abovemean$to))
vis.nodes <- as.data.frame(vis.nodes)
names(vis.nodes)[1] <- "id"
vis.nodes$label  <- vis.nodes$id
vis.nodes$title  <- vis.nodes$id
names(lang_lang_author_edges_abovemean)[3] <- "value"

visNetwork(vis.nodes, lang_lang_author_edges_abovemean, width="100%", height="400px")


###Compare networks with QAP test

# option 1: create an array of keyword matrices to compare using QAP

netArray <- array(dim=c(2,length(authorNames),length(authorNames)))
netArray[1,,] <- central_authors_m
netArray[2,,] <- peripheral_authors_m

# perform the QAP comparison (TAKES TOO LONG)
# centralVsperipheral <- qaptest(netArray,gcor,g1=1,g2=2)

###try correlation instead
centralVsperipheral_gcor <- gcor(netArray, g1=1, g2=2)

# option 2: create unique author and language objects
authorNames <- sort(unique(author_lang_freq$author))
languageNames <- sort(unique(author_lang_freq$language))

# create unique collocate and keyword objects
collocateNames <- sort(unique(myData$col))
keywordNames <- sort(unique(myData$collocates.keyword))

#### Make the keyword by collocate matrix
keyByCol <- as.matrix(do.call(cbind,
                              lapply(as.list(languageNames),
                                     function(x){
                                       as.numeric(authorNames %in% author_lang_freq$author[which(author_lang_freq$language==x)])})))
k1 <- lapply(as.list(languageNames),
             function(x){
               as.numeric(authorNames %in% author_lang_freq$author[which(author_lang_freq$language==x)])})
keyByCol <- as.matrix(do.call(cbind,k1))
# force the names of the matrices to reflect the keywords and collocates
names(keyByCol) <- keywordNames
row.names(keyByCol) <- collocateNames


# keyByCol <- matrix(NA,nrow=length(keywordNames),ncol=length(collocateNames))


### remove duplicate rows
subset(myData, !duplicated(myData)) 

# Make the keyword by collocate matrix
keyByCol <- as.matrix(do.call(cbind,
                              lapply(as.list(keywordNames),
                                     function(x){
                                       as.numeric(collocateNames %in% myData$col[which(myData$collocates.keyword==x)])})))
k1 <- lapply(as.list(keywordNames),
             function(x){
               as.numeric(collocateNames %in% myData$col[which(myData$collocates.keyword==x)])})
keyByCol <- as.matrix(do.call(cbind,k1))
# force the names of the matrices to reflect the keywords and collocates
names(keyByCol) <- keywordNames
row.names(keyByCol) <- collocateNames

# use matrix multiplication to make the keyword by keyword matrix
keyByKey <- keyByCol %*% t(keyByCol)

# or do the same for subsets of the data of your choice. For example, here, by sentiment
keyByKeyPos <- keyByCol[which(names(keyByCol) %in% myData$col[which(myData$sent==”pos”)])] %*%
  t(keyByCol[which(names(keyByCol) %in% myData$col[which(myData$sent==”pos”)])])

keyByKeyNeg <- keyByCol[which(names(keyByCol) %in% myData$col[which(myData$sent==”neg”)])] %*%
  t(keyByCol[which(names(keyByCol) %in% myData$col[which(myData$sent==”neg”)])])

# create an array of keyword matrices to compare using QAP
keyArray <- array(dim=c(2,length(keywordNames),length(keywordNames)))
keyArray[1,,] <- keyByKeyPos
keyArray[2,,] <- keyByKeyNeg

# perform the QAP comparison
posVsNeg <- qaptest(keyArray,gcor,g1=1,g2=2)

###Distance between peripheral and central networks
library(NetworkDistance)

## load example data
data(g2)
## compute distance matrix
output = nd.edd(g2, out.dist=FALSE)
## visualize
opar <- par(no.readonly=TRUE)
par(pty="s")
image(output$D[,20:1], main="two group case", axes=FALSE, col=gray(0:32/32))
par(opar)

##Compare the two matrices by distance
library(NetworkDistance)

A = list()
for (i in 1:10){A[[i]]=peripheral_authors_m} # first 3 are type-1
for (i in 11:20){A[[i]]=central_authors_m} # next  3 are type-2

## Compute Distance Matrix and Visualize
output = nd.edd(A, out.dist=FALSE)
image(output$D, main="two group case")

## Compute Distance Matrix and Visualize
output = nd.gdd(A)
image(as.matrix(output$D), main="two group case")
## End(Not run)

out1 <- nd.centrality(A,out.dist=FALSE,mode="Degree")
out2 <- nd.centrality(A,out.dist=FALSE,mode="Close")
out3 <- nd.centrality(A,out.dist=FALSE,mode="Between")
## visualize
par(mfrow=c(1,3))
image(out1$D, main="Degree")
image(out2$D, main="Closeness")
image(out3$D, main="Betweenness")
