#####
# SNA on KZN 26 cellphone, phonebook, and acquaintance records
# Should be run after "KZN26 Data Curation.R"
#
# Data source: Court record - Thereza Botha, directly sourced from Vodacom & MTN through the court
# Data includes calls from 1 - 4 Oct 2006, Contact lists, Testimonial evidence
#
# Copyright - Annie Kok - anniekok3@gmail.com
#####

options(digits = 2)

# Packages ####
library(tidyverse)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(corrplot)
library(ggeasy)
library(ggpubr)
library(migraph)
library(manynet) 
library(igraph)

#   1.    First create all the required network objects
#   1.1   Create Data subsets ####
# Create directed and weighted subgraph by filtering out other types
# Rearrange edgelist so that sender and receiver are the first columns to create symbolic edgelist
edgelist <- select(edgelist, sender, receiver, everything())

# First add colours to each node type for plotting later
all_types <- sort(unique(ph_all_nodes$Type))
# Assign a specific colour to each
# This is hardcoded!! Would need to reconsider if any new types are added
cols <- c('#81d22f', '#0004ff', '#fa4ecd',  '#fa8611', '#85f2f2', '#d40000' , '#c8c7c7', '#1293ff')
#          "Acc"      "Ass"      "Not"       "Ph_Unk"   "SAP"      "Sus"       "Tax"      "Unk" 
ind <- match(all_nodes$Type, all_types)
ph_ind <- match(ph_all_nodes$Type, all_types)
all_nodes <- mutate(all_nodes, colour = cols[ind])
ph_all_nodes <- mutate(ph_all_nodes, colour = cols[ph_ind]) %>% 
  filter(!(Type == 'Unknown' & !Id %in% phonecontactlinks$contact))

# Create acc-only links
acc_links <- edgelist %>% filter(str_detect(sender, '^Acc') & str_detect(receiver, '^Acc'))

# Create acc and ass-only links
accass_links <- edgelist %>% filter(str_detect(sender, '^Acc|^Ass') & str_detect(receiver, '^Acc|^Ass'))

# Create accass and acc_nodes list
accass_nodes <- all_nodes %>% filter(str_detect(Type, '^Acc|^Ass'))
acc_nodes <- all_nodes %>% filter(str_detect(Type, 'Acc'))

# Create person of interest list filtering out unknowns
# See Section 7.1.2 where we determine who is added as a POI
unk_poi_calls <- c('Unk1', 'Unk141', 'Unk206', 'Unk721', 'Unk107', 'Unk39', 'Unk2')
poi_links <- edgelist %>% 
  filter((str_detect(sender, 'Unk', negate = T) | str_detect(sender, paste0("\\b(", paste(unk_poi_calls, collapse = "|"), ")\\b"))) &
  (str_detect(receiver, 'Unk', negate = T) | str_detect(receiver, paste0("\\b(", paste(unk_poi_calls, collapse = "|"), ")\\b"))))
#Messy! But it works.

# filter out taxi driver and SAPS handler, Noted2 (no contact). Decided to keep Acc10 for fair comparison with other networks
poi_links <- poi_links %>% 
  filter(str_detect(sender, 'Tax', negate = T) & str_detect(receiver, 'Tax', negate = T) & str_detect(sender, 'SAP', negate = T)& str_detect(receiver, 'SAP', negate = T)) 
  
poi_nodes <- all_nodes %>% filter(str_detect(Type, 'Unk', negate = T) | 
                                    str_detect(Id, paste0("\\b(", paste(unk_poi_calls, collapse = "|"), ")\\b")))
poi_nodes <- poi_nodes %>% filter(str_detect(Type, 'SAP', negate = T) & 
                                    str_detect(Type, 'Tax', negate = T) &  # filter out taxi and saps handler
                                    str_detect(Id, 'Noted2', negate = T)) #&               
                                    #str_detect(Id, 'Acc10', negate = T))  # filter out Noted2 en Acc10                    

# Create filtered Phonebook contacts for Accused 
acc_ph_links <- phonecontactlinks %>% filter(str_detect(source, '^Acc'), str_detect(contact, '^Acc'))
acc_ph_nodes <- ph_all_nodes %>% filter(str_detect(Type, '^Acc'))

# Create filtered Phonebook contacts for Accused AND Associates
accass_ph_links <- phonecontactlinks %>% filter(str_detect(source, '^Acc|^Ass'), str_detect(contact, '^Acc|^Ass'))
accass_ph_nodes <- ph_all_nodes %>% filter(str_detect(Type, '^Acc|^Ass'))

# Create filtered ponebook contacts for all persons of interest
poi_ph_links <- phonecontactlinks %>% 
  filter(str_detect(contact, '^Unk', negate = T) | 
           str_detect(contact, paste0("\\b(", paste(unk_poi_calls, collapse = "|"), ")\\b"))) %>% 
  filter(str_detect(contact, '^Ph', negate = T))
poi_ph_links <- poi_ph_links %>% filter(str_detect(contact, '^Tax', negate = T) &  str_detect(contact, '^SAP', negate = T)) # negate saps handler and taxi driver

poi_ph_nodes <- ph_all_nodes %>% 
  filter(str_detect(Id, '^Unk', negate = T) | 
           str_detect(Id, paste0("\\b(", paste(unk_poi_calls, collapse = "|"), ")\\b"))) %>% 
  filter(Type != 'Phonebook Contact')

poi_ph_nodes <- poi_ph_nodes %>% 
  filter(str_detect(Id, '^Tax', negate = T) & str_detect(Id, '^SAP', negate = T)) # remove SAPShandler and taxi driver from poi ph

# Create robbery-time links
robbery_links <- edgelist %>% filter(datetime > '2006-10-02 16:33:57', datetime < '2006-10-02 20:08:06')
robbery_links <- robbery_links %>% 
  filter(str_detect(sender, '^Acc') & str_detect(receiver, '^Acc')) # include only acc

#   1.2   Collapse multiple edges ####
# Complete network
all_lcollapse <- edgelist %>%  
  group_by(sender, receiver) %>% 
  summarise(weight = n())

# Accused network
acc_lcollapse <- acc_links %>%  
  group_by(sender, receiver) %>% 
  summarise(weight = n())

# Accused ROBBERY network
robbery_lcollapse <- robbery_links %>%  
  group_by(sender, receiver) %>% 
  summarise(weight = n())

# Accused and associates network
accass_lcollapse <- accass_links %>%  
  group_by(sender, receiver) %>% 
  summarise(weight = n())

# Persons of interest network
poi_lcollapse <- poi_links %>%  
  group_by(sender, receiver) %>% 
  summarise(weight = n())

#   1.3   Create Network Objects: normal, weighted and undirected ####

# Create network dataframes (as is)
complete_net <- graph_from_data_frame(d = edgelist, vertices = all_nodes, directed = T)             # complete network object
acc_net <- graph_from_data_frame(d = acc_links, vertices = acc_nodes, directed = T)                 # accused-only network object
accass_net <- graph_from_data_frame(d = accass_links, vertices = accass_nodes, directed = T)        # accused and associates network object
poi_net <- graph_from_data_frame(d = poi_links, vertices = poi_nodes, directed = T)                 # persons of interest network object
acq_net <- graph_from_data_frame(d = acq, vertices = acc_nodes, directed = T)                       # accused testimony network object
court_cont_net <- graph_from_data_frame(d = hofcontactslinks, vertices = accass_nodes, directed = T)# acc&ass contacts as per court network object
phonebook_net <- graph_from_data_frame(d = phonecontactlinks, vertices = ph_all_nodes, directed = T)# complete ph network object
acc_ph_net <- graph_from_data_frame(d = acc_ph_links, vertices = acc_ph_nodes, directed = T)        # accused ph contacts network object
accass_ph_net <- graph_from_data_frame(d = accass_ph_links, vertices = accass_ph_nodes, directed = T) # accused and ASS ph contacts network object
poi_ph_net <- graph_from_data_frame(d = poi_ph_links, vertices = poi_ph_nodes, directed = T)        # POI contacts network object
acc_robbery <- graph_from_data_frame(d = robbery_links, vertices = acc_nodes, directed = T)         # network object during robbery

# Create network WEIGHTED dataframes (collapsed but directed)
completeW_net <- graph_from_data_frame(d = all_lcollapse, vertices = all_nodes, directed = T)       # complete network object
accW_net <- graph_from_data_frame(d = acc_lcollapse, vertices = acc_nodes, directed = T)            # accused-only network object
accassW_net <- graph_from_data_frame(d = accass_lcollapse, vertices = accass_nodes, directed = T)   # accused and associates network object
poiW_net <- graph_from_data_frame(d = poi_lcollapse, vertices = poi_nodes, directed = T)            # persons of interest network object
robberyW_net <- graph_from_data_frame(d = robbery_lcollapse, vertices = acc_nodes, directed = T)    # accused-only network object

# Create network Weighted and undirected (links summed)

completeW_UD <- as.undirected(complete_net, mode = "collapse", edge.attr.comb = list(weight = "sum")) # complete network undirected
accW_UD <- as.undirected(acc_net, mode = "collapse", edge.attr.comb = list(weight = "sum"))           # accused-only network undirected            
accassW_UD <- as.undirected(accass_net, mode = "collapse", edge.attr.comb = list(weight = "sum"))     # accused and associates network network undirected            
poiW_UD <- as.undirected(poi_net, mode = "collapse", edge.attr.comb = list(weight = "sum"))           # persons of interest network network undirected            
acqW_UD <- as.undirected(acq_net, mode = "collapse", edge.attr.comb = list(weight = "sum"))           # acquaintances network undirected            
courtW_contact_UD <- as.undirected(court_cont_net, mode = "collapse")                                 # court contacts network undirected (multiple links between the same nodes = irrelevant)            
phonebookW_UD <- as.undirected(phonebook_net, mode = "collapse")                                      # phonebook network undirected (multiple links between the same nodes = irrelevant)
acc_ph_UD <- as.undirected(acc_ph_net, mode = "collapse")                                             # Accused ph contacts network object undirected
accass_ph_UD <- as.undirected(accass_ph_net, mode = "collapse")                                       # Accused and Ass ph contacts network object undirected
poi_ph_UD <- as.undirected(poi_ph_net, mode = "collapse")                                             # POI ph contacts network object undirected
robbery_UD <- as.undirected(acc_robbery, mode = "collapse")                                           # POI ph contacts network object undirected

#   1.4   Remove loops ####
# Simplify network to remove loops on weighted network
completeW_nloop <- as.directed(simplify(completeW_net, remove.multiple = F, remove.loops = T))   # complete weighted network without loops  
accW_nloop <- as.directed(simplify(accW_net, remove.multiple = F, remove.loops = T))             # accused-only weighted network without loops  
accassW_nloop <- as.directed(simplify(accassW_net, remove.multiple = F, remove.loops = T))       # accused and associates weighted network network without loops 
poiW_nloop <- as.directed(simplify(poiW_net, remove.multiple = F, remove.loops = T))             # persons of interest weighted network without loops 
# acq_net has no loops
phonebookW_nloop <- as.directed(simplify(phonebook_net, remove.multiple = T, remove.loops = T))  # phonebook network without loops and multiple links removed

# Simply standard network to remove loops
complete_nloop <- as.directed(simplify(complete_net, remove.multiple = F, remove.loops = T))   # complete weighted network without loops  
acc_nloop <- as.directed(simplify(acc_net, remove.multiple = F, remove.loops = T))             # accused-only weighted network without loops  
accass_nloop <- as.directed(simplify(accass_net, remove.multiple = F, remove.loops = T))       # accused and associates weighted network network without loops 
poi_nloop <- as.directed(simplify(poi_net, remove.multiple = F, remove.loops = T))             # persons of interest weighted network without loops 
# acq_net has no loops
phonebook_nloop <- as.directed(simplify(phonebook_net, remove.multiple = T, remove.loops = T)) # phonebook network without loops and multiple links removed
acc_ph_net_nloop <- as.directed(simplify(acc_ph_net, remove.multiple = F, remove.loops = T))   # acc phonebook network without loops
accass_ph_net_nloop <- as.directed(simplify(accass_ph_net, remove.multiple = F, remove.loops = T))   # acc phonebook network without loops
poi_ph_net_nloop <- as.directed(simplify(poi_ph_net, remove.multiple = F, remove.loops = T))   # acc phonebook network without loops

# Undirected, weighted network to remove loops
completeW_UD_nloop <- simplify(completeW_UD, remove.multiple = T, remove.loops = T)   # complete weighted network without loops  
accW_UD_nloop <- simplify(accW_UD, remove.multiple = T, remove.loops = T)             # accused-only weighted network without loops  
accassW_UD_nloop <- simplify(accassW_UD, remove.multiple = T, remove.loops = T)       # accused and associates weighted network network without loops 
poiW_UD_nloop <- simplify(poiW_UD, remove.multiple = T, remove.loops = T)             # persons of interest weighted network without loops 
# acq_net has no loops
phonebookW_UD_nloop <- simplify(phonebookW_UD, remove.multiple = T, remove.loops = T) # phonebook network without loops and multiple links removed
acc_ph_UD_nloop <- simplify(acc_ph_UD, remove.multiple = F, remove.loops = T)         # Undirected acc ph network without loops 
accass_ph_UD_nloop <- simplify(accass_ph_UD, remove.multiple = F, remove.loops = T)   # Undirected acc ph network without loops 
poi_ph_UD_nloop <- simplify(poi_ph_UD, remove.multiple = F, remove.loops = T)         # Undirected acc ph network without loops 

#   1.5   Remove isolates ####
# Detached the graphs to remove any isolated nodes from the Weighted and loopless network
completeW_nloop_dg <- decompose.graph(completeW_nloop)[[1]]  # detached graph 1 from list of four graphs
accW_nloop_dg <- decompose.graph(accW_nloop)[[1]]            # detached graph 1 from list of two graphs
accassW_nloop_dg <- decompose.graph(accassW_nloop)[[1]]      # detached graph 1 from list of two graphs
poiW_nloop_dg <- decompose.graph(poiW_nloop)[[1]]            # detached graph 1 from list of two graphs
acq_dg <- decompose.graph(acq_net)[[1]]                      # detached graph 1 from list of two graphs

# Detached the graphs to remove any isolated nodes from the loopless standard network
complete_nloop_dg <- decompose.graph(complete_nloop)[[1]]    # detached graph 1 from list of four graphs
acc_nloop_dg <- decompose.graph(acc_nloop)[[1]]              # detached graph 1 from list of two graphs
accass_nloop_dg <- decompose.graph(accass_nloop)[[1]]        # detached graph 1 from list of two graphs
poi_nloop_dg <- decompose.graph(poi_nloop)[[1]]              # detached graph 1 from list of two graphs

phonebook_nloop_dg <- decompose.graph(phonebook_nloop)[[1]]          # detached graph 1 from list of 570 graphs
acc_ph_net_nloop_dg <- decompose.graph(acc_ph_net_nloop)[[1]]        # detached graph 1 from list of 3 graphs
accass_ph_net_nloop_dg <- decompose.graph(accass_ph_net_nloop)[[1]]  # detached graph 1 from list of 3 graphs
poi_ph_net_nloop_dg <- decompose.graph(poi_ph_net_nloop)[[1]]        # detached graph 1 from list of 4 graphs

# Detached the UD graphs to remove any isolated nodes undirected and weighted network
completeW_UD_nloop_dg <- decompose.graph(completeW_UD_nloop)[[1]]   # detached graph 1 from list of four graphs
accW_UD_nloop_dg <- decompose.graph(accW_UD_nloop)[[1]]             # detached graph 1 from list of two graphs
accassW_UD_nloop_dg <- decompose.graph(accassW_UD_nloop)[[1]]       # detached graph 1 from list of two graphs
poiW_UD_nloop_dg <- decompose.graph(poiW_UD_nloop)[[1]]             # detached graph 1 from list of 4 graphs

phonebookW_UD_nloop_dg <- decompose.graph(phonebookW_UD_nloop)[[1]] # detached graph 1 from list of 570 graphs
accass_ph_UD_nloop_dg <- decompose.graph(accass_ph_UD_nloop)[[1]]   # detached graph 1 from list of 3 graphs
acc_ph_UD_nloop_dg <- decompose.graph(acc_ph_UD_nloop)[[1]]         # detached graph 1 from list of 3 graphs
accass_ph_UD_nloop_dg <- decompose.graph(accass_ph_UD_nloop)[[1]]   # detached graph 1 from list of 3 graphs
poi_ph_UD_nloop_dg <- decompose.graph(poi_ph_UD_nloop)[[1]]         # detached graph 1 from list of 3 graphs

# 7.1     Network-level Analysis
# Section 5.1.1   Accused Network Metrics ####

## No. of Nodes and Links
 V(acq_net)              # all nodes based on testimonial evidence 
 nrow(acc_ph_nodes)      # acc nodes filtered from phonebook contacts
 nrow(acc_nodes)         # all accused nodes, including Acc_10
 
 E(acq_net)              # all links between acc as testified in court
 nrow(acc_ph_links)      # acc links filtered from phonebook contacts
 nrow(acc_links)         # all accused links, minus Acc_10 (no number)

## Disconnected Components
 clusters(acq_net)
 clusters(acc_ph_net_nloop)
 clusters(acc_net)

## Plot according to type

### Generate acquaintance network based on Type (use standard network, because this network has no loops or multiple edges)
type_acq <- vertex_attr(acq_net, "Type")

pdf('acqType.pdf')
set.seed(1111)
plot(acq_net,  
     vertex.size = 12,
     vertex.label = V(acq_net)$name, vertex.label.cex = 0.6, vertex.label.color = 'black',
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
     layout=layout_nicely(acq_net),
     vertex.color = V(acq_net)$colour)
dev.off()

### Generate acc phonebook contacts network based on Type
type_acc_pb <- vertex_attr(acc_ph_net_nloop, "Type")

pdf('AccPH_Typedg.pdf')
set.seed(4)
plot(acc_ph_net_nloop_dg,  
     vertex.size = 12,
     vertex.label = V(acc_ph_net_nloop_dg)$name, vertex.label.cex = 0.6, vertex.label.color = 'black',
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
     layout=layout_nicely(acc_ph_net_nloop_dg),
     vertex.color = V(acc_ph_net_nloop_dg)$colour)
dev.off()

### Generate Acc network based on Type  
type_acc <- vertex_attr(accW_nloop_dg, "Type")

pdf('AccTypedg.pdf')
set.seed(13)
plot(accW_nloop_dg,  
     vertex.size=12,
     vertex.label = V(accW_nloop_dg)$name, vertex.label.cex = 0.6, vertex.label.color = 'black',
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
     layout=layout.graphopt(accW_nloop_dg),
     vertex.color = V(accW_nloop_dg)$colour)
dev.off()


### Network Density
# Multiple edges are counted as separate edges when calculating the density, so simplify first and remove isolates. 
edge_density(as.directed(simplify(acq_net, remove.multiple = T, remove.loops = T)), loops = F) #Acq Density
edge_density(acc_ph_net_nloop, loops = F)                                                      #Acc_PB Density
edge_density(as.directed(simplify(acc_net, remove.multiple = T, remove.loops = T)), loops = F) #Acc Density

strength(acc_net, mode = 'in', loops = F)

 ### Average Degree -
# Calculate the total degree. Must simplify the network before calculating the degree, because multiple edges should be counted as a single edge.
# remove isolates - in particular 10, because node is not present to begin with.
acqdeg <- igraph::degree(acq_net, mode = "total", loop = F) # isolates not removed, because all who are named are included
acc_phdeg <- igraph::degree(acc_ph_UD_nloop, mode = "total") # acc10 not present and we know acc4 isolated.
accdeg <- igraph::degree(accW_UD_nloop, mode = "total", loop = F) # 
acqindeg <- igraph::degree(acq_net, mode = "in", loop = F)
acqoutdeg <- igraph::degree(acq_net, mode = "out", loop = F)

acc_phindeg <- igraph::degree(acc_ph_net_nloop, mode = "in", loop = F)
acc_phoutdeg <- igraph::degree(acc_ph_net_nloop, mode = "out", loop = F)

accindeg <- igraph::degree(accW_nloop, mode = "in", loop = F)
accoutdeg <- igraph::degree(accW_nloop, mode = "out", loop = F)

#Rather use overlayed density plots than histograms
df <- rbind(data.frame(Degree = acqdeg, Network = 'Acquaintance'),
            data.frame(Degree = acc_phdeg, Network = 'Phonebook'),
            data.frame(Degree = accdeg, Network = 'Communication'))
p1 <- ggplot(df, aes(x = Degree, group = Network, col = Network, fill = Network)) + 
  geom_density(position = 'dodge', alpha = 0.2, size = 1) +
  theme_bw() + 
  labs(y = 'Density of Node Degree', title = 'Total Degree') +
  easy_center_title()

df <- rbind(data.frame(Degree = acqindeg, Degree_Type = 'In-Degree'),
            data.frame(Degree = acqoutdeg, Degree_Type = 'Out-Degree'))
p2 <- ggplot(df, aes(x = Degree, group = Degree_Type, col = Degree_Type, fill = Degree_Type)) + 
  geom_density(position = 'dodge', alpha = 0.2, size = 1) +
  theme_bw() + 
  labs(y = 'Density of Node Degree', title = 'Acquaintance') +
  easy_center_title()

df <- rbind(data.frame(Degree = acc_phindeg, Degree_Type = 'In-Degree'),
            data.frame(Degree = acc_phoutdeg, Degree_Type = 'Out-Degree'))
p3 <- ggplot(df, aes(x = Degree, group = Degree_Type, col = Degree_Type, fill = Degree_Type)) + 
  geom_density(position = 'dodge', alpha = 0.2, size = 1) +
  theme_bw() + 
  labs(y = 'Density of Node Degree', title = 'Phonebook') +
  easy_center_title()

df <- rbind(data.frame(Degree = accindeg, Degree_Type = 'In-Degree'),
            data.frame(Degree = accoutdeg, Degree_Type = 'Out-Degree'))
p4 <- ggplot(df, aes(x = Degree, group = Degree_Type, col = Degree_Type, fill = Degree_Type)) + 
  geom_density(position = 'dodge', alpha = 0.2, size = 1) +
  theme_bw() + 
  labs(y = 'Density of Node Degree', title = 'Communication') +
  easy_center_title()


# pdf('totaldegree_dens_plot.pdf')
ggarrange(p1, p2, p3, p4, 
          nrow = 2, ncol = 2,
          labels = c('A', 'B', 'C', 'D'))
# dev.off()

# Calculate the average degree
mean(acqdeg)
mean(acc_phdeg)
mean(accdeg)

mean(accindeg)
mean(accoutdeg)


# Scale-free
net_scalefree(acq_dg)
net_scalefree(acc_ph_net)
net_scalefree(accW_nloop_dg)
net_scalefree(accass_ph_net)
net_scalefree(accassW_nloop_dg)
net_scalefree(poi_ph_net)
net_scalefree(poiW_nloop_dg)

# Create a Barplot for each graph's average degrees
# Shouldn't be normalized, because it removes all nuance from the plot
# barplot(table(factor(acqdeg, levels = min(acqdeg):max(acqdeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree') 
# barplot(table(factor(accdeg, levels = min(accdeg):max(accdeg))), space = 0, 
#         xlab = 'Degree') 
# barplot(table(factor(acc_phdeg, levels = min(acc_phdeg):max(acc_phdeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree')
# 
# barplot(table(factor(accindeg, levels = min(accindeg):max(accindeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree')
# barplot(table(factor(accoutdeg, levels = min(accoutdeg):max(accoutdeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree')

### Average path length
mean_distance(acq_net)
mean_distance(acc_ph_net_nloop)
mean_distance(accW_UD_nloop) #Calls must be undirected, because links are created regardless of who initiates contact

#Avg path of random Acq equivalent network
mean_distance(erdos.renyi.game(n = (vcount(acq_net)), p.or.m = (edge_density(acq_net, loops = F)), directed = T), directed = T) 

### Diameter 
diameter(acq_net)
diameter(acc_ph_net_nloop)
diameter(accW_UD_nloop) #Calls must be undirected, because links are created regardless of who initiates contact - Nie meer oortuig nie

# Plots: Distance from the outliers
# Distance of every other person from acc26 in accass network
dist.from.acc26 <- distances(accW_nloop_dg, v = V(accW_nloop_dg)[name == 'Acc26'], to = V(accW_nloop_dg), weights = NA)

pdf('distance26_acc.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc26)+1)
col <- col[dist.from.acc26+1]
plot(accW_nloop_dg, vertex.size = 15, vertex.label.cex=0.5,  vertex.color = col, vertex.label = ifelse(dist.from.acc26 == 0, "Acc26", dist.from.acc26), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

# Distance of every other person from acc22 in acc
dist.from.acc22 <- distances(accW_nloop_dg, v = V(accW_nloop_dg)[name == 'Acc22'], to = V(accW_nloop_dg), weights = NA)

pdf('distance22_acc.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc22)+1)
col <- col[dist.from.acc22+1]
plot(accW_nloop_dg, vertex.color = col, vertex.size = 15, vertex.label.cex=0.5,  vertex.label = ifelse(dist.from.acc22 == 0, "Acc22", dist.from.acc22), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

#distance from acc26 and 22 in acq network
dgacq <- decompose.graph(acq_net)[[1]] 
dist.from.acc26 <- distances(dgacq, v = V(dgacq)[name == 'Acc26'], to = V(dgacq), weights = NA)

pdf('distance26_acq.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc26)+1)
col <- col[dist.from.acc26+1]
plot(dgacq, vertex.color = col, vertex.size = 15, vertex.label.cex=0.5, vertex.label = ifelse(dist.from.acc26 == 0, "Acc26", dist.from.acc26), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

# Distance of every other person from acc22 in acq
dist.from.acc22 <- distances(dgacq, v = V(dgacq)[name == 'Acc22'], to = V(dgacq), weights = NA)
pdf('distance22_acq.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc22)+1)
col <- col[dist.from.acc22+1]
plot(dgacq, vertex.color = col, vertex.size = 15, vertex.label.cex=0.5, layout=layout.auto, vertex.label = ifelse(dist.from.acc22 == 0, "Acc22", dist.from.acc22), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

farthest_vertices(acqW_UD)
# bridges(acqW_UD)

### Transitivity - clustering coefficient 
transitivity(acq_net, type = "global") #network-level Transitivity
# transitivity(acq_net, type = "local") #node-level clustering coefficient

transitivity(acc_ph_net_nloop, type = "global") #network-level Transitivity
# transitivity(acc_ph_net_nloop, type = "local") #node-level clustering coefficient

transitivity(accW_nloop, type = "global") #network-level Transitivity
# transitivity(accW_nloop, type = "local") #node-level clustering coefficient

### Number of Communities 
ceb1 <- cluster_edge_betweenness(acqW_UD) 
ceb2 <- cluster_edge_betweenness(acc_ph_UD_nloop) 
ceb3 <- cluster_edge_betweenness(accW_UD) 

# details
length(ceb1) # number of communities
length(ceb2)
length(ceb3)
# membership(ceb1) # community membership for each node

# Section 5.1.2   Other Role-players Network Metrics ####

## Expanding Persons of Interest Network

# Total degree to identify and include Unknown persons of interest
complete_nodal_degree <- as.data.frame(sort(degree(completeW_UD_nloop, mode = "total"), decreasing = TRUE)) 
# include Unk1, 141, 206, 721 because degree of 4 (higher than noted1)

# Now also consider volume of contact. 
complete_edge_degree <- as.data.frame(sort(degree(complete_nloop, mode = "total"), decreasing = TRUE))
# Unk107 and 39 stand out

filter(all_lcollapse, sender == 'Unk107' | receiver == 'Unk107') %>% arrange(desc(weight))
filter(all_lcollapse, sender == 'Unk39' | receiver == 'Unk39') %>% arrange(desc(weight))
# Both primarily had contact with Acc26. 

# What about the phonebook degrees? 
ph_nodal_degree <- as.data.frame(sort(degree(phonebookW_UD_nloop, mode = "total"), decreasing = TRUE)) 
filter(phonecontactlinks, contact == 'Ph_Unk689')
filter(phonecontactlinks, contact == 'Unk2')
# Both of these have a degree of 7. Some strong connection to an acq clique.

filter(all_lcollapse, sender == 'Unk2' | receiver == 'Unk2') %>% arrange(desc(weight))
# Unk2 had multiple contact with Acc2, 22 & 23, so will include in POI. Note that Acc2 & 22 weren't in his phonebook.

### Number of nodes and links 
## Cellphone records Networks
# V(accass_net)           # all accused nodes and 5 associates
 V(poi_net)              # all nodes including persons of interest
# V(complete_net)         # all nodes including unknowns
# 
# E(accass_net)           # all accused nodes and 5 associates
 E(poi_net)              # all nodes including persons of interest
# E(complete_net)         # all nodes including unknowns

## Phonebook Contacts Network
# V(accass_ph_net)           # all nodes and associates links, minus Acc_10 (no number)
 V(poi_ph_net)              # all nodes including persons of interest
 V(phonebook_net)           # all nodes including unknowns
# 
# E(accass_ph_net)           # all accused and associates links, minus Acc_10 (no number)
 E(poi_ph_net)              # all links including persons of interest
 E(phonebook_net)           # all links including unknowns

### Disconnected components?
# clusters(accass_net)
# clusters(accass_ph_net)
 clusters(poi_net)
 clusters(poi_ph_net)
# clusters(complete_net)
 # clusters(phonebook_net)

### Plot according to Type

#Generate AccASS network based on Type 
type_accass <- vertex_attr(accassW_nloop_dg, "Type")

pdf('accassTYPE.pdf')
set.seed(8)
plot(accassW_nloop_dg,  
     vertex.size = 12,
     vertex.label = V(accassW_nloop_dg)$name, vertex.label.cex = 0.5, 
     vertex.label.color = ifelse(V(accassW_nloop_dg)$Type == 'Associate', 'white', 'black'),
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = 0.2, #can set arrow here, see above
     layout = layout.graphopt(accassW_nloop_dg),
     vertex.color = V(accassW_nloop_dg)$colour)
legend("bottomleft", legend = levels(factor(type_accass)), 
       fill = unique(V(accassW_nloop_dg)$colour),
       title = 'Type', title.adj = 0.16, 
       cex = 0.7, bty = 'n')
dev.off()

#Generate persons of interest network based on Type 
type_poi <- vertex_attr(poiW_nloop_dg, "Type")

pdf('poiTYPE.pdf')
set.seed(10)
plot(poiW_nloop_dg,  
     vertex.size = 12,
     vertex.label = V(poiW_nloop_dg)$name, vertex.label.cex = 0.5, 
     vertex.label.color = ifelse(V(poiW_nloop_dg)$Type == 'Associate', 'white', 'black'),
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
     layout= layout.graphopt(poiW_nloop_dg),
     vertex.color = V(poiW_nloop_dg)$colour)
legend("bottomleft", legend = levels(factor(type_poi)), 
       fill = unique(V(poiW_nloop_dg)$colour), 
       title = 'Type', title.adj = 0.091, 
       cex = 0.7, bty = 'n')
dev.off()
# pdf_crop('poiTYPE.pdf', mustWork = T)

# Generate Complete network based on Type 
# Delete nodes with fewer than 2 connections
completeW_nloop_simple <- delete.vertices(completeW_nloop, which(degree(completeW_nloop) <= 2))
type_complete <- vertex_attr(completeW_nloop_simple, "Type")

pdf('Complete_networkTYPE.pdf')
set.seed(5555)
plot(completeW_nloop_simple,  
     vertex.size=8,
     #vertex.label = V(completeW_nloop_simple)$name, vertex.label.cex = 0.5, vertex.label.color = 'white',
     vertex.label = NA,
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .2,
     layout=layout.auto(completeW_nloop_simple),
     vertex.color = V(completeW_nloop_simple)$colour)
legend("bottomleft", 
       legend = levels(factor(type_complete)), 
       fill = unique(V(completeW_nloop_simple)$colour), 
       title = 'Type',  title.adj = 0.09,
       cex = 0.7, bty = 'n')
dev.off()

#Generate Accass phonebook contacts network based on Type
type_accass_ph <- vertex_attr(accass_ph_net_nloop, "Type")

pdf('accassph_TYPE.pdf')
set.seed(18818)
plot(accass_ph_net_nloop,  
     vertex.size = 12,
     vertex.label = V(accass_ph_net_nloop)$name, vertex.label.cex = 0.5, vertex.label.color = 'white',
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
     layout= layout_nicely(accass_ph_net_nloop),
     vertex.color = V(accass_ph_net_nloop)$colour)
legend("topleft", legend = levels(factor(type_accass_ph)), 
       fill = unique(V(accass_ph_net_nloop)$colour), 
       title = 'Type', title.adj = 0.091, 
       cex = 0.7, bty = 'n')
dev.off()

neighbors(accass_ph_net_nloop, 'Acc3', mode = c("total"))

#Generate POI phonebook contacts network based on Type
type_poi_ph <- vertex_attr(poi_ph_net_nloop, "Type")

pdf('poiph_TYPE.pdf')
set.seed(5503445)
plot(poi_ph_net_nloop,  
     vertex.size = 12,
     vertex.label = V(poi_ph_net_nloop)$name, vertex.label.cex = 0.5, vertex.label.color = 'white',
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .3,
       layout= layout_components(poi_ph_net_nloop),
     vertex.color = V(poi_ph_net_nloop)$colour)
legend("topleft", legend = levels(factor(type_poi_ph)), 
       fill = unique(V(poi_ph_net_nloop)$colour), 
       title = 'Type', title.adj = 0.091, 
       cex = 0.7, bty = 'n')
dev.off()

#Generate phonebook contacts network based on Type - delete fewer than 2 degrees
phonebookW_nloop_simple <- igraph::delete.vertices(phonebookW_nloop, which(degree(phonebookW_nloop) <= 1))
type_pb <- vertex_attr(phonebookW_nloop_simple, "Type")
print_labs <- c('Acc6', 'Acc3', 'Acc17', 'Acc4')

pdf('pb_TYPE.pdf')
set.seed(4)
plot(phonebookW_nloop_simple,  
     vertex.size = 10,
     vertex.label = ifelse(V(phonebookW_nloop_simple)$name %in% print_labs, V(phonebookW_nloop_simple)$name, ''),
     vertex.label.cex = 0.5, vertex.label.color = ifelse(V(phonebookW_nloop_simple)$Type == 'Accused', 'black', 'white'),
     edge.width = 2, #set edge weight or frequency as edge width 
     edge.arrow.size = .2,
     layout=layout_nicely(phonebookW_nloop_simple),
     vertex.color = V(phonebookW_nloop_simple)$colour)
legend("bottomleft", legend = levels(factor(type_pb)), 
       fill = unique(V(phonebookW_nloop_simple)$colour), 
       title = 'Type', title.adj = 0.09, 
       cex = 0.7, bty = 'n')
dev.off()

# Acc4 is isolated, show all edges going from vertex Acc4 - maak miskien table van mutual contacts?
neighbors(phonebookW_nloop, 'Acc4', mode = c("total"))

### Density 
edge_density(acc_ph_net, loops = F) 
edge_density(accass_ph_net, loops = F) 
edge_density(poi_ph_net, loops = F)
edge_density(phonebook_nloop, loops = F) 

edge_density(accW_net, loops = F)
edge_density(accassW_net, loops = F)
edge_density(poiW_net, loops = F)
edge_density(completeW_net, loops = F)

# Check density if networks were undirected
edge_density(acc_ph_UD_nloop, loops = F)
edge_density(accass_ph_UD_nloop, loops = F) 
edge_density(poi_ph_UD_nloop, loops = F)
edge_density(phonebookW_UD_nloop, loops = F) 

edge_density(accW_UD_nloop, loops = F)
edge_density(accassW_UD_nloop, loops = F)
edge_density(poiW_UD_nloop, loops = F)
edge_density(completeW_UD_nloop, loops = F)

### Average Degree 
accass_phdeg <- igraph::degree(accass_ph_UD_nloop, mode = "total", loop = F)
poi_phdeg <- igraph::degree(poi_ph_UD_nloop, mode = "total", loop = F)
phonedeg <- igraph::degree(phonebookW_UD_nloop, mode = "total", loop = F)

head(sort(strength(completeW_nloop, mode = 'total'), decreasing =  T), 10)

filtered_ph <- delete_vertices(phonebookW_UD, which(degree(phonebookW_UD) <= 2))
filtered_phonedeg <- igraph::degree(filtered_ph, mode = "total")

accassdeg <- igraph::degree(accassW_UD_nloop, mode = "total", loop = F)
poideg <- igraph::degree(poiW_UD_nloop, mode = "total", loop = F)
completedeg <- igraph::degree(completeW_UD_nloop, mode = "total", loop = F)

# Scale-free?
network_scalefree(poi_net)
network_scalefree(poi_ph_net)

# Calculate the average degree
mean(accass_phdeg)
mean(poi_phdeg)
mean(phonedeg)
# mean(filtered_phonedeg)

mean(accassdeg)
mean(poideg)
mean(completedeg)

# Create a histogram for each network's degree distribution

pdf('BarDegree_complete.pdf')
barplot(table(factor(completedeg, levels = min(completedeg):max(completedeg))), space = 0, las = 2,
        ylab = 'Frequency', xlab = 'Degree') 
dev.off()

pdf('BarDegree_pb.pdf')
barplot(table(factor(phonedeg, levels = min(phonedeg):max(phonedeg))), space = 0, 
        ylab = 'Frequency', xlab = 'Degree')
dev.off()

# Complete and phonebook have severe outliers and are very skewed to the right
# Therefore, cut off the tail and also just show the lower degree distribution
completedeg_lower <- completedeg[completedeg <= 5]
pdf('BarDegree_complete5.pdf')
barplot(table(factor(completedeg_lower, levels = min(completedeg_lower):max(completedeg_lower))), space = 0, 
        ylab = 'Frequency', xlab = 'Degree') 
dev.off()

phonedeg_lower <- phonedeg[phonedeg <= 5]
pdf('BarDegree_pb5.pdf')
barplot(table(factor(phonedeg_lower, levels = min(phonedeg_lower):max(phonedeg_lower))), space = 0, 
        ylab = 'Frequency', xlab = 'Degree')
dev.off()

# accass and poi

pdf('BarDegree_accass.pdf')
hist(accassdeg, seq(-0.5, 22.5, 1), xlab = 'Degree',  axes = F, main = '')
axis(side = 1, at = seq(0,22,2), labels = seq(0,22,2), tick = F, line = -1)
axis(side = 2, at = 0:5, labels = 0:5, tick = T, line = -1)
# barplot(table(factor(accassdeg, levels = min(accassdeg):max(accassdeg))), space = 0, 
        # ylab = 'Frequency', xlab = 'Degree', xlim = c(0,22), ylim = c(0,5))
dev.off()

pdf('BarDegree_pb_accass.pdf')
hist(accass_phdeg, seq(-0.5, 14.5, 1), xlab = 'Degree',  axes = F, main = '', ylim = c(0, 5))
axis(side = 1, at = 0:14, labels = 0:14, tick = F, line = -1)
axis(side = 2, at = 0:5, labels = 0:5, tick = T, line = -1)
# barplot(table(factor(accass_phdeg, levels = min(accass_phdeg):max(accass_phdeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree', ylim = c(0,5))
dev.off()

pdf('BarDegree_poi.pdf')
hist(poideg, seq(-0.5, 22.5, 1), xlab = 'Degree',  axes = F, main = '', ylim = c(0, 8))
axis(side = 1, at = seq(0,22,2), labels = seq(0,22,2), tick = F, line = -1)
axis(side = 2, at = 0:8, labels = 0:8, tick = T, line = -1)
# barplot(table(factor(poideg, levels = min(poideg):max(poideg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree', xlim = c(0,22), ylim = c(0,8)) 
dev.off()

pdf('BarDegree_pb_poi.pdf')
hist(poi_phdeg, seq(-0.5, 14.5, 1), xlab = 'Degree',  axes = F, main = '', ylim = c(0, 8))
axis(side = 1, at = 0:14, labels = 0:14, tick = F, line = -1)
axis(side = 2, at = 0:8, labels = 0:8, tick = T, line = -1)
# barplot(table(factor(poi_phdeg, levels = min(poi_phdeg):max(poi_phdeg))), space = 0, 
#         ylab = 'Frequency', xlab = 'Degree', ylim = c(0,8))
dev.off()


### Average path length 
mean_distance(accass_ph_UD_nloop, directed = T)
mean_distance(poi_ph_UD_nloop, directed = T)
mean_distance(phonebookW_UD_nloop, directed = T)

mean_distance(accassW_UD_nloop, directed = T)
mean_distance(poiW_UD_nloop, directed = T)
mean_distance(completeW_UD_nloop, directed = T)

# Plots: Distance from the outliers
# Distance of every other person from acc26 in accass network
dist.from.acc26 <- distances(accassW_nloop_dg, v = V(accassW_nloop_dg)[name == 'Acc26'], to = V(accassW_nloop_dg), weights = NA)

set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc26)+1)
col <- col[dist.from.acc26+1]
plot(accassW_nloop_dg, vertex.size = 15, vertex.label.cex=0.5,  vertex.color = col, vertex.label = ifelse(dist.from.acc26 == 0, "Acc26", dist.from.acc26), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)

# Distance of every other person from acc22 in accass
dgaccass <- decompose.graph(accassW_nloop)[[1]] # returns a list of three graphs
dist.from.acc22 <- distances(dgaccass, v = V(dgaccass)[name == 'Acc22'], to = V(dgaccass), weights = NA)

set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc22)+1)
col <- col[dist.from.acc22+1]
plot(dgaccass, vertex.color = col, vertex.size = 15, vertex.label.cex=0.5,  vertex.label = ifelse(dist.from.acc22 == 0, "Acc22", dist.from.acc22), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)

# Plots: Distance from the outliers
# Distance of every other person from acc26 in accass network
dist.from.acc26 <- distances(accW_nloop_dg, v = V(accW_nloop_dg)[name == 'Acc26'], to = V(accW_nloop_dg), weights = NA)

pdf('distance26_acc.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc26)+1)
col <- col[dist.from.acc26+1]
plot(accW_nloop_dg, vertex.size = 15, vertex.label.cex=0.5,  vertex.color = col, vertex.label = ifelse(dist.from.acc26 == 0, "Acc26", dist.from.acc26), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

# Distance of every other person from acc22 in acc
dist.from.acc22 <- distances(accW_nloop_dg, v = V(accW_nloop_dg)[name == 'Acc22'], to = V(accW_nloop_dg), weights = NA)

pdf('distance22_acc.pdf')
set.seed(1)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.acc22)+1)
col <- col[dist.from.acc22+1]
plot(accW_nloop_dg, vertex.color = col, vertex.size = 15, vertex.label.cex=0.5,  vertex.label = ifelse(dist.from.acc22 == 0, "Acc22", dist.from.acc22), 
     edge.arrow.size = .2, vertex.label.color = "white", edge.label = NA)
dev.off()

### Diameter 
diameter(accass_ph_net_nloop, directed = T)
diameter(poi_ph_net_nloop, directed = T)
diameter(phonebookW_nloop, directed = T)

diameter(accass_nloop, directed = T)
diameter(poi_nloop, directed = T)
diameter(completeW_nloop, directed = T)

### Transitivity - clustering coefficient
transitivity(accassW_nloop, type = "global") #network-level transitivity
 sort(transitivity(accassW_nloop, type = "local"), decreasing = T) #node-level clustering coefficient

transitivity(poiW_nloop, type = "global") #network-level transitivity
# transitivity(poiW_nloop, type = "local") #node-level clustering coefficient

transitivity(completeW_nloop, type = "global") #network-level transitivity
# transitivity(completeW_nloop, type = "local") #node-level clustering coefficient

transitivity(accass_ph_net_nloop, type = "global") #network-level transitivity
# transitivity(accass_ph_net_nloop, type = "local") #node-level clustering coefficient

transitivity(poi_ph_net_nloop, type = "global") #network-level transitivity
# transitivity(poi_ph_net_nloop, type = "local") #node-level clustering coefficient

transitivity(phonebookW_nloop, type = "global") #network-level transitivity
# transitivity(phonebookW_nloop, type = "local") #node-level clustering coefficient

# Calculate the local transitivity for vertex Ass4
transitivity(accassW_nloop, vids='Ass4', type = "local")

### Number of Communities 
# ceb1 <- cluster_edge_betweenness(accassW_UD) 
# ceb2 <- cluster_edge_betweenness(poiW_UD) 
# ceb3 <- cluster_edge_betweenness(completeW_UD) 
# 
# ceb4 <- cluster_edge_betweenness(accass_ph_UD_nloop) 
# ceb5 <- cluster_edge_betweenness(poi_ph_UD_nloop) 
# ceb6 <- cluster_edge_betweenness(phonebookW_UD) 

# details
# length(ceb1) # number of communities
# length(ceb2)
# length(ceb3)
# 
# length(ceb4)
# length(ceb5)
# length(ceb6)
# 
# membership(ceb1) # community membership for each node

# High modularity for a partitioning reflects dense connections within communities and sparse connections across communities.

# Comparison with Random Network

rand_acc <- erdos.renyi.game(n = gorder(accW_UD_nloop), p.or.m = edge_density(accW_UD_nloop), type = "gnp")
plot(accW_UD_nloop, layout = layout.auto(accW_UD_nloop), edge.arrow.size = .2)
plot(rand_acc, layout = layout.auto(rand_acc), edge.arrow.size = .2)

# Section 5.2   Community-level Analysis ####

##Infomap community detection - calls only

#Plots
pdf('acc_infomap.pdf')
set.seed(1)
acc_info <- infomap.community(accW_nloop_dg, v.weights = NULL)
V(accW_nloop_dg)$acc_info <- acc_info$membership
plot(accW_nloop_dg, layout = layout.auto(accW_nloop_dg), 
     vertex.label = '',
     edge.arrow.size = 0.1, 
     vertex.color = V(accW_nloop_dg)$acc_info, 
     vertex.size = 15)
dev.off()

pdf('accass_infomap.pdf')
set.seed(1)
accass_info <- infomap.community(accassW_nloop_dg, v.weights = NULL)
V(accassW_nloop_dg)$accass_info <- accass_info$membership
plot(accassW_nloop_dg, layout = layout.auto(accassW_nloop_dg), 
     vertex.label = ifelse(V(accassW_nloop_dg)$Type == 'Associate', 'A', ''), 
     vertex.label.color = 'black',
     vertex.label.cex = 1.5,
     edge.arrow.size = 0.1, 
     vertex.color = V(accassW_nloop_dg)$accass_info, 
     vertex.size = 15)
dev.off()

pdf('poi_infomap.pdf')
set.seed(1)
poi_info <- infomap.community(poiW_nloop_dg, v.weights = NULL)
V(poiW_nloop_dg)$poi_info <- poi_info$membership
plot(poiW_nloop_dg, layout = layout.auto(poiW_nloop_dg), 
     vertex.label = ifelse(V(poiW_nloop_dg)$Type == 'Associate', 'A', 
                           ifelse(V(poiW_nloop_dg)$Type != 'Associate' & V(poiW_nloop_dg)$Type != 'Accused', 
                                  'P', '')), 
     vertex.label.color = 'black',
     vertex.label.cex = 1.5,
     edge.arrow.size = 0.1, 
     vertex.color = V(poiW_nloop_dg)$poi_info, 
     vertex.size = 15)
dev.off()

# Modularity measures
modularity(acc_info)
modularity(accass_info)
modularity(poi_info)

## Louvain community structure - acq and phonebooks

#Plots
pdf('acq_louvain.pdf')
set.seed(3)
acq_louv <- cluster_louvain(acqW_UD)
V(acqW_UD)$cluster <- acq_louv$membership
plot(acqW_UD, vertex.label = '', 
     vertex.color = V(acqW_UD)$cluster, 
     vertex.size = 15)
dev.off()

pdf('accPH_louvain.pdf')
set.seed(1)
acc_louv <- cluster_louvain(acc_ph_UD_nloop)
V(acc_ph_UD_nloop)$cluster <- acc_louv$membership
plot(acc_ph_UD_nloop, vertex.label = '', 
     vertex.color = V(acc_ph_UD_nloop)$cluster, 
     vertex.size =  15)
dev.off()

pdf('accassPH_louvain.pdf')
set.seed(1)
accass_louv <- cluster_louvain(accass_ph_UD_nloop)
V(accass_ph_UD_nloop)$cluster <- accass_louv$membership
plot(accass_ph_UD_nloop, vertex.label = '', 
     vertex.color = V(accass_ph_UD_nloop)$cluster, 
     vertex.size =  15)
dev.off()

pdf('poiPH_louvain.pdf')
set.seed(1)
poi_louv <- cluster_louvain(poi_ph_UD_nloop)
V(poi_ph_UD_nloop)$cluster <- poi_louv$membership
plot(poi_ph_UD_nloop, 
     vertex.label = '', 
     vertex.color = V(poi_ph_UD_nloop)$cluster, 
     vertex.size =  15)
dev.off()

#Measures
modularity(acq_louv)
modularity(acc_louv)
modularity(accass_louv)
modularity(poi_louv)

## 5.2.2 Assortativity
# - Homophily: the tendency of nodes to connect to others who are similar on some variable.
# - "assortativity_nominal()" is for categorical variables (e.g., test whether gender or department determine grouping). A positive score means nodes of the same something tend to connect to one another, so we can observe  homophily or institutionally constrained homophily such as groups determined by departments. A negative score means nodes of different genders/departments tend to connect to one another. 
# - "assortativity_degree()" checks assortativity in node degrees. 
# The degree assortativity scores go from -1 to 1. A positive score means the network is assortative, meaning nodes of similar degrees tend to 
# connect among each other (high degree nodes connect to other high degree nodes and low de gree nodes connect to other low degree nodes). 
# The negative score means the network is disassortative, meaning high degree nodes tend to connect to low degree nodes.

# Assortativity based on degree
assortativity_degree(acq_net, directed = T)
assortativity_degree(acc_ph_net_nloop, directed = T)
assortativity_degree(accass_ph_net_nloop, directed = T)
assortativity_degree(poi_ph_net_nloop, directed = T)
assortativity_degree(accW_nloop, directed = T)
assortativity_degree(accassW_nloop, directed = T)
assortativity_degree(poiW_nloop, directed = T)

# Acq 
value <- as.numeric(factor(V(acq_net)$Suburb)) # convert attribute to vector of numbers
value[is.na(value)] <- 0  #change NAs to 0
assortativity(acq_net, value, directed = T) #Suburb

value <- as.numeric(factor(V(acq_net)$Area)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #Area

value <- as.numeric(factor(V(acq_net)$Province)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #Province

value <- as.numeric(factor(V(acq_net)$HomeTown)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #HomeTown

value <- as.numeric(factor(V(acq_net)$HomeProvince)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #HomeProvince

value <- as.numeric(factor(V(acq_net)$Occupation)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #Occupation

value <- as.numeric(factor(V(acq_net)$Language)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #Language

value <- as.numeric(factor(V(acq_net)$MaritalStatus)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #MaritalStatus

value <- as.numeric(factor(V(acq_net)$Age)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #Age

value <- as.numeric(factor(V(acq_net)$ArrestVehicle)) 
value[is.na(value)] <- 0  
assortativity(acq_net, value, directed = T) #ArrestVehicle

# Accused Phonebook
value <- as.numeric(factor(V(acc_ph_net_nloop)$Suburb)) # convert attribute to vector of numbers
value[is.na(value)] <- 0  #change NAs to 0
assortativity(acc_ph_net_nloop, value, directed = T) #Suburb

value <- as.numeric(factor(V(acc_ph_net_nloop)$Area)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #Area

value <- as.numeric(factor(V(acc_ph_net_nloop)$Province)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #Province

value <- as.numeric(factor(V(acc_ph_net_nloop)$HomeTown)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #HomeTown

value <- as.numeric(factor(V(acc_ph_net_nloop)$HomeProvince)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #HomeProvince

value <- as.numeric(factor(V(acc_ph_net_nloop)$Occupation)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #Occupation

value <- as.numeric(factor(V(acc_ph_net_nloop)$Language)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #Language

value <- as.numeric(factor(V(acc_ph_net_nloop)$MaritalStatus)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #MaritalStatus

value <- as.numeric(factor(V(acc_ph_net_nloop)$Age)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #Age

value <- as.numeric(factor(V(acc_ph_net_nloop)$ArrestVehicle)) 
value[is.na(value)] <- 0  
assortativity(acc_ph_net_nloop, value, directed = T) #ArrestVehicle


# Accused Communication
value <- as.numeric(factor(V(accW_nloop)$Suburb)) # convert attribute to vector of numbers
value[is.na(value)] <- 0  #change NAs to 0
assortativity(accW_nloop, value, directed = T) #Suburb

value <- as.numeric(factor(V(accW_nloop)$Area)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #Area

value <- as.numeric(factor(V(accW_nloop)$Province)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #Province

value <- as.numeric(factor(V(accW_nloop)$HomeTown)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #HomeTown

value <- as.numeric(factor(V(accW_nloop)$HomeProvince)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #HomeProvince

value <- as.numeric(factor(V(accW_nloop)$Occupation)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #Occupation

value <- as.numeric(factor(V(accW_nloop)$Language)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #Language

value <- as.numeric(factor(V(accW_nloop)$MaritalStatus)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #MaritalStatus

value <- as.numeric(factor(V(accW_nloop)$Age)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #Age

value <- as.numeric(factor(V(accW_nloop)$ArrestVehicle)) 
value[is.na(value)] <- 0  
assortativity(accW_nloop, value, directed = T) #ArrestVehicle

# Homophily plots

# Acq - Province
prov <- V(acq_net)$Province
prov_col <- c('#ffa500', '#1e90ff')
V(acq_net)$color <- prov_col[as.numeric(as.factor(vertex_attr(acq_net, 'Province')))]

pdf('Province_acq.pdf')
set.seed(1)
plot(acq_net,  
     vertex.size = 15, 
     vertex.label = V(acq_net)$name, vertex.label.cex = 0.7, vertex.label.color = 'black',
     edge.width = 2, 
     edge.arrow.size = .4,
     vertex.color = V(acq_net)$color)
legend("topright", legend = levels(factor(prov)), fill = prov_col, title = 'Province', title.adj = 0.07, 
       cex = 0.7, bty = 'n')
dev.off()

# Acq - Occupation
occ <- V(acq_net)$Occupation
occ_col <- brewer.pal(length(unique(occ)), 'Set1')
V(acq_net)$color <- occ_col[as.numeric(as.factor(vertex_attr(acq_net, 'Occupation')))]

pdf('Occupation_acq.pdf')
set.seed(1)
plot(acq_net,  
     vertex.size = 15, 
     vertex.label = V(acq_net)$name, vertex.label.cex = 0.7, vertex.label.color = 'black',
     edge.width = 2, 
     edge.arrow.size = .4,
     vertex.color = V(acq_net)$color)
legend("topright", legend = levels(factor(occ)), fill = occ_col, title = 'Occupation', title.adj = 0.07, 
       cex = 0.7, bty = 'n')
dev.off()

# Acc - Province
prov <- V(accW_nloop_dg)$Province
prov_col <- c('#ffa500', '#1e90ff')
V(accW_nloop_dg)$color <- prov_col[as.numeric(as.factor(vertex_attr(accW_nloop_dg, 'Province')))]

pdf('Province_acc.pdf')
set.seed(13)
plot(accW_nloop_dg,  
     vertex.size = 15, 
     vertex.label = V(accW_nloop_dg)$name, vertex.label.cex = 0.6, vertex.label.color = 'black',
     edge.width = 1, 
     edge.arrow.size = .3,
     layout = layout.graphopt(accW_nloop_dg),
     vertex.color = V(accW_nloop_dg)$color)
legend("topright", legend = levels(factor(prov)), fill = prov_col, title = 'Province', title.adj = 0.07, 
       cex = 0.7, bty = 'n')
dev.off()

# Acc - HomeTown
av <- V(accW_nloop_dg)$ArrestVehicle
av_col <- brewer.pal(length(unique(av)), 'Dark2')
V(accW_nloop_dg)$color <- av_col[as.numeric(as.factor(vertex_attr(accW_nloop_dg, 'ArrestVehicle')))]
V(accW_nloop_dg)$color[is.na(V(accW_nloop_dg)$color)] <- 'white'

pdf('ArrestVehicle_acc.pdf')
set.seed(13)
plot(accW_nloop_dg,  
     vertex.size = 15, 
     vertex.label = V(accW_nloop_dg)$name, vertex.label.cex = 0.6, vertex.label.color = 'black',
     edge.width = 1, 
     edge.arrow.size = .3,
     layout = layout.graphopt(accW_nloop_dg),
     vertex.color = V(accW_nloop_dg)$color)
legend("topright", legend = levels(factor(av)), fill = av_col, title = 'Arrest Vehicle', title.adj = 0.07, 
       cex = 0.7, bty = 'n')
dev.off()


### Cliques
# complete subgraph of an undirected graph - largest clique

# cliques(acq_net) # list of cliques
#This shows that information from acc5 (bridge were easily spread within the clique)
vcol <- rep("grey80", vcount(acq_net))
vcol[unlist(largest_cliques(acq_net))] <- "gold"
set.seed(1)
plot(acq_net, edge.arrow.size = 0.3, vertex.size = 15, vertex.label.color = 'black', 
     vertex.label.cex = 0.5, vertex.color = vcol, edge.label = NA)

# cliques(acc_ph_net_nloop_dg) # list of cliques
#This shows that information from acc5 (bridge were easily spread within the clique)
vcol <- rep("grey80", vcount(acc_ph_net_nloop_dg))
vcol[unlist(largest_cliques(acc_ph_net_nloop_dg))] <- "gold"
set.seed(1)
plot(acc_ph_net_nloop_dg, 
     edge.arrow.size = 0.3, 
     vertex.size = 15, 
     vertex.label.color = 'black', 
     vertex.label.cex = 0.5, 
     vertex.color = vcol,
     layout = layout_with_dh(acc_ph_net_nloop_dg),
     edge.label = NA)

#This shows that information from acc5 (bridge were easily spread within the clique)
vcol <- rep("grey80", vcount(accW_UD_nloop_dg))
vcol[unlist(largest_cliques(accW_UD_nloop_dg))] <- "gold"
set.seed(1)
plot(accW_UD_nloop_dg, edge.arrow.size = 0.3, vertex.size = 15, vertex.label.color = 'black', 
     vertex.label.cex = 0.5, vertex.color = vcol, edge.label = NA)

# Clique: vir PoiPH and poi is dit nie veel werd nie. Poiph (2, 11, 9, 22, 21, 18, 23 of 25) en poi (20, 1, 5, 26, 19, 14)


# Section 5.3   Node-level Analysis ####

### Degree centrality

# centralization metrics
# For accused
centr_degree(acq_net, mode = 'total', loops = F, normalized = T)$centralization
centr_degree(acc_ph_net, mode = 'total', loops = F, normalized = T)$centralization
centr_degree(accW_net, mode = 'total', loops = F, normalized = T)$centralization

# For PoI
centr_degree(poi_ph_net, mode = 'total', loops = F, normalized = T)$centralization
centr_degree(poiW_net, mode = 'total', loops = F, normalized = T)$centralization

# These two stood out
filter(all_lcollapse, sender == 'Acc26' | receiver == 'Acc26') %>% arrange(desc(weight))
filter(all_lcollapse, sender == 'Unk206' | receiver == 'Unk206') %>% arrange(desc(weight))

## Total-Degree centrality
acq_total_degree <- data.frame(acq_t_d = sort(degree(acq_net, mode = "total", normalized = T), decreasing = TRUE))
phpoi_total_degree <- data.frame(phpoi_t_d = sort(degree(poi_ph_net_nloop, mode = "total", normalized = T), decreasing = TRUE))
poi_total_degree <- data.frame(poi_t_d = sort(degree(poiW_nloop, mode = "total", normalized = T), decreasing = TRUE))

# Visualise total degree poi communication
poi_dg_total_degree <- data.frame(poi_t_d = sort(degree(poiW_nloop_dg, mode = "total", normalized = T), decreasing = TRUE))
prov_poi <- vertex_attr(poiW_nloop_dg, "Province")
prov_poi_col <- c('#ffa500', '#1e90ff', 'white')
plotcols <- prov_poi_col[as.numeric(as.factor(vertex_attr(poiW_nloop_dg, 'Province')))]
plotcols[is.na(plotcols)] <- 'white'
deg <- degree(poiW_nloop_dg, mode ="total")

top7 <- rownames(poi_dg_total_degree)[1:7] #plot, only showing labels for top 7 degrees

pdf('tot_centr_poi_com.pdf')
set.seed(5)
plot(poiW_nloop_dg, 
     edge.arrow.size = .2, 
     vertex.color = plotcols,
     vertex.size = deg*0.43,
     vertex.label.cex = 0.12 + deg/max(deg)^1.1, 
     vertex.label.color = 'black',
     layout = layout_with_dh(poiW_nloop_dg),
     vertex.label = ifelse(V(poiW_nloop_dg)$name %in% top7, V(poiW_nloop_dg)$name, ''))
legend("bottomleft", 
       legend = levels(factor(prov_poi)), 
       fill = prov_col, 
       title = 'Type', title.adj = 0.09, 
       cex = 0.7, bty = 'n')
dev.off()

## Out-Degree centrality
acq_out_degree <- as.data.frame(sort(degree(acq_net, mode = "out", normalized = T), decreasing = TRUE))
phpoi_out_degree <- as.data.frame(sort(degree(poi_ph_net_nloop, mode = "out", normalized = T), decreasing = TRUE))
poi_out_degree <- as.data.frame(sort(degree(poiW_nloop, mode = "out", normalized = T), decreasing = TRUE))

## In-Degree centrality
acq_in_degree <- as.data.frame(sort(degree(acq_net, mode = "in", normalized = T), decreasing = TRUE))
phpoi_in_degree <- as.data.frame(sort(degree(poi_ph_net_nloop, mode = "in", normalized = T), decreasing = TRUE))
poi_in_degree <- as.data.frame(sort(degree(poiW_nloop, mode = "in", normalized = T), decreasing = TRUE))

# Closeness Centrality (total)

#Centralisation scores
centr_clo(acq_dg, mode = 'total', normalized = T)$centralization
centr_clo(poi_ph_net_nloop_dg, mode = 'total', normalized = T)$centralization
centr_clo(poiW_nloop_dg, mode = 'total', normalized = T)$centralization

# Ordered nodes
acq_total_closeness <- data.frame(acq_t_c = sort(closeness(acq_dg, mode = "total", normalized = T), decreasing = TRUE))
phpoi_total_closeness <- data.frame(phpoi_t_c = sort(closeness(poi_ph_net_nloop_dg, mode = "total", normalized = T), decreasing = TRUE))
poi_total_closeness <- data.frame(poi_t_c = sort(closeness(poiW_nloop_dg, mode = "total", normalized = T), decreasing = TRUE))

## Closeness centrality (OUT)
acq_out_closeness <- data.frame(acq_t_c = sort(closeness(acq_dg, mode = "out", normalized = T), decreasing = TRUE))
phpoi_out_closeness <- data.frame(phpoi_t_c = sort(closeness(poi_ph_net_nloop_dg, mode = "out", normalized = T), decreasing = TRUE))
poi_out_closeness <- data.frame(poi_t_c = sort(closeness(poiW_nloop_dg, mode = "out", weights = NULL, normalized = T), decreasing = TRUE))

## Closeness centrality (in) 
acq_in_closeness <- data.frame(acq_t_c = sort(closeness(acq_dg, mode = "in", normalized = T), decreasing = TRUE))
phpoi_in_closeness <- data.frame(phpoi_t_c = sort(closeness(poi_ph_net_nloop_dg, mode = "in", normalized = T), decreasing = TRUE))
poi_in_closeness <- data.frame(poi_t_c = sort(closeness(poiW_nloop_dg, mode = "in", normalized = T), decreasing = TRUE))

##Reciprocity
#Another measure of relationships's closeness in *directed* networks is reciprocity. The reciprocity of a directed network is equal to the proportion of edges that are symmetrical. In other words, the proportion of outgoing edges that also have an incoming edge.
reciprocity(acq_net, ignore.loops = T)
reciprocity(poi_ph_net_nloop, ignore.loops = T) # Not relevant because we have one-sided source
reciprocity(poiW_nloop_dg, ignore.loops = T)

# Create random network to see if reciprocity in ACQ is random - it is not
# Number of reiterations
num_iterations <- 10000

# Store reciprocity values for each iteration
reciprocity_values <- numeric(num_iterations)
random_graph <- erdos.renyi.game(n = (vcount(acq_net)), p.or.m = (edge_density(acq_net, loops = F)), directed = T)

# Generate random networks and calculate reciprocity
for (i in 1:num_iterations) {
  random_graph <- erdos.renyi.game(n = (vcount(acq_net)), p.or.m = (edge_density(acq_net, loops = F)), directed = T)
  reciprocity_values[i] <- reciprocity(random_graph)
}
mean(reciprocity_values)


## Betweenness Centrality
#It measures how frequently a node lies on the shortest path between any two nodes in a network. 
#It is equivalent to how critical each node is to the flow of information through a network.
#Individuals with high betweenness are key bridges between different parts of the networks. Individuals with low betweenness are not that significant to the overall connectedness of the network. You can calculate the betweeness of each vertex. The first argument is the graph object and the second is whether to consider the graph is directed or not. This method gives a raw betweenness score.

#Centralisation scores
centr_betw(acq_net, directed = T, normalized = T)$centralization
centr_betw(poi_ph_net_nloop, directed = T, normalized = T)$centralization
centr_betw(poiW_nloop, directed = T, normalized = T)$centralization

#Top 10
acq_node_between <- data.frame(acq_bet = sort(betweenness(acq_net, directed = T, normalized = T), decreasing = TRUE))
poiph_node_between <- data.frame(poiph_bet = sort(betweenness(poi_ph_net_nloop, directed = T, normalized = T), decreasing = TRUE))
poi_node_between <- data.frame(poi_betw = sort(betweenness(poiW_nloop, directed = T, normalized = T), decreasing = TRUE))

# acq_betw_rem <- delete.vertices(acq_net, rownames(acq_node_between)[1]) #Remove node with highest betweenness (Acc5)
# Acc1 has second highest btw score and was out on bail, will discuss this in the interviews chapter and show network with him removed

pdf('betweenness_acq.pdf')
prov_acq <- vertex_attr(acq_dg, "Province")

plot(acq_dg, 
     vertex.size = 20, 
     vertex.label.cex = 0.5, 
     vertex.color = prov_col,
     edge.arrow.size = 0.3, 
     layout = layout_as_tree(acq_dg, mode = 'out'))
legend("topleft", 
       legend = levels(factor(prov_acq)), 
       fill = prov_col, 
       title = 'Type', title.adj = 0.09, 
       cex = 0.7, bty = 'n')
dev.off()

## PageRank centrality
acq_pageR <- data.frame(acq_t_PR = sort(page_rank(acq_net)$vector, decreasing = T))
poiph_pageR <- data.frame(poiph_t_PR = sort(page_rank(poi_ph_net_nloop)$vector, decreasing = T))
poi_pageR <- data.frame(poi_PR = sort(page_rank(poiW_nloop)$vector, decreasing = T))




# Section 5.4 Disruption simulation ####

# 1. POI disruption

new_poi_dg <- poiW_nloop_dg
new_poi_bt <- poiW_nloop_dg
new_poi_pr <- poiW_nloop_dg

poi_total_degree <- degree(new_poi_dg, mode = "all")
poi_node_betweenness <- betweenness(new_poi_bt)
poi_pageRank <- page_rank(new_poi_pr)$vector

n <- 15 #number of removals

plot_df_poi <- data.frame(Density_dg = edge_density(new_poi_dg), 
                          Density_bt = edge_density(new_poi_bt),
                          Density_pr = edge_density(new_poi_pr),
                          Comps_dg = clusters(new_poi_dg)$no, 
                          Comps_bt = clusters(new_poi_bt)$no, 
                          Comps_pr = clusters(new_poi_pr)$no,
                          Rm_dg = NA,
                          Rm_bt = NA,
                          Rm_pr = NA,
                          Removed = 0)

for(i in 1:n){
  top_node_degree <- names(poi_total_degree)[which.max(poi_total_degree)]
  top_node_betweenness <- names(poi_node_betweenness)[which.max(poi_node_betweenness)]
  top_node_pageRank <- names(poi_pageRank)[which.max(poi_pageRank)]
  
  new_poi_dg <- delete_vertices(new_poi_dg, top_node_degree)
  new_poi_bt <- delete_vertices(new_poi_bt, top_node_betweenness)
  new_poi_pr <- delete_vertices(new_poi_pr, top_node_pageRank)
  
  plot_df_poi <- rbind(plot_df_poi, c(edge_density(new_poi_dg), 
                                      edge_density(new_poi_bt),
                                      edge_density(new_poi_pr),
                                      clusters(new_poi_dg)$no, 
                                      clusters(new_poi_bt)$no, 
                                      clusters(new_poi_pr)$no,
                                      top_node_degree,
                                      top_node_betweenness,
                                      top_node_pageRank,
                                      i))
  
  poi_total_degree <- degree(new_poi_dg, mode = "all")
  poi_node_betweenness <- betweenness(new_poi_bt)
  poi_pageRank <- page_rank(new_poi_pr)$vector
}

plot_df_poi <- plot_df_poi |> mutate_at(c(1:6, 10), as.numeric)

# Random removal simulation

nsims <- 100
rnd_dens_poi <- matrix(nrow = nsims, ncol = n)
rnd_comps_poi <- matrix(nrow = nsims, ncol = n)

for(s in 1:nsims){
  new_poi_rn <- poiW_nloop_dg
  for(i in 1:n){
    random_node <- V(new_poi_rn)$name[ceiling(runif(1, 
                                                    min = 0, 
                                                    max = length(V(new_poi_rn)$name)))]
    new_poi_rn <- delete_vertices(new_poi_rn, random_node)
    rnd_dens_poi[s, i] <- edge_density(new_poi_rn)
    rnd_comps_poi[s, i] <- clusters(new_poi_rn)$no
  }
}

mean_rnd_dens_poi <- c(edge_density(poiW_nloop_dg), colMeans(rnd_dens_poi))
mean_rnd_comps_poi <- c(clusters(poiW_nloop_dg)$no, colMeans(rnd_comps_poi))

plot_df_poi <- plot_df_poi |> mutate(Density_rn = mean_rnd_dens_poi,
                                     Comps_rn = round(mean_rnd_comps_poi, 1))

pdf('PoI_disruption.pdf')
plot(plot_df_poi$Removed, plot_df_poi$Density_dg, ylim = c(0, 0.15), 'o', pch = 16, 
     xlab = 'Number of Removals', ylab = 'Density', col = 'chocolate4')
lines(plot_df_poi$Removed, plot_df_poi$Density_bt, 'o', pch = 16, col = 'deepskyblue')
lines(plot_df_poi$Removed, plot_df_poi$Density_pr, 'o', pch = 16, col = 'darkorange1')
lines(plot_df_poi$Removed, plot_df_poi$Density_rn, 'o', lty = 2, pch = 16, col = 'darkgreen')
text(plot_df_poi$Removed, plot_df_poi$Density_dg, plot_df_poi$Comps_dg, pos = 2, col = 'chocolate4')
text(plot_df_poi$Removed, plot_df_poi$Density_bt, plot_df_poi$Comps_bt, pos = 4, col = 'deepskyblue')
text(plot_df_poi$Removed, plot_df_poi$Density_pr, plot_df_poi$Comps_pr, pos = 3, col = 'darkorange1')
text(plot_df_poi$Removed, plot_df_poi$Density_rn, plot_df_poi$Comps_rn, pos = 1, col = 'darkgreen')
legend('bottomleft', legend = c('Degree', 'Betweenness', 'PageRank', 'Random'), 
       lty = c(1,1,1,2), col = c('chocolate4', 'deepskyblue', 'darkorange1', 'darkgreen'), lwd = 2)
# title('POI Disruption')

dev.off()

# 2. Acq disruption

new_acq_dg <- acq_dg
new_acq_bt <- acq_dg
new_acq_pr <- acq_dg

acq_total_degree <- degree(new_acq_dg, mode = "all")
acq_node_betweenness <- betweenness(new_acq_bt)
acq_pageRank <- page_rank(new_acq_pr)$vector

n <- 15 #number of removals

plot_df_acq <- data.frame(Density_dg = edge_density(new_acq_dg), 
                          Density_bt = edge_density(new_acq_bt),
                          Density_pr = edge_density(new_acq_pr),
                          Comps_dg = clusters(new_acq_dg)$no, 
                          Comps_bt = clusters(new_acq_bt)$no, 
                          Comps_pr = clusters(new_acq_pr)$no,
                          Rm_dg = NA,
                          Rm_bt = NA,
                          Rm_pr = NA,
                          Removed = 0)

for(i in 1:n){
  top_node_degree <- names(acq_total_degree)[which.max(acq_total_degree)]
  top_node_betweenness <- names(acq_node_betweenness)[which.max(acq_node_betweenness)]
  top_node_pageRank <- names(acq_pageRank)[which.max(acq_pageRank)]
  
  new_acq_dg <- delete_vertices(new_acq_dg, top_node_degree)
  new_acq_bt <- delete_vertices(new_acq_bt, top_node_betweenness)
  new_acq_pr <- delete_vertices(new_acq_pr, top_node_pageRank)
  
  plot_df_acq <- rbind(plot_df_acq, c(edge_density(new_acq_dg), 
                                      edge_density(new_acq_bt),
                                      edge_density(new_acq_pr),
                                      clusters(new_acq_dg)$no, 
                                      clusters(new_acq_bt)$no, 
                                      clusters(new_acq_pr)$no,
                                      top_node_degree,
                                      top_node_betweenness,
                                      top_node_pageRank,
                                      i))
  
  acq_total_degree <- degree(new_acq_dg, mode = "all")
  acq_node_betweenness <- betweenness(new_acq_bt)
  acq_pageRank <- page_rank(new_acq_pr)$vector
}

plot_df_acq <- plot_df_acq |> mutate_at(c(1:6, 10), as.numeric)

# Random removal simulation

nsims <- 100
rnd_dens <- matrix(nrow = nsims, ncol = n)
rnd_comps <- matrix(nrow = nsims, ncol = n)

for(s in 1:nsims){
  new_acq_rn <- acq_dg
  for(i in 1:n){
    random_node <- V(new_acq_rn)$name[ceiling(runif(1, 
                                                    min = 0, 
                                                    max = length(V(new_acq_rn)$name)))]
    new_acq_rn <- delete_vertices(new_acq_rn, random_node)
    rnd_dens[s, i] <- edge_density(new_acq_rn)
    rnd_comps[s, i] <- clusters(new_acq_rn)$no
  }
}

mean_rnd_dens <- c(edge_density(acq_dg), colMeans(rnd_dens))
mean_rnd_comps <- c(clusters(acq_dg)$no, colMeans(rnd_comps))

plot_df_acq <- plot_df_acq |> mutate(Density_rn = mean_rnd_dens,
                                     Comps_rn = round(mean_rnd_comps, 1))

pdf('Acq_disruption.pdf')

plot(plot_df_acq$Removed, plot_df_acq$Density_dg, xlim = c(0, 16), ylim = c(0, 0.18), 'o', pch = 16, 
     xlab = 'Number of Removals', ylab = 'Density', col = 'chocolate4')
lines(plot_df_acq$Removed, plot_df_acq$Density_bt, 'o', pch = 16, col = 'deepskyblue')
lines(plot_df_acq$Removed, plot_df_acq$Density_pr, 'o', pch = 16, col = 'darkorange1')
lines(plot_df_acq$Removed, plot_df_acq$Density_rn, 'o', lty = 2, pch = 16, col = 'darkgreen')
text(plot_df_acq$Removed, plot_df_acq$Density_dg, plot_df_acq$Comps_dg, pos = 2, col = 'chocolate4')
text(plot_df_acq$Removed, plot_df_acq$Density_bt, plot_df_acq$Comps_bt, pos = 4, col = 'deepskyblue')
text(plot_df_acq$Removed, plot_df_acq$Density_pr, plot_df_acq$Comps_pr, pos = 3, col = 'darkorange1')
text(plot_df_acq$Removed, plot_df_acq$Density_rn, plot_df_acq$Comps_rn, pos = 1, col = 'darkgreen')
legend('bottomleft', legend = c('Degree', 'Betweenness', 'PageRank', 'Random'), 
       lty = c(1,1,1,2), col = c('chocolate4', 'deepskyblue', 'darkorange1', 'darkgreen'), lwd = 2)
# title('Acq Disruption')

dev.off()



# 3. Accass disruption

new_accass_dg <- accassW_nloop_dg
new_accass_bt <- accassW_nloop_dg
new_accass_pr <- accassW_nloop_dg

accass_total_degree <- degree(new_accass_dg, mode = "all")
accass_node_betweenness <- betweenness(new_accass_bt)
accass_pageRank <- page_rank(new_accass_pr)$vector

n <- 15 #number of removals

plot_df_accass <- data.frame(Density_dg = edge_density(new_accass_dg), 
                             Density_bt = edge_density(new_accass_bt),
                             Density_pr = edge_density(new_accass_pr),
                             Comps_dg = clusters(new_accass_dg)$no, 
                             Comps_bt = clusters(new_accass_bt)$no, 
                             Comps_pr = clusters(new_accass_pr)$no,
                             Rm_dg = NA,
                             Rm_bt = NA,
                             Rm_pr = NA,
                             Removed = 0)

for(i in 1:n){
  top_node_degree <- names(accass_total_degree)[which.max(accass_total_degree)]
  top_node_betweenness <- names(accass_node_betweenness)[which.max(accass_node_betweenness)]
  top_node_pageRank <- names(accass_pageRank)[which.max(accass_pageRank)]
  
  new_accass_dg <- delete_vertices(new_accass_dg, top_node_degree)
  new_accass_bt <- delete_vertices(new_accass_bt, top_node_betweenness)
  new_accass_pr <- delete_vertices(new_accass_pr, top_node_pageRank)
  
  plot_df_accass <- rbind(plot_df_accass, c(edge_density(new_accass_dg), 
                                            edge_density(new_accass_bt),
                                            edge_density(new_accass_pr),
                                            clusters(new_accass_dg)$no, 
                                            clusters(new_accass_bt)$no, 
                                            clusters(new_accass_pr)$no,
                                            top_node_degree,
                                            top_node_betweenness,
                                            top_node_pageRank,
                                            i))
  
  accass_total_degree <- degree(new_accass_dg, mode = "all")
  accass_node_betweenness <- betweenness(new_accass_bt)
  accass_pageRank <- page_rank(new_accass_pr)$vector
}

plot_df_accass <- plot_df_accass |> mutate_at(c(1:6, 10), as.numeric)

# Random removal simulation

nsims <- 100
rnd_dens <- matrix(nrow = nsims, ncol = n)
rnd_comps <- matrix(nrow = nsims, ncol = n)

for(s in 1:nsims){
  new_accass_rn <- accassW_nloop_dg
  for(i in 1:n){
    random_node <- V(new_accass_rn)$name[ceiling(runif(1, 
                                                       min = 0, 
                                                       max = length(V(new_accass_rn)$name)))]
    new_accass_rn <- delete_vertices(new_accass_rn, random_node)
    rnd_dens[s, i] <- edge_density(new_accass_rn)
    rnd_comps[s, i] <- clusters(new_accass_rn)$no
  }
}

mean_rnd_dens <- c(edge_density(accassW_nloop_dg), colMeans(rnd_dens))
mean_rnd_comps <- c(clusters(accassW_nloop_dg)$no, colMeans(rnd_comps))

plot_df_accass <- plot_df_accass |> mutate(Density_rn = mean_rnd_dens,
                                           Comps_rn = round(mean_rnd_comps, 1))

pdf('accass_disruption.pdf')

plot(plot_df_accass$Removed, plot_df_accass$Density_dg, xlim = c(0, 16), ylim = c(0, 0.18), 'o', pch = 16, 
     xlab = 'Number of Removals', ylab = 'Density', col = 'chocolate4')
lines(plot_df_accass$Removed, plot_df_accass$Density_bt, 'o', pch = 16, col = 'deepskyblue')
lines(plot_df_accass$Removed, plot_df_accass$Density_pr, 'o', pch = 16, col = 'darkorange1')
lines(plot_df_accass$Removed, plot_df_accass$Density_rn, 'o', lty = 2, pch = 16, col = 'darkgreen')
text(plot_df_accass$Removed, plot_df_accass$Density_dg, plot_df_accass$Comps_dg, pos = 2, col = 'chocolate4')
text(plot_df_accass$Removed, plot_df_accass$Density_bt, plot_df_accass$Comps_bt, pos = 4, col = 'deepskyblue')
text(plot_df_accass$Removed, plot_df_accass$Density_pr, plot_df_accass$Comps_pr, pos = 3, col = 'darkorange1')
text(plot_df_accass$Removed, plot_df_accass$Density_rn, plot_df_accass$Comps_rn, pos = 1, col = 'darkgreen')
legend('bottomleft', legend = c('Degree', 'Betweenness', 'PageRank', 'Random'), 
       lty = c(1,1,1,2), col = c('chocolate4', 'deepskyblue', 'darkorange1', 'darkgreen'), lwd = 2)
# title('accass Disruption')

dev.off()

# Out on Bail ####
# maak 'n out on bail or wanted attribute en verwyder daai persone uit die netwerk en kyk hoe lyk dit dan.