library(dplyr)
library(plyr)
library(tidyverse)
library(network)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(gridExtra)
# load airline data
airlines1997 = read.table('1997.csv', header = T, sep = ',')
# select important vairbles
airlines1997 = airlines1997[,c(1,2,4,9,11,16,17,18,22)]
# load carrier data
carriers = read.table('carriers.csv', header=T, sep = ',')
# load airports data
airports = read.table('airports.csv', header=T, sep = ',')
# load plane-data
## for tailnums have no information (NA for all variables), we delete them since it has no use.
plane = read.table('plane-data.csv', header = T, sep = ',')


# Network Analysis on Airports
## creating edges and nodes
### edge lists
per_flight = airlines1997 %>%  
  dplyr::group_by(Origin, Dest) %>%
  dplyr::summarise(weight = n()) %>% 
  dplyr::filter(weight >= 6000) %>% # total 32 piars of aiports
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(weight))

per_flight[1:10,] #top 10 pairs of airports and total flights bwt them in 1997

# average departure delay
#### get rid of observations where departure delay is NA
naindex = which(is.na(airlines1997$DepDelay)) #there are toatl 97763 na in departure

per_delay = airlines1997[-naindex,] %>%
  dplyr::group_by(Origin) %>%
  dplyr::summarise(mean(DepDelay))

colnames(per_delay) = c('Origin','avg_delay')


### node list
sources = per_flight %>% distinct(Origin)
colnames(sources) = 'Label'
targets = per_flight %>% distinct(Dest)
colnames(targets) = 'Label'
nodes = full_join(sources, targets, by = 'Label' )
nodes$id = c(1:33)
nodes = nodes %>%
  left_join(airports, by = c('Label' = 'iata')) 
nodes = nodes %>% dplyr::select (Label, id, country)
nodes = nodes %>%
  dplyr::left_join(per_delay, by = c('Label' = 'Origin')) 

#edges
edges = per_flight %>%
  dplyr::left_join(nodes, by = c('Origin' = 'Label')) %>%
  dplyr::rename(from = id)

edges = edges %>% 
  dplyr::left_join(nodes, by = c('Dest' = 'Label')) %>%
  dplyr::rename(to =id)

edges = edges %>% dplyr::select(from, to, weight)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

country = as.factor(nodes$country)

ggraph(routes_tidy, layout = "kk") + 
  geom_node_point(aes(colour = country), size = nodes$avg_delay) +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = Label), vjust = 1.8, size = 3, position = 'identity', repel = T) +
  labs(title = "Airport Network in 1997",subtitle = "Graph by Chendan Tang") +
  theme_graph(plot_margin = margin(5, 5, 5, 5))

#######################################################################################
# let's see in 2002
airlines2002 = read.table('2002.csv', header = T, sep = ',')
airlines2002 = airlines2002[,c(1,2,4,9,11,16,17,18,22)]

per_flight2002 = airlines2002 %>%  
  dplyr::group_by(Origin, Dest) %>%
  dplyr::summarise(weight = n()) %>% 
  dplyr::filter(weight >= 6000) %>% #total 82 pairs
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(weight))

per_flight2002[1:10,]

naindex2 = which(is.na(airlines2002$DepDelay)) # total 65143 na delay

per_delay2002 = airlines2002[-naindex2,] %>%
  dplyr::group_by(Origin) %>%
  dplyr::summarise(mean(DepDelay)) %>%
  dplyr::rename(avg_delay2 = 'mean(DepDelay)')

sources2 = per_flight2002 %>% dplyr::distinct(Origin)
colnames(sources2) = 'Label'
targets2 = per_flight2002 %>% dplyr::distinct(Dest)
colnames(targets2) = 'Label'

nodes2 = full_join(sources2, targets2, by = 'Label' )
nodes2$id = c(1:29)
nodes2 = nodes2 %>%
  dplyr::left_join(airports, by = c('Label' = 'iata')) 
nodes2 = nodes2 %>% dplyr::select (Label, id, country)
nodes2 = nodes2 %>%
  dplyr::left_join(per_delay2002, by = c('Label' = 'Origin')) 

#edges
edges2 = per_flight2002 %>%
  dplyr::left_join(nodes2, by = c('Origin' = 'Label')) %>%
  dplyr::rename(from = id)

edges2 = edges2 %>% 
  dplyr::left_join(nodes2, by = c('Dest' = 'Label')) %>%
  dplyr::rename(to =id)

edges2 = edges2 %>% dplyr::select(from, to, weight)

routes_tidy2 <- tbl_graph(nodes = nodes2, edges = edges2, directed = TRUE)

country = as.factor(nodes2$country)

ggraph(routes_tidy2, layout = "kk") + 
  geom_node_point(aes(colour = country), size = nodes2$avg_delay2) +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = Label), vjust = 1.8, size = 3, position = 'identity', repel = T) +
  labs(title = "Airport Network in 2002", subtitle = "Graph by Chendan Tang") +
  theme_graph(plot_margin = margin(5, 5, 5, 5))

##################################################################################
# manufactor
## combine table
#####1997
airline_plane1997 = airlines1997 %>%
  left_join(plane, by = c('TailNum' = 'tailnum'))
## too many manufacture, here choose top five, also drop manufacturer is na
naman1997 = which(is.na(airline_plane1997$manufacturer))
airline_plane1997 = airline_plane1997[-naman1997,]
nplane_per_man = airline_plane1997 %>% dplyr::group_by(manufacturer) %>% dplyr::summarise(n = n())%>% arrange(desc(n))
head(nplane_per_man)

##########McDonnell Douglas was formed by the merger of McDonnell Aircraft and the Douglas Aircraft Company in 1967.
########## if the name contains both 'MCDONNELL' and 'DOUGLAS', we consider it as the company after merge. So, we combine 'MCDONNELL DOUGLAS','MCDONNELL DOUGLAS AIRCRAFT CO' and 'MCDONNELL DOUGLAS CORPORATION', they are the same company. 
# combine the company
index1 = which(airline_plane1997$manufacturer == 'MCDONNELL DOUGLAS AIRCRAFT CO')
index2 = which(airline_plane1997$manufacturer == 'MCDONNELL DOUGLAS CORPORATION')
index3 = which(airline_plane1997$manufacturer == 'DOUGLAS')
airline_plane1997[index1,]$manufacturer = 'MCDONNELL DOUGLAS'
airline_plane1997[index2,]$manufacturer = 'MCDONNELL DOUGLAS'
airline_plane1997[index3,]$manufacturer = 'MCDONNELL DOUGLAS'

nplane_per_man = airline_plane1997 %>% dplyr::group_by(manufacturer) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
head(nplane_per_man)
## lets' select the top 4 manufacturer 'BOEING' 'MCDONNELL DOUGLAS' 'AIRBUS INDUSTRIE' 'CESSNA'
data1997 = airline_plane1997 %>% filter(manufacturer %in% c('BOEING', 'MCDONNELL DOUGLAS', 'AIRBUS INDUSTRIE', 'CESSNA'))
p1 = ggplot(data, aes(x=DepDelay)) + 
  geom_density(aes(group=manufacturer, colour=manufacturer, fill=manufacturer), position = 'stack')+
  xlim(c(-30,60)) + 
  labs(title = 'Stacked Density of DepDelay in 1997')

####2002
airline_plane2002 = airlines2002 %>%
  left_join(plane, by = c('TailNum' = 'tailnum'))
naman2002 = which(is.na(airline_plane2002$manufacturer))
airline_plane2002 = airline_plane2002[-naman2002,]
nplane_per_man_2002 = airline_plane2002 %>% dplyr::group_by(manufacturer) %>% dplyr::summarise(n = n())%>% arrange(desc(n))
head(nplane_per_man_2002)
###!!!According to Wiki, in 2001, Airbus Industrie GIE was reorganised as Airbus SAS, a simplified joint-stock company. 
# so we consider them the same compnay. here we unify them user 'AIRBUS' name
# still mcdonnel douglas need to be chnaged in 2002
index21 = which(airline_plane2002$manufacturer == 'AIRBUS INDUSTRIE' )
index1 = which(airline_plane2002$manufacturer == 'MCDONNELL DOUGLAS AIRCRAFT CO')
index2 = which(airline_plane2002$manufacturer == 'MCDONNELL DOUGLAS CORPORATION')
index3 = which(airline_plane2002$manufacturer == 'DOUGLAS')
airline_plane2002$manufacturer[index1] = 'MCDONNELL DOUGLAS'
airline_plane2002$manufacturer[index2] = 'MCDONNELL DOUGLAS'
airline_plane2002$manufacturer[index3] = 'MCDONNELL DOUGLAS'
airline_plane2002$manufacturer[index21] = 'AIRBUS'
nplane_per_man_2002 = airline_plane2002 %>% dplyr::group_by(manufacturer) %>% dplyr::summarise(n = n())%>% arrange(desc(n))
head(nplane_per_man_2002) #new rank
## lets' select the top 4 manufacturer 'BOEING' 'MCDONNELL DOUGLAS' 'AIRBUS' 'CESSNA'
data2002 = airline_plane2002 %>% filter(manufacturer %in% c('BOEING', 'MCDONNELL DOUGLAS', 'AIRBUS', 'CESSNA'))
p2 = ggplot(data, aes(x=DepDelay)) + 
  geom_density(aes(group=manufacturer, colour=manufacturer, fill=manufacturer), position = 'stack')+
  xlim(c(-30,60)) + 
  labs(title = 'Stacked Density of DepDelay in 2002')

grid.arrange(p1,p2)

#######################################################################
# manufacture year
data1997$age = data1997$Year - as.numeric(as.character(data1997$year))
index = which(is.na(data1997$age))
data1997 = data1997[-index,]
idx = which(is.na(data1997$DepDelay))
data1997 = data1997[-idx,]
p3 = ggplot(data1997, aes(x=age, y=DepDelay, color=manufacturer, shape=manufacturer)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggplot(data1997, aes(x=age, y=DepDelay)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  labs(title = 'Plane Age and DepDelay in 1997')

data2002$age = data2002$Year - as.numeric(as.character(data2002$year))
index2002 = which(is.na(data2002$age))
data2002 = data2002[-index2002,]
idx2 = which(is.na(data2002$DepDelay))
data2002 = data2002[-idx2,]
p4 = ggplot(data2002, aes(x=age, y=DepDelay, color=manufacturer, shape=manufacturer)) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  labs(title = 'Plane Age and DepDelay in 2002')

grid.arrange(p3,p4)

###################################################################

#reference: Stat578 Lecture Slides (Statistical Network and Graphical Models) by Annie Qu 
