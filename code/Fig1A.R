#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2021-06-02
# Network map
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # numerous data wrangling packages
library(data.table)   # quick data loadinglibrary(lubridate)
library(lubridate)    # Working with dates
library(RColorBrewer) # Fancy colours
library(ggrepel)      # Fancy labels in plot
library(scales)       # Log-transformed axis labels
library(ggpubr)       # Arranging several ggplots
library(stargazer)    # LaTeX regression tables
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)

#
library(reshape2)
library("network")
library("maps")
library("rnaturalearth")
library("geomnet")
library("ggnetwork")
library(rgeos)
library(rgdal)
library(raster)


#%#%#%#%#%#%
# Load data
#%#%#%#%#%#

# Choose yearly network data
df3 <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/network_data/olm_network_data_2019.csv")

df4 <- sample_frac(df3, size = 0.2)

nodes <- data.frame(df4 %>% group_by(worker_country_real, worker_admin1, worker_lat, worker_lng) %>%
  summarise(count = n()))

colnames(nodes) <- c("country", "region", "latitude", "longitude", "count")

nodes <- nodes %>% filter(region != "None")
nodes$ID <- apply( nodes[ , c(1, 2) ] , 1 , paste , collapse = ", " )

nodes <- nodes %>% group_by(country, region, ID) %>% summarise(latitude = mean(latitude), longitude = mean(longitude), count = sum(count))

# Nodes 2
nodes2 <- data.frame(df4 %>% group_by(employer_country_real, employer_admin1, employer_lat, employer_lng) %>%
  summarise(count = n()))

colnames(nodes2) <- c("country", "region", "latitude", "longitude", "count")

nodes2$ID <- apply( nodes2[ , c(1, 2) ] , 1 , paste , collapse = ", " )

nodes2 <- nodes2 %>% filter(region != "None")
nodes2 <- nodes2 %>% group_by(country, region, ID) %>% summarise(latitude = mean(latitude), longitude = mean(longitude), count = sum(count))

nodes3 <- rbind(nodes, nodes2)
nodes3 <- nodes3[,c(-6)]
nodes3 <- nodes3[!duplicated(nodes3),]

nodes3 <- nodes3 %>% filter(region != "None")
nodes3 <- nodes3 %>% group_by(country, region, ID) %>% summarise(latitude = mean(latitude), longitude = mean(longitude))

seller <- nodes %>% dplyr::select(ID, sold = count)
buyer <- nodes2 %>% dplyr::select(ID, bought = count)

nodes3 <- merge(nodes3, seller, by = c("country","region","ID"), all.x = T)
nodes3 <- merge(nodes3, buyer, by = c("country","region","ID"), all.x = T)
nodes3[is.na(nodes3)] <- 0
nodes3 <- nodes3 %>% mutate(balance = sold - bought)
nodes3$balanceFactor <- ifelse(nodes3$balance > 0, 1, 0)


# Edges
edges <- data.frame(df4 %>% group_by(employer_country_real, employer_admin1, employer_lat, employer_lng,
                                    worker_country_real, worker_admin1, worker_lat, worker_lng) %>%
  summarise(count = n()))

edges <- edges %>% filter(employer_admin1 != "None", worker_admin1 != "None")

edges$source <- apply( edges[ , c(1, 2) ] , 1 , paste , collapse = ", " )
edges$target <- apply( edges[ , c(5, 6) ] , 1 , paste , collapse = ", " )

sample <- 0.1

# Random sample
reducedEdgelist <- edges[sample(1:nrow(edges), size = sample*nrow(edges), replace = TRUE, prob = edges$count
                                ),c("source", "target", "count")]

coocNet<-network(reducedEdgelist,
                 matrix.type='edgelist',
                 directed=FALSE,  # this will be an undirected network
                 ignore.eval=FALSE#,  # confusingly, this tells it to include edge weights
                 #names.eval=c('count')  # names for the edge weights
)


#----------------------------
# HERE IMPROVEMENT NEEDED
coocNet%v%'longitude'<-sapply(network.vertex.names(coocNet),function(name){
  nodes3[nodes3$ID==name,]$longitude
})

# HERE IMPROVEMENT NEEDED
coocNet%v%'latitude'<-sapply(network.vertex.names(coocNet),function(name){
  nodes3[nodes3$ID==name,]$latitude
})

coocNet%v%'balanceFactor'<-sapply(network.vertex.names(coocNet),function(name){
  nodes3[nodes3$ID==name,]$balanceFactor
})

countries <- ne_countries(returnclass = "sf")

# This part maps the points to Robinson!

coords=cbind(coocNet%v%'longitude',coocNet%v%'latitude')

coords2 <- data.frame(coords) # Hier ist der Trick, dass wir die Punkte in Robinson umwandeln

coordinates(coords2) <- c("X1","X2")

proj4string(coords2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coords3 <- spTransform(coords2,CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

coocNet%v%'longitude' <- coords3@coords[,1]
coocNet%v%'latitude' <- coords3@coords[,2]

network_df <- ggnetwork(coocNet)

# Adjust end coordinates to Robinson
from_coords_mercator <- paste(network_df$x, network_df$y, sep = ",") 
from_coords_robinson <- paste(network_df$latitude, network_df$longitude, sep = ",")
mapper <- data.frame(cbind(from_coords_mercator,from_coords_robinson))
colnames(mapper)  <- c("mercator", "robinson")

to_coords_mercator <- paste(network_df$x, network_df$y, sep = ",")
to_coords_mercator <- data.frame(to_coords_mercator)
colnames(to_coords_mercator) <- c("mercator")
mapper2 <- merge(mapper, to_coords_mercator, by = "mercator")
mapper2 <- mapper2 %>% distinct(.keep_all = TRUE)

network_df$to_coords <- paste(network_df$xend, network_df$yend, sep = ",")
network_df2 <- merge(network_df, mapper2, by.x = "to_coords", by.y = "mercator")
network_df2$robinson <- as.character(network_df2$robinson)
network_df2$latitude_to <- sapply(network_df2$robinson, function(x) as.numeric(strsplit(x, ",")[[1]][1]))
network_df2$longitude_to <- sapply(network_df2$robinson, function(x) as.numeric(strsplit(x, ",")[[1]][2]))
network_df2$lat_to2 <- network_df2$latitude_to * .999 
network_df2$lon_to2 <- network_df2$longitude_to * .999

network_df2$balanceFactor <- ifelse(network_df2$balanceFactor == 0, "Majority buyer", "Majority seller")

# Plot
ggplot() +
  geom_sf(data = countries, fill = adjustcolor("grey90",alpha.f = 0.5), 
          col = "grey30", lwd = 0.1) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
           ylim=c(-5500000,7800000), xlim = c(-14000000,14800000))+
  geom_edges(data = network_df2,
             aes(x = longitude, y = latitude, 
                 xend = lon_to2, yend = lat_to2), 
             alpha = 0.25, curvature = 0.3, lwd = 0.25, colour = "#ac7921") +
  geom_nodes(data = network_df2, aes(x=longitude, y = latitude, fill = balanceFactor), 
             alpha = 0.3,size = 2,shape = 21, stroke = 0.0) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10], 
                                brewer.pal(11,"RdBu")[2]), 
                               na.translate=FALSE) +
  labs(fill = "Region: ") +
  theme_blank() + theme(legend.position = c(0.9,0.8), legend.background = element_blank(),
                        legend.direction = "vertical",
                        legend.text = element_text(size = 14), 
                        legend.title = element_text(size = 14),
                        panel.grid.major = element_line(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, lwd = 1.2), label.position = "right"),
         fill = guide_legend(override.aes = list(alpha = 1, size = 3), label.position = "right"))
 