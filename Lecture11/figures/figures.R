#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################


# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



# Load Packages -----------------------------------------------------------
pkg<-list("sp")
lapply(pkg, require, character.only=T)
rm(pkg)



# Spatial Correlation Example ----------------------------------------------


grid <- GridTopology(c(0,0), c(1,1), c(8,8))
polys <- as(grid, "SpatialPolygons")
centroids <- coordinates(polys)
id <- names(polys)

ex <- SpatialPolygonsDataFrame(polys, data = data.frame(id = id, row.names = row.names(polys)))

ex@data$color<-"#ffffff"
ex@data$color[ex@data$id%in%c("g1","g64")]<-'#b30000'
ex@data$color[ex@data$id%in%c("g2","g9","g10","g55","g56","g55","g63")]<-'#e34a33'
ex@data$color[ex@data$id%in%c("g3","g11","g17","g18","g19",
                              "g46","g47","g48","g54","g62")]<-'#fc8d59'
ex@data$color[ex@data$id%in%c("g4","g12","g20","g28","g27",
                              "g26","g25","g37","g38","g39",
                              "g40","g45","g53","g61")]<-'#fdbb84'

pdf("spatial_correlation.pdf")
plot(ex, col=ex@data$color)
dev.off()

# Spatial Heterogeneity Examle ----------------------------------------------

grid <- GridTopology(c(0,0), c(1,1), c(8,8))
polys <- as(grid, "SpatialPolygons")
centroids <- coordinates(polys)
id <- names(polys)

ex <- SpatialPolygonsDataFrame(polys, data = data.frame(id = id, row.names = row.names(polys)))

ex@data$color<-"#ffffff"
ex@data$color[ex@data$id%in%c("g1","g3","g10","g12",
                              "g17","g19",
                              "g26","g28",
                              "g33","g35",
                              "g42","g44",
                              "g49","g51",
                              "g58","g60")]<-'#b30000'

ex@data$color[ex@data$id%in%c("g5","g6",
                              "g13","g14","g15",
                              "g21","g22","g23",
                              "g29","g31",
                              "g38",
                              "g39","g40",
                              "g46","g53",
                              "g53","g54","g55",
                              "g62","g64")]<-'#fc8d59'


ex@data$color[ex@data$id%in%c("g7","g8","g16",
                              "g30",
                              "g37","g39",
                              "g24","g32",
                              "g45","g47","g48",
                              "g56",
                              "g61","g63")]<-'#fdd49e'

pdf("spatial_heterogeneity.pdf")
plot(ex, col=ex@data$color)
dev.off()



# Rook Criterion ----------------------------------------------------------



grid <- GridTopology(c(0,0), c(1,1), c(7,7))
polys <- as(grid, "SpatialPolygons")
centroids <- coordinates(polys)
id <- names(polys)
tr <- ifelse(id == "g18", 1, 0)
tr[id == "g11" | id == "g17" | id == "g19" | id == "g25"]<-2

ex <- SpatialPolygonsDataFrame(polys, data = data.frame(id = id, tr = tr, row.names = row.names(polys)))

colors<-ifelse(seq_along(ex) %in% c(unlist(ex), which(ex$tr==1)), 'gray', NA)
colors[seq_along(ex) %in% c(unlist(ex), which(ex$tr==2))] <-'blue'

pdf("rook.pdf")
plot(ex, col=colors)
dev.off()



# Queen Criterion ---------------------------------------------------------

grid <- GridTopology(c(0,0), c(1,1), c(7,7))
polys <- as(grid, "SpatialPolygons")
centroids <- coordinates(polys)
id <- names(polys)
tr <- ifelse(id == "g18", 1, 0)
tr[id == "g10" | id == "g11" | id == "g12" | id == "g17" | id == "g19" | id == "g24" |  id == "g25" | id == "g26"]<-2

ex <- SpatialPolygonsDataFrame(polys, data = data.frame(id = id, tr = tr, row.names = row.names(polys)))

colors<-ifelse(seq_along(ex) %in% c(unlist(ex), which(ex$tr==1)), 'gray', NA)
colors[seq_along(ex) %in% c(unlist(ex), which(ex$tr==2))] <-'blue'

pdf("queen.pdf")
plot(ex, col=colors)
dev.off()


