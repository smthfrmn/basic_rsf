library(tidyterra)
library(terra)
library(cowplot)
library(ggplot2)
library(tidyverse)

# gnwt_par<- read.csv("C:/Users/HP/Downloads/TASR_NSR_Parturition_data-to-2021cvg_ed_20231130c.csv")
# head(gnwt_par)
data_df <- readRDS("GNWT_RSF_data.rds")
data_fltr <- data_df |> filter(case == 1)
gnwt_sf <-  data_fltr |> filter(!is.na(location.long )) |>
  st_as_sf( coords = c("location.long", "location.lat"), crs = 4326)

### Upload the rasters
# dist_road <- rast("D:/Work/MoveApps/Y2Y_road_analysis/data/Y2Y_road_dist_001.tif")
# fc <- rast("D:/Work/MoveApps/Y2Y_road_analysis/data/Y2Y_forest_cover.tif")
# ch <- rast("D:/Work/MoveApps/Y2Y_road_analysis/data/Y2Y_canopy_height.tif")
# 
# data_df$dist_road <- scale(extract(dist_road, cbind(data_df$location.long, data_df$location.lat)))
#  data_df$fc <- scale(extract(fc, cbind(data_df$location.long, data_df$location.lat)))
#  data_df$ch <- scale(extract(ch, cbind(data_df$location.long, data_df$location.lat)))
 modglm <-glm(case ~ elevation + delx + dely + distxy+fc+ch+dist_road,data = data_df)

 output <- as.data.frame(summary(modglm)$coefficients)
 
rownames(output)[c(2,6,7)]<-c("elevation", "forest_cover", "canopy_height")

    modplot <-ggplot(output) +
       geom_point(aes(x= rownames(output), y= Estimate), col ="blue")+
       geom_linerange(aes(x= rownames(output),
                           ymin= Estimate-1.96*`Std. Error` , 
                           ymax= Estimate+1.96*`Std. Error`))+
        labs(x= "Variable", y= "Estimate")+
       theme_bw()
 modplot
 ggsave(modplot, file = "Parturition_Coeffcient_plot.jpeg",
        +        width=9, height=6, units= "in", dpi=300)
 
 ##### RAster handling...not required other than plotting
 dist_road1 <- crop(dist_road, ch)
 ch_re <- project(ch, dist_road1)
 fc_re <- project(fc, dist_road1)
 rs<-c(fc_re, ch_re, dist_road1)
 
 rs1 <- crop(rs, c(-120.8,-116.2,61.79,64.47))
 elev <-get_elev_raster(rs1, src = "aws",  
                         prj = st_crs(4326),  units="meters", z=9)
 
 elev_re <- project(elev, rs1)
 
 rs2 <- scale(rs1$tree_canopy_cover)*(output$Estimate[6])+ 
   scale(rs1$b1)*(output$Estimate[7]) + 
   scale(rs1$layer)*(output$Estimate[8])+
   scale(elev_re)*(output$Estimate[2])
 
 chp <- ggplot()+geom_spatraster(data = rs1$`Canopy Height`)+
   #facet_wrap(~ lyr, scales = "free")+
   theme_bw()+scale_fill_hypso_c()+
   geom_sf(data = gnwt_sf)+ labs(fill="Canopy Height")+
   theme(legend.position = "bottom")
 
 rdp <- ggplot()+geom_spatraster(data = rs1$Distance_to_road)+
   #facet_wrap(~ lyr, scales = "free")+
   theme_bw()+scale_fill_hypso_c()+
   geom_sf(data = gnwt_sf) + labs(fill="Distance_to_road")+
   theme(legend.position = "bottom")
 
 fcp <- ggplot()+geom_spatraster(data = rs1$`% forest cover`)+
   #facet_wrap(~ lyr, scales = "free")+
   theme_bw()+scale_fill_hypso_c()+
   geom_sf(data = gnwt_sf)+ labs(fill="`% forest cover`")+
   theme(legend.position = "bottom")
 
 elevp <- ggplot()+geom_spatraster(data = rs1$Elevation)+
   #facet_wrap(~ lyr, scales = "free")+
   theme_bw()+scale_fill_hypso_c()+
   geom_sf(data = gnwt_sf) + labs(fill="Elevation")+
   theme(legend.position = "bottom")

cmb_plot<- plot_grid(chp, fcp, rdp, elevp) 

ggsave(cmb_plot, file="Combined_raster_plot.jpeg", width = 12, height = 8, units = "in", dpi=300, bg= "white") 
