library(move2)
#library(raster)
library(terra)
library(tidyterra)
library(sf)
#library(sp)
library(ggplot2)
library(elevatr)
library(tidyverse)
library(progress)
library(cowplot)

rFunction <- function(data, raster_file= NULL, categorical= FALSE, 
                      type_ind = FALSE, num_layers = 1)
{
  data <- data |> mutate(location.long = sf::st_coordinates(data)[,1],
                         location.lat = sf::st_coordinates(data)[,2],
                         trackId = mt_track_id(data))
   
  data_df <-as.data.frame(data)
  
  
  data_df <- data_df %>% 
    mutate(delx = location.long - mean(location.long), 
           dely= location.lat - mean(location.lat)) %>% 
    mutate(distxy = sqrt((delx)^2 + (dely)^2))
  
  data_sf <-  data_df |> filter (case == TRUE) |>
    st_as_sf(coords = c("location.long", "location.lat"), crs=4326)
  
  rast_ext <- ext(c(xmin = min(data_df$location.long)-0.5, xmax = max(data_df$location.long)+0.5,
                    ymin = min(data_df$location.lat)-0.5, ymax = max(data_df$location.lat)+0.5))
  
  #### Upload the raster file
  if(is.null(raster_file) == FALSE & is.null(raster_cat_file) == FALSE)
    {    rast1 <-rast(paste0(getAppFilePath("raster_file"),"raster.tif"))
         rast2 <-rast(paste0(getAppFilePath("raster_cat_file"),"raster_cat.tif"))
         raster_cat_dat <- terra::extract(rast2, cbind(data_df$location.long, data_df$location.lat))
         cols2 <- colnames(raster_cat_dat)
         ## Regression of dummy variables for categorical raster
         # if (categorical){
         #   if(nlyr(rast1) >1)
         #   {logger.info(print("The raster contains multiple ")))
         #   
         #   modglm <-glm(case ~ as.factor(raster_dat) + delx + dely + distxy,data = data_df)
         # } 
    if(nlyr(rast1) != num_layers)
    {logger.info(print("User provided number of layers does not match with the number of layers of the uploaded raster"))}
    raster_dat <- terra::extract(rast1, cbind(data_df$location.long, data_df$location.lat))
    cols <- colnames(raster_dat)
         ##Run the regression
         modglm <-glm(as.formula(paste("case ~ delx + dely + distxy + ", paste0("raster_cat_dat$",cols2), "+",
                                       paste0("raster_dat$",cols, collapse = "+"))), data = data_df)
         
         ## Arranging the regression output
         output <- broom::tidy(modglm, conf.int = TRUE)
         
         modplot <-ggplot(output) +
           geom_point(aes(y= term, x= estimate), col ="blue")+
           geom_linerange(aes(y= term,
                              xmin= conf.low , 
                              xmax= conf.high))+
           labs(y= "Variable", x= "Coefficient Estimate")+
           theme_bw()+
           coord_cartesian(xlim = c(-10,10))
         
         rast_crop <- c(crop(rast1, rast_ext), crop(rast2, rast_ext))
         ### plot the raster layers )
         ### plot the raster layers 
         rast_plot <- ggplot()+geom_spatraster(data = rast_crop)+
                  facet_wrap(~ lyr)+
                  theme_bw()+scale_fill_hypso_c()+
                  geom_sf(data = data_sf, size=0.5)+ 
            theme(legend.position = "right",
                 axis.text = element_text(size = 6))
         
         }else
  {  ##Download the elevation data
    elev_dat <- get_elev_point(as.data.frame(cbind("x"=data_df$location.long, 
                                                   "y"=data_df$location.lat)),
                                 prj=st_crs(4326),  units="meters", src = "aws")
    
    data_df$elevation <- scale(elev_dat$elevation)
    
    ### Load the raster data
    raster <- rast(paste0(getAppFilePath("raster_file"),"raster.tif"))
    raster$LC <- as.factor(raster$LC)
    data_df$forest_cover <- as.numeric(scale(terra::extract(raster$tree_canopy_cover, 
                                 cbind(data_df$location.long, data_df$location.lat),
                          method= "bilinear")))
    data_df$lulc <- terra::extract(raster$LC, 
                                              cbind(data_df$location.long, data_df$location.lat))
    
    raster_ghm <- rast(paste0(getAppFilePath("raster_file"),"raster_hm.tif"))
    data_df$ghm <- terra::extract(raster_ghm$gHM, 
                                   cbind(data_df$location.long, data_df$location.lat))
    
    ##Run the regression
    #str(data_df)
    modglm <-glm(case ~ elevation + forest_cover + (lulc$LC) + ghm$gHM + 
                   delx + dely + distxy, data = data_df,
                 family = binomial(link = "logit"))
    
    ## Arranging the regression output
    output <- broom::tidy(modglm, conf.int = TRUE)
    
    modplot <-ggplot(output) +
      geom_point(aes(y= term, x= estimate), col ="blue")+
      geom_linerange(aes(y= term,
                         xmin= conf.low , 
                         xmax= conf.high))+
      labs(y= "Variable", x= "Coefficient Estimate")+
      theme_bw()+
      coord_cartesian(xlim = c(-10,10))
    
    
    rast_crop <- c(crop(raster$tree_canopy_cover, rast_ext), crop(as.numeric(raster$LC), rast_ext),
                   crop(raster_ghm, rast_ext))
    
    rast_crop <- c(crop(raster$tree_canopy_cover, rast_ext), crop(raster$LC, rast_ext),
                   crop(raster_ghm, rast_ext))
    
    elev <-get_elev_raster(rast_crop, src = "aws",  prj = st_crs(4326),  units="meters", z=9)
    
    ### raster plots to check 
    fcp <- ggplot()+geom_spatraster(data = rast_crop$tree_canopy_cover)+
      theme_bw()+ scale_fill_hypso_c(direction = -1, palette = "dem_screen")+
      geom_sf(data = data_sf, size = 0.25)+ labs(fill="`% forest cover`")+
      theme(legend.position = "right",
            axis.text = element_text(size = 5)) +ggtitle("Percentage forest cover")
    
    lulcp <- ggplot()+geom_spatraster(data = rast_crop$LC)+
      theme_bw()+scale_fill_hypso_d()+
      geom_sf(data = data_sf, size = 0.25) + labs(fill="Landuse\nlandcover")+
      theme(legend.position = "right",
            axis.text = element_text(size = 5)) +ggtitle("Landuse-landcover")
    
    hmp <- ggplot()+geom_spatraster(data = rast_crop$gHM)+
      theme_bw()+scale_fill_viridis_c(direction = -1)+
      geom_sf(data = data_sf, size = 0.25)+ labs(fill="Global human\nmodification")+
      theme(legend.position = "right",
            axis.text = element_text(size = 5)) +ggtitle("Human modification")
    
    elevp <- ggplot()+geom_spatraster(data = elev)+
      theme_bw()+scale_fill_wiki_c()+
      geom_sf(data = data_sf, size = 0.25) + labs(fill="Elevation")+
      theme(legend.position = "right",
            axis.text = element_text(size = 5))+ggtitle("Elevation")
    
    rast_plot<- plot_grid( fcp, elevp,lulcp, hmp) 
  # if(predict=TRUE)
  # { 
  #   elev <-get_elev_raster(data_bbox, src = "aws",  
  #                          prj = sp::proj4string(data),  units="meters", z=9)  
  #   raster_dat <-extract(elev, coordinates(data), fun= "mean")
  #   
  # }
  }
  
  ## Regression of dummy variables for categorical raster
  # if (categorical){
  #   
  #   modglm <-glm(case ~ as.factor(raster_dat) + delx + dely + distxy,data = data_df)
  # } 
  
    
    if(type_ind){
      uid <-unique(data_df$trackId)
      #ind_result <-list()
      glmfits <- data_df %>% nest(data=-trackId) %>% 
        mutate(mod = map(data, function(x) (glm(case ~ elevation + forest_cover + lulc$LC + ghm$gHM +
                                                     delx + dely + distxy, data = x,
                                                    family = binomial(link = "logit")))))
      
      coefglm<-NULL
      
      for(i in 1:length(uid)){
        {
          coefglm<-rbind(coefglm, cbind(trackId=glmfits$trackId[[i]],  broom::tidy(glmfits$mod[[i]], conf.int = TRUE)))
        }                  
      }
      output <- coefglm
      
      modplot <-ggplot(output) +
        geom_point(aes(y= term, x= estimate), col ="blue")+
        geom_linerange(aes(y= term,
                           xmin= conf.low , 
                           xmax= conf.high))+
        labs(y= "Variable", x= "Coefficient Estimate")+
        facet_wrap(~ trackId)+
        theme_bw()+
        coord_cartesian(xlim = c(-10,10))
      # for(i in 1:length(uid)){
      #   data_temp <- data
      #   ##to-do :include also the categorical raster function
      #   ind_result[[i]] <-modglm <-glm(case ~ elevation + fc + lulc$LC_Type1
      #                                  + delx + dely + distxy, data = data_df,
      #                                  family = binomial(link = "logit"))
      # }
    }
    
  
  
  ggsave(modplot, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Coeffcient_plot.jpeg"),
         width=9, height=6, units= "in", dpi=300)
  
  ggsave(rast_plot, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Raster_plot.jpeg"),
         width=9, height=6, units= "in", dpi=300, bg= "white")
  
  write.csv(output, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"RSF_output.csv"),
                                  row.names = FALSE)
  return(data)
}
