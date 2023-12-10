library(move)
#library(raster)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(elevatr)
library(tidyverse)
library(progress)

rFunction <- function(data, raster_file= NULL, categorical= FALSE, 
                      type_ind = FALSE, num_layers = 1)
{
  data_df <-as.data.frame(data)
  
  data_df <- data_df %>% 
    mutate(delx = location.long - mean(location.long), 
           dely= location.lat - mean(location.lat)) %>% 
    mutate(distxy = sqrt((delx)^2 + (dely)^2))
  
  ### Load the raster data
  raster <- rast(paste0(getAppFilePath("raster_file"),"raster.tif"))
  
  # data_df$delx <- data_df$location.long - mean(data_df$location.long)
  # data_df$dely <- data_df$location.lat - mean(data_df$location.lat)
  # data_df$distxy <- sqrt((data_df$delx)^2+(data_df$dely)^2)
  # 
  # data_bbox <- expand.grid( c(min(data_df$location.long), max(data_df$location.long)),
  #                           c(min(data_df$location.lat), max(data_df$location.lat)))

   
  #### Upload the raster file
  if(is.null(raster_file) == FALSE)
    {    rast1 <-rast(paste0(getAppFilePath("raster_file"),"raster.tif"))
    if(nlyr(rast1) != num_layers)
      {logger.info(print("User provided number of layers does not match with the number of layers of the uploaded raster"))}
         raster_dat <-extract(rast1, coordinates(data) ,method='bilinear', fun=mean)
         #logger.info(print("The uploaded raster is not in lat-long projection, please change the projection")))
         ## Regression of dummy variables for categorical raster
         if (categorical){
           
           modglm <-glm(case ~ as.factor(raster_dat) + delx + dely + distxy,data = data_df)
         } 
         
         ##Run the regression
         modglm <-glm(case ~ raster_dat + delx + dely + distxy, data = data_df)
         
         }else
  {  ##Download the elevation data
    elev_dat <- get_elev_point(as.data.frame(cbind("x"=data_df$location.long, 
                                                   "y"=data_df$location.lat)),
                                 prj=st_crs(4326),  units="meters", src = "aws")
    
    data_df$elevation <- scale(elev_dat$elevation)
    data_df$forest_cover <- as.numeric(scale(terra::extract(raster$tree_canopy_cover, 
                                 cbind(data_df$location.long, data_df$location.lat),
                          method= "bilinear")))
    data_df$lulc <- terra::extract(raster$LC_Type1, 
                                              cbind(data_df$location.long, data_df$location.lat))
    ##Run the regression
    #str(data_df)
    modglm <-glm(case ~ elevation + forest_cover + (lulc$LC_Type1) +
                   delx + dely + distxy, data = data_df,
                 family = binomial(link = "logit"))
    
    ## Arranging the regression output
    output <- as.data.frame(summary(modglm)$coefficients)
    
    modplot <-ggplot(output) +
      geom_point(aes(y= rownames(output), x= Estimate), col ="blue")+
      geom_linerange(aes(y= rownames(output),
                         xmin= Estimate-1.96*`Std. Error` , 
                         xmax= Estimate+1.96*`Std. Error`))+
      labs(y= "Variable", x= "Coefficient Estimate")+
      theme_bw()+
      coord_cartesian(xlim = c(-10,10))
  # if(predict=TRUE)
  # { 
  #   elev <-get_elev_raster(data_bbox, src = "aws",  
  #                          prj = sp::proj4string(data),  units="meters", z=9)  
  #   raster_dat <-extract(elev, coordinates(data), fun= "mean")
  #   
  # }
  }
  
  ## Regression of dummy variables for categorical raster
  if (categorical){
    
    modglm <-glm(case ~ as.factor(raster_dat) + delx + dely + distxy,data = data_df)
  } 
  
    
    if(type_ind){
      uid <-unique(data_df$trackId)
      #ind_result <-list()
      glmfits <- data_df %>% nest(data=-trackId) %>% 
        mutate(mod = map(data, function(x) (glm(case ~ elevation + forest_cover + lulc$LC_Type1 +
                                                     delx + dely + distxy, data = data_df,
                                                    family = binomial(link = "logit")))))
      
      coefglm<-NULL
      
      for(i in 1:length(uid)){
        {
          coefglm<-rbind(coefglm, cbind(id=glmfits$trackId[[i]],  broom::tidy(glmfits$mod[[i]]$model)))
        }                  
      }
      output <- coefglm
      
      modplot <-ggplot(output) +
        geom_point(aes(y= rownames(output), x= Estimate), col ="blue")+
        geom_linerange(aes(y= rownames(output),
                           xmin= Estimate-1.96*`Std. Error` , 
                           xmax= Estimate+1.96*`Std. Error`))+
        labs(y= "Variable", x= "Coefficient Estimate")+
        facet_wrap(~ id)+
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
  write.csv(output, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"RSF_output.csv"))
  return(data)
}
