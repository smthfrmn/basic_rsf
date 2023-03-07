library(move)
library(raster)
library(sf)
library(sp)
library(ggplot2)
library(elevatr)
library(dplyr)

RFunction <- function(data, raster_file= NULL, categorical= FALSE)
{
  data_df <-as.data.frame(data)
  
  data_df$delx <- data_df$location.long - mean(data_df$location.long)
  data_df$dely <- data_df$location.lat - mean(data_df$location.lat)
  
  # data_bbox <- expand.grid( c(min(data_df$location.long), max(data_df$location.long)),
  #                           c(min(data_df$location.lat), max(data_df$location.lat)))
  
   
  #### Upload the raster file
  if(is.null(raster_file) == FALSE){  rast1 <-raster(paste0(getAppFilePath("raster_file"),"raster.tif"))
         raster_dat <-extract(rast1, coordinates(data) ,method='bilinear', fun=mean)
         #logger.info(print("The uploaded raster is not in lat-long projection, please change the projection")))
  }else
  {  ##Download the elevation data
    elev_dat <- get_elev_point(as.data.frame(cbind(data_df$location.long, data_df$location.lat)),
                                 prj=proj4string(data),  units="meters", src = "aws")
    raster_dat <- scale(elev_dat$elevation)
  
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
    
    modglm <-glm(case ~ as.factor(raster_dat) + delx + dely,data = data_df)
  } 
  
    ##Run the regression
    modglm <-glm(case ~ raster_dat + delx + dely,data = data_df)
  
    ## Arranging the regression output
  output <- as.data.frame(summary(modglm)$coefficients)
  
   modplot <-ggplot(output) +
    geom_point(aes(x= rownames(output), y= Estimate), col ="blue")+
    geom_linerange(aes(x= rownames(output),
                       ymin= Estimate-1.96*`Std. Error` , 
                       ymax= Estimate+1.96*`Std. Error`))+
     labs(x= "Variable", y= "Estimate")+
    theme_bw()
  
  ggsave(modplot, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Coeffcient_plot.jpeg"),
         width=9, height=6, units= "in", dpi=300)
  write.csv(output, file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"RSF_output.csv"))
  return(data)
}
