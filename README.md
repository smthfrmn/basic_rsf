# simple_rsf
Repository for doing simple resource selection analysis in Moveapps 
Github repository: https://github.com/nilanjanchatterjee/basic_rsf

## Description
This app models resource selection with the *used* animal locations and *available* points generated in the **Background point generator** app. The user can upload resource layers for the model or use global rasters of elevation, forest cover, land cover and human modification that are provided as default. Habitat selection can be modeled for the entire data set (population) or each individual. 

## Documentation
   
This app models the data for resource selection analysis by fitting a generalized linear model (GLM). For the analysis, *used* points are the locations from the input data set and *background* points are randomly generated locations to model the habitat selection. The analysis can be carried out for each individual or for the population, and the number of background points can also be defined by the user as a ratio between *used* and *background* points in the `Background Point Generator` app. 

Environmental variables representing relevant habitat and environmental characteristics can be uploaded to the app to be included in the analysis. :Please read the following requirements for uploading input layers:: 
* These resource layers must be provided in a **raster file** using the **WGS84 lat-long** (EPSG:4326) coordinate system. Other projection formats will lead to an error in the modelling. 
* The spatial extent of the raster must include the entire extent of the used and available points. 
* Multiple layers may be included in a raster file.
* Be careful to identify whether your raster represents a categorical (e.g., habitat classification) or continuous (e.g., elevation in meters) variable, and upload the file in the corresponding setting.

The app includes several global resource layers that are used as default for the analysis:  
* elevation from [Amazon Web Services Terrain Tiles](https://registry.opendata.aws/terrain-tiles/) using the [`elevatR`](https://cran.r-project.org/web/packages/elevatr/index.html) package (ground resolution varies by latitude and is 216.2 m at 45° latitude, [read more here](https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution)) (displayed in outputs as 'elevation')
* percentage tree cover from the [Global Forest Cover Change (GFCC)](https://doi.org/10.5067/MEaSUREs/GFCC/GFCC30TC.003) dataset (30-m resolution) (displayed in outputs as 'forest_cover')
* land cover from [MODIS Land Cover Type Product MCD12Q1 Version 6.1](https://doi.org/10.5067/MODIS/MCD12Q1.061), Land Cover Type 1 (Annual International Geosphere-Biosphere Programme (IGBP) classification) (500-m resolution) (displayed in outputs as 'lulc$LC#', see categories below)
* human modification the SEDAC [Global Human Modification of Terrestrial Systems](https://doi.org/https://doi.org/10.7927/edbc-3z60) dataset (1-km resolution, representing a median year of 2016) (displayed in outputs as ghm$gHM)

In addition, three other spatial variables are used:
* distance from the longitudinal centroid location of the individual/population (displayed in outputs as 'delx')
* distance from the latitudinal centroid location of the individual/population (displayed in outputs as 'dely')
* distance from the centroid location of the individual/population (displayed in outputs as 'delxy')

The outputs includes a coefficient plot of the variables used in the resource selection function and a data frame of regression coefficients. Coefficients overlapping zero indicate non-significance (no selection), whereas coefficients greater than 0 indicate preference for and less than 0 signify avoidance of the environmental variable. 

Land cover key: The Annual International Geosphere-Biosphere Programme (IGBP) classification from product [MCD12Q1](https://doi.org/10.5067/MODIS/MCD12Q1.061) (LC_Type1) has 17 classes. The coefficient used in the model refers the specific habitat with respect to first habitat class. The key is provided in Table 3 of the MODIS 12 User Guide or at [Google Earth Engine](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1):
1: Evergreen Needleleaf Forests: dominated by evergreen conifer trees (canopy >2m). Tree cover >60%.  
2: Evergreen Broadleaf Forests: dominated by evergreen broadleaf and palmate trees (canopy >2m). Tree cover >60%.  
3: Deciduous Needleleaf Forests: dominated by deciduous needleleaf (larch) trees (canopy >2m). Tree cover >60%.  
4: Deciduous Broadleaf Forests: dominated by deciduous broadleaf trees (canopy >2m). Tree cover >60%.  
5: Mixed Forests: dominated by neither deciduous nor evergreen (40-60% of each) tree type (canopy >2m). Tree cover >60%.  
6: Closed Shrublands: dominated by woody perennials (1-2m height) >60% cover.  
7: Open Shrublands: dominated by woody perennials (1-2m height) 10-60% cover.  
8: Woody Savannas: tree cover 30-60% (canopy >2m).  
9: Savannas: tree cover 10-30% (canopy >2m).  
10: Grasslands: dominated by herbaceous annuals (<2m).  
11: Permanent Wetlands: permanently inundated lands with 30-60% water cover and >10% vegetated cover.  
12: Croplands: at least 60% of area is cultivated cropland.  
13: Urban and Built-up Lands: at least 30% impervious surface area including building materials, asphalt and vehicles.  
14: Cropland/Natural Vegetation Mosaics: mosaics of small-scale cultivation 40-60% with natural tree, shrub, or herbaceous vegetation.  
15: Permanent Snow and Ice: at least 60% of area is covered by snow and ice for at least 10 months of the year.  
16: Barren: at least 60% of area is non-vegetated barren (sand, rock, soil) areas with less than 10% vegetation.  
17: Water Bodies: at least 60% of area is covered by permanent water bodies.  

## Input data

*move/moveStack* in Movebank format 

## Output data

*move/moveStack* in Movebank format 

### Artefacts

`RSF_output.csv`: summary of the regression coefficients including the following attributes:  
- `trackId` denotes the individual id if the resource selection analysis is done for individual wise
- `term` stands for the environmental variable used in the analysis
- `estimate` represents the coefficient estimate of the habitat variables
- `std.error` represents the standard error 
- `statistic` represents the value of the z-statistic
- `p.value` represents whether the coefficient estimate is significantly different from 0 or not
- `conf.low` shows the lower confidence limit of the coefficient estimate (95% CI)
- `conf.high` shows the higher confidence limit of the coefficient estimate (95% CI)

`Coefficient_plot.jpeg`: a plot of the coefficient estimates for each variable showing the lower and upper confidence limits (95% CI). See above for a very general explanation of how to interpret the results.

`Raster_plot.jpeg`: a plot of the environmental raster(s) overlain with the animal presence locations

### Settings
**Continuous raster file (`raster_file`):** optional upload of a raster file containing continuous resource data. See requirements above.

**Categorical raster file (`raster_cat_file`):** optional upload of a raster file containing categorical resource data. See requirements above.

**Analysis level (`type_ind`):** selection to model the population (entire data set) or each individual. Default is `population`.

**Number of layers in the uploaded raster (`num_layers`):** an integer representing the number of layers in the uploaded raster file, if provided

### Null or error handling

Please report errors and we will describe explanations for recurring questions here.