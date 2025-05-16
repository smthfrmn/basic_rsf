# simple_rsf
Repository for doing simple resource selection analysis in Moveapps 
Github repository: https://github.com/nilanjanchatterjee/basic_rsf

## Description

The app models the *used* and *available* points generated in the **Background point generator** app into a resource selection function for a population or each individual from a tracking dataset.

## Documentation
   
This app models the data for resource selection analysis. For the analysis, *used* points are the locations from the radio-telemetry data and *background* points are randomly generated locations to model the habitat selection. The analysis can be carried out for each individual or for the population in this app and the number of background points can also be defined by the user as a ratio between *used* and *background* points in the `Background Point Generator` app. 

The user can either upload two rasters (containing multiple layers) or the default variables will be used to fit the RSF. There are currently four default variables:
- [Percentage forest cover](https://lpdaac.usgs.gov/products/gfcc30tcv003/)
- [Landuse-landcover](https://modis.gsfc.nasa.gov/data/dataprod/mod12.php)
- [Global human modification](https://data.nasa.gov/dataset/Global-Human-Modification-of-Terrestrial-Systems/4t8v-e7f3/about_data)
- [Elevation in meters](https://cran.r-project.org/web/packages/elevatr/index.html)


If the user uploads their own rasters, environmental variables from all layers will be extracted and included in the model. Because `terra` by default considers all raster layers to be continuous (versus categorical), this app programmatically determines if the layer is categorical and converts the variables, accordingly, to factors. 


The output file includes a coefficient plot of the variables used in the resource selection function and a data frame of regression coefficients. Coefficients overlapping zero shows non-significance whereas coefficients greater than 0 shows preference and less than 0 signifies avoidance of the environmental variable. 

## Input data

`move2::move2_loc` and following use of the Background points generator App (set to the same scale -- "individual" or "population")

## Output data

`move2::move2_loc`

### Artefacts

*rsf_coefficients_output.csv* with the regression coefficients   

The coefficient summary of the model(s) fit when the app was run. Note that if the user did not upload any rasters and the default environmental variables were used the landuse data has 17 classes and the coefficient refers to the specific habitat with respect to first habitat class. Detailed classification of the different landuse-landcover classes can be found here (LC_Type1)[https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1].

- `<track_id_var>`: the track id (e.g., whatever was passed in the move2 object from the previous app) for which the RSF was fit. Only applicable when `scale` is "Individual".
- `term`: the environmental variable used in the analysis
- `estimate`: the coefficient estimate of the habitat variables
- `std.error`: the standard error of the coefficient estimate
- `statistic`: the value of the z-statistic of the coefficient estimate
- `p.value`: whether the coefficient estimate is significantly different from 0 or not
- `conf.low`: the lower confidence limit of the coefficient estimate (95% CI)
- `conf.high`: the higher confidence limit of the coefficient estimate (95% CI)

*model_coefficient_plot.jpeg*

- `delx` represent the affinity towards the centroid location of the individual/population for longitude
- `dely` represent the affinity towards the centroid location of the individual/population for latitude
- `delxy` represent the affinity towards the centroid location of the individual/population for both the axes
- other environmental variables with their respective preference/avoidance


*raster_plot.jpeg*

A plot all the rasters from which environmental variables were extracted and used to fit the RSF. Movement trajectories are plotted on each. Note that if `scale` is "Individual", individuals are represented by different colors.