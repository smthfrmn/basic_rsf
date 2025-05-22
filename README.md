# Basic RSF

MoveApps

Repository for doing simple resource selection analysis in Moveapps 
Github repository: https://github.com/nilanjanchatterjee/basic_rsf

## Description

The app models the *used* and *available* points generated in the **Background point generator** app into a resource selection function for a population or each individual from a tracking dataset.

## Documentation
   
This app models the data for resource selection analysis. For the analysis, *used* points are the locations from the radio-telemetry data and *background* points are randomly generated locations to model the habitat selection. The analysis can be carried out for each individual or for the population in this app and the number of background points can also be defined by the user as a ratio between *used* and *background* points in the `Background Point Generator` app. 

The user can either upload two rasters (containing multiple layers) or the default variables will be used to fit the RSF. There are currently four default variables:
- [Percent Tree Cover](https://lpdaac.usgs.gov/products/gfcc30tcv003/)
- [Land Cover Type](https://modis.gsfc.nasa.gov/data/dataprod/mod12.php)
- [Global Human Modification](https://sedac.ciesin.columbia.edu/data/set/Lulc-human-modification-terrestrial-systems)
- [Elevation in meters (but scaled and centered)](https://cran.r-project.org/web/packages/elevatr/index.html)


If the user uploads their own rasters, environmental variables from all layers will be extracted and included in the model. Because `terra` by default considers all raster layers to be continuous (versus categorical), this app programmatically determines if the layer is categorical and converts the variables, accordingly, to factors. 


The output file includes a coefficient plot of the variables used in the resource selection function and a data frame of regression coefficients. Coefficients overlapping zero shows non-significance whereas coefficients greater than 0 shows preference and less than 0 signifies avoidance of the environmental variable. 


### Application scope
#### Generality of App usability
*State here if the App was developed for a specific species, taxon or taxonomic group, or to answer a specific question. How might it influence the scope and utility of the App. This information will help the user to understand why the App might be producing no or odd results.*

*Examples:*

This App was developed using data from birds and ungulates, but should function with any species. Default environmental variables are all terrestrial. If your species lives in the water, make sure you upload appropriate rasters containing variables that vary in water environments.

#### Required data properties
To use this App, you must first run the Background Points Generator App which will generate a bunch of background points that will, with the observed points, be used to model the resource selection function.

### Input type
`move2::move2_loc` and following use of the Background points generator App (set to the same scale -- "individual" or "population")

### Output type
`move2::move2_loc`

### Artefacts

*rsf_coefficients_output.csv* with the regression coefficients   

The coefficient summary of the model(s) fit when the app was run. Note that if the user did not upload any rasters and the default environmental variables were used the landuse data has 17 classes and the coefficient refers to the specific habitat with respect to first habitat class. Detailed classification of the different landuse-landcover classes can be found here [LC_Type1](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1).

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

### Settings 

`Scale`: Specify how you want to perform the resource selection function (population or individual-level). Your scale here should match the one you used in the Background Points Generator App.

`Raster File One [Optional]`: Upload your local raster file here. All layers in the raster will be included as a custom variable in the Resource Selection Function (RSF). If you do not upload any rasters, consider selecting at least one of the default environmental variables offered below. If the raster is a different projection (i.e., different CRS) than the input move data, the raster will be projected to the CRS of the move data. The projecting process will make the app run for longer. You can view the logs to see if this occurred. Consider projecting your raster locally to match (most likely to WGS 84 or EPSG:4326) to improve the performance time.

`Raster File Two [Optional]`: Upload a second local raster file here, if you have it. Refer to the 'Note' in the 'Raster File One' description.

`Percent tree cover`: For this setting and the following three checkboxes, select the checkbox if you want to include the default variables in your RSF. If you didn't upload any custom rasters, you should select at least one default variable; otherwise, your RSF will only include coefficients for selection for the centroid location. You can also include default variables in an RSF with custom variables. See data source: https://lpdaac.usgs.gov/products/gfcc30tcv003/. Values will be scaled and centered before the model is fit.

`Land cover type`: Include this as default variable. See data source: https://modis.gsfc.nasa.gov/data/dataprod/mod12.php

`Global human modification`: Include this as a default variable. See data source: https://data.nasa.gov/dataset/Global-Human-Modification-of-Terrestrial-Systems/4t8v-e7f3/about_data


`Elevation (m)`: Include this default variable: Note: this data is sourced from a third party and can make the App run for a while, especially if your move data covers a large geographic area. See data source: https://cran.r-project.org/web/packages/elevatr/index.html. Values will be scaled and centered before the model is fit.

### Changes in output data

The input data remains unchanged.

### Most common errors

The *model_coefficient_plot.jpeg* may contain confidence intervals that do not overlap with the actual coefficient estimate. This is an indication that the model did not actually converge. Check the logs to see what messages were recorded during model fitting. 

It is possible the App will incorrectly interpret an uploaded raster's data as categorical when it should be continuous. If this happens, either log an issue on github on the App or email Smith Freeman at freem850@umn.edu with the raster you tried to use.

### Null or error handling

**Settings `Raster File One [Optional]` and `Raster File Two [Optional]`:** If the user uploads a file but it does not end in ".tif", the App will log an error and return immediately.