# simple_rsf
Repository for doing simple resource selection analysis in Moveapps 
Github repository: https://github.com/nilanjanchatterjee/basic_rsf

## Description
The app models the *used* and *available* points generated in the **Background point generator** app into a resource selection function for population or each individuals from a tracking dataset.

## Documentation
   
This app models the data for resource selection analysis. For the analysis, *used* points are the locations where the animal is detected and *background* points are randomly generated locations to model the habitat selection. The analysis can be carried out for each individual or for the population in this app and the number of background points can also be defined by the user as a radio between *used* and *background* points. 

Environmental variables needs to be projected in the **lat-long** format. Other projection formats will lead to error in the modelling. 


## Input data

*move/moveStack* in Movebank format 

## Output data

*move/moveStack* in Movebank format 

###Artefacts

*csv* with the regression coefficients   
*jpeg* with the coefficient plot
