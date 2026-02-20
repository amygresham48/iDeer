# iDeer Central Repository

This repository contains all code written for the iDeer project (grant number)

## Code for current deer impact risk maps

### Notebooks

#### Location
current-risk-maps/notebooks/

#### Files
current_deer_impact_risk_EW_small_deer_workflow_June_2025_FINAL_EXTRACTION_V3.qmd
current_deer_impact_risk_EW_large_deer_workflow_June_2025_FINAL_EXTRACTION_V3.qmd

#### Description: 
These files contain the R code needed to create the current deer impact risk maps from small deer (Reeve's muntjac, Chinese water deer, roe deer) and large deer (Red deer, sika deer, fallow deer)

### Functions

#### Location:
current-risk-maps/functions

#### Files:
/extract_raster_pixels_func_10km_chunks.R
/extract_raster_pixels_to_wood_polygons_func_10km_chunks_mean_poly_values_NFI_LCM_merged_pre_chunked_exact.R
/extract_raster_pixels_to_wood_polys_func_site_level.R
/split_wood_polys_by_tile.R

#### Description: 
For full description of functions, see current-risk-map-functions-README.txt

## Code for iDeer RShiny application

The main repository contains all components needed to produce the iDeer Tool in shiny.io.

### App code:
/app.R

### Main functions to update risk maps:
/update_spatial_layers_after_planting_large_deer_CPTS_iDEER_TOOL_NFI_LCM_2023.R
/update_spatial_layers_after_planting_small_deer_CPTS_iDEER_TOOL_NFI_LCM_2023.R

### Side functions required by Main functions:
/extract_raster_pixels_func.R
/extract_raster_pixels_to_wood_polys_func_site_level.R
/update_wood_polys_function_NFI_split_new_polys.R

#### Description: 
For full description of functions, see update-risk-map-functions-README.txt


