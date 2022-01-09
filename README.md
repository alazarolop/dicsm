<h1 align="center">
  <br>
  <a href="https://github.com/alazarolop/dicsm"><img src="https://media.githubusercontent.com/media/alazarolop/dicsm/main/4-disaggregation/images/graph_abs.png" alt="DiCSM" width="600"></a>
  <br>
  Disaggregation of Conventional Soil Maps
  <br>
</h1>
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

**Table of Contents** 

- [Features](#features)
- [Workflow](#workflow)
- [Requirements and Software](#requirements-and-software)
- [Feedback](#feedback)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Features

A new methodology for the disaggregation of polytaxic Soil Map Units of conventional soil maps that relies on the detection of *soil homogeneous areas with no prior definition*. It's based on: 

- The **unsupervised CLARA classification** technique with **Mahalanobis** distance within a space of selected covariates
- An expert **knowledge-driven correlation** of the areas with the Soil Taxonomy Units. 

Because of them, its application might be geared towards regions where the potential location of STU within SMUs is deemed unclear, and it has a potential to support two-stage strategies [(Lázaro-López, 2021)](https://doi.org/10.1071/SR20288). 
It's implemented in the Designation of Origen Campo de Borja (Gómez-Miguel, 2014).

## Workflow

It's divided into 4 sequential steps covering:

1. **Data sources** (*sources*):
The selection of remote sensing and DEM data sources, their acquisition and processing for input into the calculations of environmental covariates. It also gets the soil map ready.

1. **Covariates** (*covariates*):
Calculation, analysis and selection of covariates intended for geo-statistical techniques.
  
1. **Division** (*division*):
Division of Cartographic Soil Units by the unsupervised classification method to yield groups of delineations.

1. **Disaggregation** (*disaggregation*):
Correlation of the delineation groups with the Soil Taxonomic Units to give rise to a new disaggregated soil map with potentially homogeneous Soil Map Units. Then, it's validated with soil observations. 
  

##	Requirements and Software

**Soil map**

A digitalized soil map on top of the model for Soil Resource Inventories [(Lázaro-López et al., 2018)](https://doi.org/10.1051/e3sconf/20185002008). It resembles the Soil and Terrain database programme [(SOTER)](https://www.isric.org/projects/soil-and-terrain-soter-database-programme) and the National Soil Information System [(NASIS)](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/tools/?cid=nrcs142p2_053552) schemas.

**Data management**

[PostgreSQL](https://www.postgresql.org) and its spatial extension [PostGIS](https://postgis.net).

**Statistics**

The workflow is developed in [#Rstats](https://cran.r-project.org). Main packages are [*Tidyverse*](https://www.tidyverse.org), [*RPostgres*](https://rpostgres.r-dbi.org), [*sf*](https://r-spatial.github.io/sf/), [*raster*](https://github.com/rspatial/raster), [*foreach*](https://github.com/RevolutionAnalytics/foreach), [*bigmemory*](https://github.com/kaneplusplus/bigmemory), [*ClusterR*](https://github.com/mlampros/ClusterR), [*Nbclust*](https://github.com/cran/NbClust) and [*RSAGA*](https://github.com/r-spatial/RSAGA).

**GIS**

[*SAGA*](http://www.saga-gis.org/en/index.html) and [*QGIS*](https://www.qgis.org/en/site/) for data sources processing.

## Feedback

Feel free to send feedback on Twitter [@alazarolop](https://twitter.com/alazarolop) or file an issue. Feature requests as well as contributions are always welcome.

## License

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/alazarolop/dicsm">Disaggregation of Conventional Soil Maps</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://github.com/alazarolop">Alberto Lázaro-López</a> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Attribution 4.0 International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>

