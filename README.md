**Understanding Phenology using Sentinel-2A**

**Introduction**

Annually recurring biological timing in plants like flowering, bud-bursting, and leaf fall are known as tree phenology. Tree phenology is highly sensitive, so they act as a visual indicator of climate change. Evidence of the early start of the growing season in temperate regions induced by climate change has been reported in many studies. The biological events that occur early or late can also influence gaseous exchange, succession, carbon cycling, and ecosystem productivity. Also, the net primary production of forest and its spatial pattern are increased by long growing seasons.

Traditionally, phenological studies rely on human observation and noting down the data based on their subjective judgement, and were confined to a limited geographical extent. Satellite-based remote sensing can be a reliable tool to study and monitor the phenology of trees through continuous observation. These satellites collect the data in the form of light waves reflected from earth's surface, and are recorded in digital numbers. Those digital numbers are then converted to unique spectral signatures and help in the identification of different objects on the earth’s surface. Moreover, spectral reflectance is generated due to the ability of the target object to reflect electromagnetic spectrum under different wavelengths making it easy to detect. The reflectance is then used to generate different indices that best help to discern the target object.

For a smaller geographical extent, medium-resolution datasets from Landsat or Sentinel images are popular for phenological study in a higher spatial and temporal resolution because of their smaller uncertainty and free availability. Finally, this study aims to combine the phenological dynamics with remote sensing dataset and dig deeper to understand the impact of fertilizer on forest trees species and their capacity to retain leaf.

**Methodology**

I will use Google Earth Engine to select the images from the region of interest for the time interval from January 2019 to December 2021. Google Earth Engine (Gorelick et. al, 2017) has the feature of selecting any bands from the specified date, manipulating them as per our need and generating the graph. Google Earth Engine is a cloud computing platform which helps to provide access to super-computers and easily handles large datasets for analysis. For this study, NDVI from Sentinel-2 data was calculated using band 8 (NIR) and band 4 (Red) )values. These NDVI values act as an index for phenological cycle because they are higher during summer with all the leaf on it and minimum during winter because of leaf fall. Cloud cover was masked before calculating the vegetation indices as they can alter the result. Data were visualized and downloaded for further statistical analysis in R-studio 4.3.1 (R core Team, 2013).

After filtering the cloud cover data, the data was downloaded for each treatment plot. The data was imputed for each day using the stine interpolation method (Stineman, 1980). This method is considered better because it produces values that do not have more inflection points required by dataset. First, there were noise and outliers in the dataset. Remote sensing data have such issues because data are weather dependent. Smoothing the data helps to remove noise or outliers on the dataset. Savitsky-Golay filter for smoothening time-series data is considered best among others (Press and Teukolsky, 1990).

After smoothening the data, I tried to fit the model for the data. Further, the analysis will focus on characterizing the start of the season (SOS) and end of the season (EOS) by detecting the abrupt increase and decrease in slope of NDVI curve. For this study, we consider Start of Season (SOS) as the onset of the leaves in the trees. For the vegetation index, it will be the onset of the rising of the values after the constant values occurring due to leaf fall. As leaves start budding out, they will reach the maximum and will eventually fall out during winter. The vegetation index will have a peak followed by downfall and eventually a constant minimum value because of no leaves on the trees. This phase of decreasing VI followed by a constant minimum represents EOS. Break for Additive Season and Trends (BFAST) algorithm will be used to detect breakpoints for SOS and EOS (Verbesselt et. al, 2010). So, we will understand how precisely Bfast algorithm is able to SOS and EOS. Also, these breakpoints for the data across the plot will be compared to detect if there is any temporal difference subjected to the effect of fertilization.

**Field Work**

For this study, I collected corner stake locations for all MELNHE plots using Trimble Geo 7x Global Positioning System (GPS) during summer 2022. This device collects point locations under 1-2 meters of accuracy.

**Lab Work**

After precise point locations are collected, I prepared the boundary map for all the plots. The shapefile for the plot will be uploaded to Google Earth Engine so time-series satellite images can be collected from the delineated areas of interest. Most importantly, mean value for each plot will be used for the analysis. Data will be collected for a period of two-year (2019-2020) time range collected by Sentinel 2A and 2B from the start of the growing season to the end of the season.

**Data analysis**

I used Google Earth Engine to select the images from the region of interest for the time interval from January 2019 to December 2021. Google Earth Engine (Gorelick et. al, 2017) has the feature of selecting any bands from the specified date, manipulating them as per our need and generating the graph. Google Earth Engine is a cloud computing platform which helps to provide access to super-computers and easily handles large datasets for analysis. For this study, NDVI from Sentinel-2 data was calculated using band 8 (NIR) and band 4 (Red) )values. These NDVI values act as an index for phenological cycle because they are higher during summer with all the leaf on it and minimum during winter because of leaf fall. Cloud cover was masked before calculating the vegetation indices as they can alter the result. Data were visualized and downloaded for further statistical analysis in R-studio 4.3.1 (R core Team, 2013).
After filtering the cloud cover data, the data was downloaded for each treatment plot. The data was imputed for each day using the stine interpolation method (Stineman, 1980). This method is considered better because it produces values that do not have more inflection points required by dataset. First, there were noise and outliers in the dataset. Remote sensing data have such issues because data are weather dependent. Smoothing the data helps to remove noise or outliers on the dataset. Savitsky-Golay filter for smoothening time-series data is considered best among others (Press and Teukolsky, 1990).
After smoothening the data, I tried to fit the model for the data. Further, the analysis will focus on characterizing the start of the season (SOS) and end of the season (EOS) by detecting the abrupt increase and decrease in slope of NDVI curve. For this study, we consider Start of Season (SOS) as the onset of the leaves in the trees. For the vegetation index, it will be the onset of the rising of the values after the constant values occurring due to leaf fall. As leaves start budding out, they will reach the maximum and will eventually fall out during winter. The vegetation index will have a peak followed by downfall and eventually a constant minimum value because of no leaves on the trees. This phase of decreasing VI followed by a constant minimum represents EOS. Break for Additive Season and Trends (BFAST) algorithm will be used to detect breakpoints for SOS and EOS (Verbesselt et. al, 2010). So, we will understand how precisely Bfast algorithm is able to SOS and EOS. Also, these breakpoints for the data across the plot will be compared to detect if there is any temporal difference subjected to the effect of fertilization.