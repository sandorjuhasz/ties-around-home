# Spatially concentrated social capital of urban residents

This repository contains data and code to reproduce the results and figures of the following preprint:

Kovács, Á. J., Juhász, S., Bokányi, E., & Lengyel, B. (2021). Spatially concentrated social capital of urban residents. 1–20. Retrieved from http://arxiv.org/abs/2107.13474

## Datafiles

* `data/cbsacode_shortname_tracts.csv`: maps census tract geoids to cbsacodes of metropolitan areas and their short names, `;`-separated
* `data/censusdata_top50_2012.csv`: ACS 2012 census tract level demographic data
* `data/censustract_geoms_top50.geojson`: map of the census tracts of the top 50 metropolitan areas of the US, each row is a standalone JSON corresponding to one tract
* `data/usageousers_city_follower_networks.rpt.gz`: metro area mutual Twitter follower networks, user_id1 and user_id2 are Twitter profile ids, cbsacode is the metro area
* `data/usageousers_data_export_with_tract_geoid_top50.csv.gz`: for each user_id, geoid of the home and work census tracts in metropolitan areas
