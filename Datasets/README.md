
# Dataset Collection

Since we do not want to overload our repository with datasets, we will be linking sources, queries, and other related information here. We used [Google Cloud BigQuery](https://console.cloud.google.com/bigquery) to obtain much of this data.

# Texas County Vaccine Information

Run the following query in BigQuery:

```
WITH
  num_vaccine_sites_per_county AS (
  SELECT
    facility_sub_region_1 AS us_state,
    facility_sub_region_2 AS us_county,
    facility_sub_region_2_code AS us_county_fips,
    COUNT(DISTINCT facility_place_id) AS num_vaccine_sites
  FROM
    bigquery-public-data.covid19_vaccination_access.facility_boundary_us_all
  WHERE
    STARTS_WITH(facility_sub_region_2_code, "48")
  GROUP BY
    facility_sub_region_1,
    facility_sub_region_2,
    facility_sub_region_2_code ),
  total_population_per_county AS (
  SELECT
    LEFT(geo_id, 5) AS us_county_fips,
    ROUND(SUM(total_pop)) AS total_population
  FROM
    bigquery-public-data.census_bureau_acs.censustract_2018_5yr
  WHERE
    STARTS_WITH(LEFT(geo_id, 5), "48")
  GROUP BY
    LEFT(geo_id, 5) )
SELECT
  * EXCEPT(us_county_fips),
  ROUND((num_vaccine_sites * 1000) / total_population, 2) AS sites_per_1k_ppl
FROM
  num_vaccine_sites_per_county
INNER JOIN
  total_population_per_county
USING
  (us_county_fips)
ORDER BY
  sites_per_1k_ppl ASC;
```

# Covid Infections / Deaths + Census Data (USAFACTS)

The data provides United States Covid case and death counts by state and country, sourced from the CDC. The Census data provides useful information about the population and can be joined with other datasets to provide valuable insight about the population. 

Run the following query in BigQuery:

```
SELECT
  *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19
JOIN `bigquery-public-data.census_bureau_acs.county_2017_5yr` acs
ON covid19.county_fips_code = acs.geo_id
WHERE date = DATE_SUB(CURRENT_DATE(), INTERVAL 365 day) # Find Latest Yearly Census
```

# Texas Covid Infections / Deaths

Run the following query in BigQuery:

```
SELECT
  *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19 WHERE state = "TX"
```

# Google Global Mobility Report

This data shows how communities move differently due to Covid. These reports aim to provide insight into changes and responses, including chart movement trends over time by geography and other areas. [Click Here](https://www.google.com/covid19/mobility/index.html) and download the global dataset for the most information.

# Texas County Map

You may find a map of Texas counties [here](https://www.county.org/TAC/media/TACMedia/About%20Texas%20Counties/CountyMap.pdf). Results may be more impactful by cross referencing the map.