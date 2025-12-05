# Get map unit interpretations from Soil Data Access by rule name

Get map unit interpretations from Soil Data Access by rule name

## Usage

``` r
get_SDA_interpretation(
  rulename,
  method = c("Dominant Component", "Dominant Condition", "Weighted Average", "None"),
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  include_minors = TRUE,
  miscellaneous_areas = TRUE,
  query_string = FALSE,
  not_rated_value = NA_real_,
  wide_reason = FALSE,
  dsn = NULL
)
```

## Arguments

- rulename:

  character vector of interpretation rule names (matching `mrulename` in
  `cointerp` table)

- method:

  aggregation method. One of: "Dominant Component", "Dominant
  Condition", "Weighted Average", "None". If "None" is selected one row
  will be returned per component, otherwise one row will be returned per
  map unit.

- areasymbols:

  vector of soil survey area symbols

- mukeys:

  vector of map unit keys

- WHERE:

  character containing SQL WHERE clause specified in terms of fields in
  `legend`, `mapunit`, or `component` tables, used in lieu of `mukeys`
  or `areasymbols`

- include_minors:

  logical. Include minor components? Default: `TRUE`.

- miscellaneous_areas:

  *logical*. Include miscellaneous areas (non-soil components) in
  results? Default: `TRUE`.

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- not_rated_value:

  used where rating class is "Not Rated". Default: `NA_real_`

- wide_reason:

  Default: `FALSE`; if `TRUE` apply post-processing to all columns with
  prefix `"reason_"` to create additional columns for sub-rule ratings.

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Value

a data.frame

## Details

### Rule Names in `cointerp` table

- AGR - Avocado Root Rot Hazard (CA)

- AGR - California Revised Storie Index (CA)

- AGR - Hops Site Suitability (WA)

- AGR - Map Unit Cropland Productivity (MN)

- AGR - Nitrate Leaching Potential, Nonirrigated (WA)

- AGR - No Till (TX)

- AGR - Pesticide Loss Potential-Soil Surface Runoff (NE)

- AGR - Ridge Till (TX)

- AGR - Selenium Leaching Potential (CO)

- AGR - Water Erosion Potential (NE)

- AGR - Wind Erosion Potential (TX)

- AGR - Winter Wheat Yield (MT)

- AGR-Pesticide and Nutrient Runoff Potential (ND)

- AGR-Rooting Depth (ND)

- American Wine Grape Varieties Site Desirability (Long)

- American Wine Grape Varieties Site Desirability (Medium)

- American Wine Grape Varieties Site Desirability (Very Long)

- AWM - Animal Mortality Disposal (Catastrophic) (MO)

- AWM - Irrigation Disposal of Wastewater (OH)

- AWM - Irrigation Disposal of Wastewater (VT)s

- AWM - Land Application of Municipal Biosolids, summer (OR)

- AWM - Manure and Food Processing Waste (MD)

- AWM - Manure and Food Processing Waste (OH)

- AWM - Overland Flow Process Treatment of Wastewater (VT)

- AWM - Rapid Infil Disposal of Wastewater (DE)

- AWM - Sensitive Soil Features (MN)

- AWM - Sensitive Soil Features (WI)

- BLM - Fencing

- BLM - Fire Damage Susceptibility

- BLM - Mechanical Treatment, Rolling Drum

- BLM - Rangeland Drill

- BLM - Rangeland Seeding, Colorado Plateau Ecoregion

- BLM - Rangeland Seeding, Great Basin Ecoregion

- BLM-Reclamation Suitability (MT)

- CLASS RULE - Depth to lithic bedrock (5 classes) (NPS)

- CLASS RULE - Soil Inorganic Carbon kg/m2 to 2m (NPS)

- CLASS RULE - Soil Organic Carbon kg/m2 to 2m (NPS)

- CLR-pastureland limitation (IN)

- Commodity Crop Productivity Index (Soybeans) (TN)

- CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)

- CPI - Barley, IRR - Eastern Idaho Plateaus (ID)

- CPI - Grass Hay, IRR - Klamath Valleys and Basins (OR)

- CPI - Small Grains, IRR - Snake River Plains (ID)

- CPI - Wheat, IRR - Eastern Idaho Plateaus (ID)

- CZSS - Salinization due to Coastal Saltwater Inundation (CT)

- DHS - Catastrophic Event, Large Animal Mortality, Burial

- DHS - Catastrophic Mortality, Large Animal Disposal, Pit

- DHS - Catastrophic Mortality, Large Animal Disposal, Trench

- DHS - Potential for Radioactive Bioaccumulation

- DHS - Potential for Radioactive Sequestration

- DHS - Suitability for Composting Medium and Final Cover

- ENG - Construction Materials; Gravel Source

- ENG - Construction Materials; Gravel Source (AK)

- ENG - Construction Materials; Gravel Source (ID)

- ENG - Construction Materials; Gravel Source (OH)

- ENG - Construction Materials; Gravel Source (VT)

- ENG - Construction Materials; Gravel Source (WA)

- ENG - Construction Materials; Roadfill (OH)

- ENG - Construction Materials; Sand Source (OR)

- ENG - Construction Materials; Sand Source (WA)

- ENG - Construction Materials; Topsoil (GA)

- ENG - Construction Materials; Topsoil (MD)

- ENG - Daily Cover for Landfill

- ENG - Daily Cover for Landfill (AK)

- ENG - Disposal Field Suitability Class (NJ)

- ENG - Dwellings W/O Basements (OH)

- ENG - Dwellings with Basements (AK)

- ENG - Large Animal Disposal, Pit (CT)

- ENG - Lawn, landscape, golf fairway (CT)

- ENG - Lined Retention Systems

- ENG - Local Roads and Streets (OH)

- ENG - On-Site Waste Water Absorption Fields (MO)

- ENG - Septic Tank Absorption Fields

- ENG - Septic Tank Absorption Fields (MD)

- ENG - Septic Tank Absorption Fields (TX)

- ENG - Septic Tank, Gravity Disposal (TX)

- ENG - Sewage Lagoons

- ENG - Small Commercial Buildings (OH)

- ENG - Soil Potential Ratings of SSDS (CT)

- FOR (USFS) - Road Construction/Maintenance (Natural Surface)

- FOR - Compaction Potential (WA)

- FOR - Conservation Tree/Shrub Groups (MT)

- FOR - Damage by Fire (OH)

- FOR - General Harvest Season (VT)

- FOR - Hand Planting Suitability

- FOR - Hand Planting Suitability, MO13 (DE)

- FOR - Hand Planting Suitability, MO13 (MD)

- FOR - Log Landing Suitability

- FOR - Log Landing Suitability (ME)

- FOR - Log Landing Suitability (VT)

- FOR - Log Landing Suitability (WA)

- FOR - Mechanical Planting Suitability (CT)

- FOR - Mechanical Planting Suitability, MO13 (MD)

- FOR - Mechanical Site Preparation (Deep)

- FOR - Mechanical Site Preparation (Deep) (DE)

- FOR - Mechanical Site Preparation (Surface) (DE)

- FOR - Mechanical Site Preparation (Surface) (MI)

- FOR - Mechanical Site Preparation; Surface (ME)

- FOR - Potential Erosion Hazard, Road/Trail, Spring Thaw (AK)

- FOR - Potential Seedling Mortality (PIA)

- FOR - Potential Seedling Mortality(ME)

- FOR - Puddling Hazard

- FOR - Road Suitability (Natural Surface) (ME)

- FOR - Road Suitability (Natural Surface) (WA)

- FOR - Soil Rutting Hazard (OH)

- FOR - Soil Sustainability Forest Biomass Harvesting (CT)

- FOR - White Oak Suitability (MO)

- FOR-Biomass Harvest (WI)

- FOTG - Indiana Corn Yield Calculation (IN)

- GRL - Excavations to 24 inches for Plastic Pipelines (TX)

- GRL - Fencing, 24 inch Post Depth (MT)

- GRL - NV range seeding (Wind C = 100) (NV)

- GRL - NV range seeding (Wind C = 40) (NV)

- GRL - NV range seeding (Wind C = 60) (NV)

- GRL - NV range seeding (Wind C = 80) (NV)

- GRL - NV range seeding (Wind C \>= 160) (NV)

- GRL - Rangeland Planting by Mechanical Seeding (TX)

- GRL - Rangeland Root Plowing (TX)

- Hybrid Wine Grape Varieties Site Desirability (Long)

- Low Pressure Pipe Septic System (DE)

- MIL - Bivouac Areas (DOD)

- MIL - Excavations Crew-Served Weapon Fighting Position (DOD)

- MIL - Excavations for Individual Fighting Position (DOD)

- MIL - Trafficability Veh. Type 1 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 2 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 4 1-pass wet season (DOD)

- MIL - Trafficability Veh. Type 4 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 6 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 7 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 7 dry season (DOD)

- NCCPI - Irrigated National Commodity Crop Productivity Index

- Nitrogen Loss Potential (ND)

- Potential Windthrow Hazard (TN)

- REC - Foot and ATV Trails (AK)

- REC - Playgrounds (AK)

- Reclamation Suitability (ND)

- RSK-risk assessment for manure application (OH)

- SAS - CMECS Substrate Origin

- SAS - CMECS Substrate Subclass/Group/Subgroup

- SAS - Mooring Anchor - Deadweight

- Septic System A/B Soil System (Alternate) (PA)

- Septic System CO-OP RFS III w/Spray Irrigation (PA)

- Septic System Dual Field Trench (conventional) (WV)

- Septic System Elevated Field (alternative) (WV)

- Septic System In Ground Trench (conventional) (PA)

- Septic System In Ground Trench (conventional) (WV)

- AGR - Filter Strips (TX)

- AGR - Hops Site Suitability (ID)

- AGR - Mulch Till (TX)

- AGR - Nitrate Leaching Potential, Nonirrigated (MT)

- AGR - Nitrate Leaching Potential, Nonirrigated (WV)

- AGR - No Till (VT)

- AGR - Oats Yield (MT)

- AGR - Pesticide Loss Potential-Leaching

- AGR - Pesticide Loss Potential-Leaching (NE)

- AGR - Rutting Hazard =\< 10,000 Pounds per Wheel (TX)

- AGR - S. Highbush Blueberry Suitability MLRA 153 (SC)

- AGR - Wind Erosion Potential (NE)

- AGR-Available Water Capacity (ND)

- AGR-Physical Limitations (ND)

- AGR-Sodicity (ND)

- AGR-Surface Crusting (ND)

- AGR-Wind Erosion (ND)

- AWM - Irrigation Disposal of Wastewater (DE)

- AWM - Land App of Municipal Sewage Sludge (DE)

- AWM - Land App of Municipal Sewage Sludge (MD)

- AWM - Land Application of Milk (CT)

- AWM - Land Application of Municipal Biosolids, spring (OR)

- AWM - Land Application of Municipal Sewage Sludge

- AWM - Land Application of Municipal Sewage Sludge (OH)

- AWM - Land Application of Municipal Sewage Sludge (VT)

- AWM - Large Animal Disposal, Pit (MN)

- AWM - Manure and Food Processing Waste

- AWM - Manure and Food Processing Waste (VT)

- AWM - Rapid Infil Disposal of Wastewater (MD)

- AWM - Rapid Infiltration Disposal of Wastewater (VT)

- AWM - Slow Rate Process Treatment of Wastewater (VT)

- BLM - Chaining Suitability

- BLM - Fugitive Dust Resistance

- BLM - Soil Restoration Potential

- BLM - Yellow Star-thistle Invasion Susceptibility

- CLASS RULE - Depth to non-lithic bedrock (5 classes) (NPS)

- CLR-cropland limitation for corn and soybeans (IN)

- Commodity Crop Productivity Index (Corn) (WI)

- CPI - Grass Hay, NIRR - Klamath Valleys and Basins (OR)

- CPI - Potatoes Productivity Index (AK)

- CPI - Potatoes, IRR - Eastern Idaho Plateaus (ID)

- CPI - Small Grains, NIRR - Palouse Prairies (ID)

- DHS - Emergency Animal Mortality Disposal by Shallow Burial

- DHS - Rubble and Debris Disposal, Large-Scale Event

- ENG - Aquifer Assessment - 7081 (MN)

- ENG - Construction Materials - Gravel Source (MN)

- ENG - Construction Materials; Gravel Source (MI)

- ENG - Construction Materials; Gravel Source (OR)

- ENG - Construction Materials; Reclamation

- ENG - Construction Materials; Reclamation (OH)

- ENG - Construction Materials; Sand Source

- ENG - Construction Materials; Sand Source (AK)

- ENG - Construction Materials; Sand Source (ID)

- ENG - Construction Materials; Sand Source (IN)

- ENG - Construction Materials; Sand Source (OH)

- ENG - Construction Materials; Topsoil

- ENG - Construction Materials; Topsoil (WA)

- ENG - Ground-based Solar Arrays, Soil-based Anchor Systems

- ENG - Local Roads and Streets

- ENG - New Ohio Septic Rating (OH)

- ENG - Sanitary Landfill (Trench) (OH)

- ENG - Septic Tank Absorption Fields (AK)

- ENG - Septic Tank Absorption Fields (DE)

- ENG - Septic Tank Absorption Fields (NY)

- ENG - Sewage Lagoons (OH)

- ENG - Shallow Excavations (AK)

- ENG - Shallow Excavations (MI)

- ENG - Unpaved Local Roads and Streets

- FOR - Black Walnut Suitability Index (MO)

- FOR - Conservation Tree and Shrub Groups (TX)

- FOR - Construction Limitations - Haul Roads/Log Landing (OH)

- FOR - Construction Limitations For Haul Roads (MI)

- FOR - Hand Planting Suitability (ME)

- FOR - Harvest Equipment Operability (MD)

- FOR - Harvest Equipment Operability (OH)

- FOR - Harvest Equipment Operability (VT)

- FOR - Mechanical Planting Suitability

- FOR - Mechanical Planting Suitability (ME)

- FOR - Mechanical Planting Suitability, MO13 (DE)

- FOR - Potential Erosion Hazard (Off-Road/Off-Trail)

- FOR - Potential Erosion Hazard (Road/Trail) (PIA)

- FOR - Potential Seedling Mortality (VT)

- FOR - Potential Windthrow Hazard (NY)

- FOR - Potential Windthrow Hazard (VT)

- FOR - Puddling Potential (WA)

- FOR - Road Suitability (Natural Surface)

- FOR - Road Suitability (Natural Surface) (OH)

- FOR - Road Suitability (Natural Surface) (OR)

- FOR - Rutting Hazard by Season

- FOR - Shortleaf pine littleleaf disease susceptibility

- FOR - Soil Compactibility Risk

- FOR - Soil Rutting Hazard (ME)

- FOR - Windthrow Hazard

- FOR-Construction Limitations for Haul Roads/Log Landings(ME)

- FOTG - Indiana Slippage Potential (IN)

- Gravity Full Depth Septic System (DE)

- GRL - Fencing, Post Depth =\<36 inches

- GRL - NV range seeding (Wind C = 50) (NV)

- GRL - Ranch Access Roads (TX)

- GRL - Rangeland Roller Chopping (TX)

- Ground Penetrating Radar Penetration

- Ground-based Solar Arrays_bedrock(ME)

- Ground-based Solar Arrays_bedrock_slope_ballast(ME)

- Hybrid Wine Grape Varieties Site Desirability (Short)

- ISDH Septic Tank Interpretation (IN)

- Land Application of Municipal Sewage Sludge (PA)

- MIL - Helicopter Landing Zones (DOD)

- MIL - Trafficability Veh. Type 2 1-pass wet season (DOD)

- MIL - Trafficability Veh. Type 5 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 5 dry season (DOD)

- MIL - Trafficability Veh. Type 7 1-pass wet season (DOD)

- NCCPI - National Commodity Crop Productivity Index (Ver 3.0)

- REC - Camp and Picnic Areas (AK)

- REC - Picnic Areas (CT)

- REC - Playgrounds (CT)

- SAS - CMECS Substrate Subclass

- Septic System Drip Irrigation (Alternate) (PA)

- Septic System Free Access Sand Filter w/Drip Irrigation (PA)

- Septic System In Ground Bed (conventional) (PA)

- Septic System Peat Based Option1 (UV & At-Grade Bed)Alt (PA)

- Septic System Peat Sys Opt3 w/Subsurface Sand Filter (PA)

- Septic System Sand Mound Bed or Trench (PA)

- Septic System Shallow Placement Pressure Dosed (Alt.) (PA)

- SOH - Aggregate Stability (ND)

- SOH - Agricultural Organic Soil Subsidence

- SOH - Dynamic Soil Properties Response to Biochar

- SOH - Organic Matter Depletion

- SOIL HEALTH ASSESSMENT (NJ)

- URB - Commercial Brick Bldg; w/Reinforced Concrete Slab (TX)

- URB - Reinforced Concrete Slab (TX)

- URB/REC - Camp Areas

- URB/REC - Camp Areas (OH)

- URB/REC - Off-Road Motorcycle Trails (OH)

- URB/REC - Paths and Trails (OH)

- URB/REC - Picnic Areas

- URB/REC - Playgrounds

- URB/REC - Playgrounds (GA)

- Vinifera Wine Grape Site Desirability (Short to Medium)

- WLF - Irr. Domestic Grasses & Legumes for Food & Cover (TX)

- WLF - Upland Coniferous Trees (TX)

- WLF - Upland Deciduous Trees (TX)

- WLF - Upland Desertic Shrubs & Trees (TX)

- WLF - Upland Native Herbaceous Plants (TX)

- WLF - Upland Shrubs & Vines (TX)

- WLF-Soil Suitability - Karner Blue Butterfly (WI)

- WMS - Drainage (IL)

- WMS - Drainage - (MI)

- WMS - Embankments, Dikes, and Levees

- WMS - Embankments, Dikes, and Levees (OH)

- WMS - Grassed Waterways - (MI)

- AGR - Air Quality; PM10 (TX)

- AGR - Air Quality; PM2_5 (TX)

- AGR - Aronia Berry Suitability (SD)

- AGR - Farmland of Statewide Importance (TX)

- AGR - Index for alfalfa hay, irrigated (NV)

- AGR - Nitrate Leaching Potential, Nonirrigated (MA)

- AGR - Rangeland Grass/Herbaceous Productivity Index (TX)

- AGR - Rutting Hazard \> 10,000 Pounds per Wheel (TX)

- AGR - Water Erosion Potential (TX)

- AGR - Wine Grape Site Suitability (WA)

- AGR-Natural Fertility (ND)

- AGR-Subsurface Salinity (ND)

- AWM - Filter Group (OH)

- AWM - Irrigation Disposal of Wastewater

- AWM - Land Application of Dry and Slurry Manure (TX)

- AWM - Land Application of Municipal Biosolids, winter (OR)

- AWM - Overland Flow Process Treatment of Wastewater

- AWM - Rapid Infiltration Disposal of Wastewater

- AWM - Vegetated Treatment Area (PIA)

- AWM - Waste Field Storage Area (VT)

- BLM - Mechanical Treatment, Shredder

- BLM - Medusahead Invasion Susceptibility

- BLM - Soil Compaction Resistance

- Capping Fill Gravity Septic System (DE)

- CLASS RULE - Depth to any bedrock kind (5 classes) (NPS)

- CPI - Alfalfa Hay, IRR - Eastern Idaho Plateaus (ID)

- CPI - Alfalfa Hay, IRR - Klamath Valley and Basins (OR)

- CPI - Alfalfa Hay, IRR - Snake River Plains (ID)

- CPI - Alfalfa Hay, NIRR- Eastern Idaho Plateaus (ID)

- CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)

- CPI - Small Grains Productivity Index (AK)

- DHS - Catastrophic Event, Large Animal Mortality, Incinerate

- DHS - Emergency Land Disposal of Milk

- DHS - Site for Composting Facility - Subsurface

- DHS - Suitability for Clay Liner Material

- ENG - Cohesive Soil Liner (MN)

- ENG - Construction Materials - Sand Source (MN)

- ENG - Construction Materials; Gravel Source (CT)

- ENG - Construction Materials; Gravel Source (NY)

- ENG - Construction Materials; Reclamation (DE)

- ENG - Construction Materials; Roadfill

- ENG - Construction Materials; Roadfill (AK)

- ENG - Construction Materials; Sand Source (NY)

- ENG - Construction Materials; Sand Source (VT)

- ENG - Construction Materials; Topsoil (AK)

- ENG - Construction Materials; Topsoil (DE)

- ENG - Construction Materials; Topsoil (MI)

- ENG - Construction Materials; Topsoil (OR)

- ENG - Conventional On-Site Septic Systems (TN)

- ENG - Deep Infiltration Systems

- ENG - Disposal Field Gravity (DE)

- ENG - Dwellings With Basements (OH)

- ENG - Ground-based Solar Arrays, Ballast Anchor Systems

- ENG - Large Animal Disposal, Trench (CT)

- ENG - Lawn, Landscape, Golf Fairway (MI)

- ENG - Lawn, Landscape, Golf Fairway (VT)

- ENG - Sanitary Landfill (Area) (OH)

- ENG - Sanitary Landfill (Trench)

- ENG - Sanitary Landfill (Trench) (AK)

- ENG - Septage Application - Surface (MN)

- ENG - Septic Tank Absorption Fields - At-Grade (MN)

- ENG - Septic Tank Absorption Fields - Mound (MN)

- ENG - Septic Tank Leaching Chamber (TX)

- ENG - Septic Tank, Subsurface Drip Irrigation (TX)

- ENG - Shallow Excavations

- ENG - Shallow Infiltration Systems

- ENG - Small Commercial Buildings

- ENG - Soil Potential of Road Salt Applications (CT)

- ENG - Source of Caliche (TX)

- ENG - Stormwater Management / Ponds (NY)

- ENG - Unlined Retention Systems

- Farm and Garden Composting Facility - Surface

- FOR - Biomass Harvest (MA)

- FOR - Black Walnut Suitability Index (KS)

- FOR - Displacement Potential (WA)

- FOR - Drought Vulnerable Soils

- FOR - General Harvest Season (ME)

- FOR - Harvest Equipment Operability

- FOR - Mechanical Site Preparation (Deep) (MD)

- FOR - Mechanical Site Preparation (Surface)

- FOR - Mechanical Site Preparation; Deep (CT)

- FOR - Potential Erosion Hazard (Road/Trail)

- FOR - Potential Fire Damage Hazard

- FOR - Potential Seedling Mortality

- FOR - Potential Seedling Mortality (MI)

- FOR - Potential Windthrow Hazard (ME)

- FOR - Potential Windthrow Hazard (MI)

- FOR - Road Suitability (Natural Surface) (ID)

- FOR - Rutting Hazard by Month

- FOR - Windthrow Hazard (WA)

- FOTG - NLI Interp Calculation - (IN)

- Fragile Soil Index

- GRL - Juniper Encroachment Potential (NM)

- GRL - NV range seeding (Wind C = 20) (NV)

- GRL - Pasture and Hayland SG (OH)

- GRL - Rangeland Prescribed Burning (TX)

- GRL - Rangeland Soil Seed Bank Suitability (NM)

- GRL-FSG-NP-W (MT)

- GRL-SHSI Soil Health Sustainability Index (MT)

- Ground-based Solar Arrays_saturationt(ME)

- Ground-based Solar Arrays_slope(ME)

- Inland Wetlands (CT)

- IRR-restrictive features for irrigation (OH)

- MIL - Excavations for Vehicle Fighting Position (DOD)

- MIL - Trafficability Veh. Type 1 1-pass wet season (DOD)

- MIL - Trafficability Veh. Type 2 dry season (DOD)

- MIL - Trafficability Veh. Type 3 50-passes wet season (DOD)

- MIL - Trafficability Veh. Type 6 1-pass wet season (DOD)

- MIL - Trafficability Veh. Type 6 dry season (DOD)

- Muscadine Wine Grape Site Desirability (Very Long)

- NCCPI - NCCPI Cotton Submodel (II)

- Permafrost Sensitivity (AK)

- Pressure Dose Capping Fill Septic System (DE)

- REC - Camp Areas (CT)

- REC - Off-Road Motorcycle Trails (CT)

- SAS - CMECS Substrate Class

- SAS - CMECS Substrate Subclass/Group

- SAS - Eelgrass Restoration Suitability

- SAS - Land Utilization of Dredged Materials

- SAS - Northern Quahog (Hard Clam) Habitat Suitability

- Septic System At Grade Shallow Field (alternative) (WV)

- Septic System At-Grade Bed (Alternate) (PA)

- Septic System CO-OP RFS III w/Drip Irrigation (PA)

- Septic System Drip Irrigation (alternative) (WV)

- Septic System Free Access Sand Filterw/Spray Irrigation (PA)

- Septic System Peat Based Option1 w/At-Grade Bed (Alt.) (PA)

- Septic System Spray Irrigation (PA)

- Septic System Steep Slope Sand Mound (Alternate) (PA)

- Shallow Infiltration Systems

- SOH - Organic Matter Depletion Potential, Irrigated (CA)

- SOH - Soil Surface Sealing

- TROP - Plantains Productivity

- URB/REC - Camp Areas (GA)

- URB/REC - Camp Areas (MI)

- URB/REC - Golf Fairways (OH)

- URB/REC - Off-Road Motorcycle Trails

- URB/REC - Paths and Trails (MI)

- URB/REC - Playgrounds (OH)

- Vinifera Wine Grape Site Desirability (Long to Medium)

- WLF - Chufa for Turkey Forage (LA)

- WLF - Food Plots for Upland Wildlife \< 2 Acres (TX)

- WLF - Freshwater Wetland Plants (TX)

- WLF - Irrigated Saline Water Wetland Plants (TX)

- WLF - Riparian Herbaceous Plants (TX)

- WLF - Riparian Shrubs, Vines, & Trees (TX)

- WLF - Saline Water Wetland Plants (TX)

- WLF - Upland Mixed Deciduous & Coniferous Trees (TX)

- WMS - Constructing Grassed Waterways (TX)

- WMS - Constructing Terraces and Diversions (OH)

- WMS - Embankments, Dikes, and Levees (VT)

- WMS - Irrigation, Sprinkler (close spaced outlet drops)

- WMS - Irrigation, Sprinkler (general)

- WMS - Pond Reservoir Area (GA)

- WMS-Subsurface Water Management, Installation (ND)

- WMS-Subsurface Water Management, Outflow Quality (ND)

- AGR - Barley Yield (MT)

- AGR - Conventional Tillage (TX)

- AGR - Grape non-irrigated (MO)

- AGR - Industrial Hemp for Fiber and Seed Production

- AGR - Nitrate Leaching Potential, Irrigated (WA)

- AGR - Pasture hayland (MO)

- AGR - Pesticide Loss Potential-Soil Surface Runoff

- AGR - Prime Farmland (TX)

- AGR - Spring Wheat Yield (MT)

- AGR-Agronomic Concerns (ND)

- AGR-Pesticide and Nutrient Leaching Potential, NIRR (ND)

- AGR-Surface Salinity (ND)

- AGR-Water Erosion Potential (ND)

- Alaska Exempt Wetland Potential (AK)

- American Wine Grape Varieties Site Desirability (Short)

- AWM - Irrigation Disposal of Wastewater (MD)

- AWM - Manure and Food Processing Waste (DE)

- AWM - Manure Stacking - Site Evaluation (TX)

- AWM - Phosphorus Management (TX)

- AWM - Slow Rate Process Treatment of Wastewater

- BLM - Pygmy Rabbit Habitat Potential

- BLM - Rangeland Tillage

- BLM - Site Degradation Susceptibility

- CA Prime Farmland (CA)

- CLASS RULE - Depth to root limiting layer (5 classes) (NPS)

- Commodity Crop Productivity Index (Corn) (TN)

- CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)

- CPI - Barley, NIRR - Eastern Idaho Plateaus (ID)

- CPI - Grass Hay, IRR - Eastern Idaho Plateaus (ID)

- CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)

- CPI - Potatoes, IRR - Snake River Plains (ID)

- CPI - Small Grains, NIRR - Palouse Prairies (OR)

- CPI - Small Grains, NIRR - Palouse Prairies (WA)

- CPI - Small Grains, NIRR - Snake River Plains (ID)

- CPI - Wheat, NIRR - Eastern Idaho Plateaus (ID)

- CPI - Wild Hay, NIRR - Eastern Idaho Plateaus (ID)

- CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)

- CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)

- Deep Infiltration Systems

- DHS - Site for Composting Facility - Surface

- Elevated Sand Mound Septic System (DE)

- ENG - Animal Disposal by Composting (Catastrophic) (WV)

- ENG - Application of Municipal Sludge (TX)

- ENG - Closed-Loop Horizontal Geothermal Heat Pump (CT)

- ENG - Construction Materials; Gravel Source (IN)

- ENG - Construction Materials; Gravel Source (NE)

- ENG - Construction Materials; Reclamation (MD)

- ENG - Construction Materials; Reclamation (MI)

- ENG - Construction Materials; Roadfill (GA)

- ENG - Construction Materials; Sand Source (CT)

- ENG - Construction Materials; Sand Source (GA)

- ENG - Construction Materials; Topsoil (ID)

- ENG - Construction Materials; Topsoil (OH)

- ENG - Daily Cover for Landfill (OH)

- ENG - Disposal Field (NJ)

- ENG - Disposal Field Type Inst (NJ)

- ENG - Dwellings W/O Basements

- ENG - Dwellings With Basements

- ENG - Dwellings without Basements (AK)

- ENG - Lawn and Landscape (OH)

- ENG - Lawn, Landscape, Golf Fairway

- ENG - Local Roads and Streets (AK)

- ENG - Local Roads and Streets (GA)

- ENG - On-Site Waste Water Lagoons (MO)

- ENG - Pier Beam Building Foundations (TX)

- ENG - Sanitary Landfill (Area)

- ENG - Sanitary Landfill (Area) (AK)

- ENG - Septage Application - Incorporation or Injection (MN)

- ENG - Septic System; Disinfection, Surface Application (TX)

- ENG - Septic Tank Absorption Fields (FL)

- ENG - Septic Tank Absorption Fields (OH)

- ENG - Septic Tank Absorption Fields - Trench (MN)

- ENG - Sewage Lagoons (AK)

- ENG - Shallow Excavations (OH)

- ENG - Soil Suitability for SLAMM Marsh Migration (CT)

- ENG - Stormwater Management / Infiltration (NY)

- ENG - Stormwater Management / Wetlands (NY)

- FOR - Black Walnut Suitability (WI)

- FOR - Black Walnut Suitability (WV)

- FOR - Construction Limitations for Haul Roads/Log Landings

- FOR - Displacement Hazard

- FOR - Harvest Equipment Operability (DE)

- FOR - Harvest Equipment Operability (ME)

- FOR - Harvest Equipment Operability (MI)

- FOR - Log Landing Suitability (ID)

- FOR - Log Landing Suitability (MI)

- FOR - Log Landing Suitability (OR)

- FOR - Mechanical Planting Suitability (OH)

- FOR - Mechanical Site Preparation (Surface) (MD)

- FOR - Mechanical Site Preparation (Surface) (OH)

- FOR - Mechanical Site Preparation; Surface (CT)

- FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (MI)

- FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (OH)

- FOR - Potential Seedling Mortality (FL)

- FOR - Potential Seedling Mortality (OH)

- FOR - Road Suitability (Natural Surface) (VT)

- FOR - Soil Rutting Hazard

- FOTG - Indiana Soy Bean Yield Calculation (IN)

- FOTG - Indiana Wheat Yield Calculation (IN)

- FOTG - NLI report Calculation - (IN)

- GRL - Fencing, Post Depth =\<24 inches

- GRL - Fencing, Post Depth Less Than 24 inches (TX)

- GRL - Fencing, Post Depth Less Than 36 inches (TX)

- GRL - NV range seeding (Wind C = 10) (NV)

- GRL - NV range seeding (Wind C = 30) (NV)

- GRL - Rangeland Chaining (TX)

- GRL - Rangeland Disking (TX)

- GRL - Rangeland Dozing/Grubbing (TX)

- GRL - Utah Juniper Encroachment Potential

- GRL - Western Juniper Encroachment Potential (OR)

- Ground-based Solar Arrays_bedrock_slope_anchor(ME)

- Ground-based Solar Arrays_saturation_flooding_Frost(ME)

- Hybrid Wine Grape Varieties Site Desirability (Medium)

- Lined Retention Systems

- MIL - Trafficability Veh. Type 1 dry season (DOD)

- MIL - Trafficability Veh. Type 3 1-pass wet season (DOD)

- MIL - Trafficability Veh. Type 3 dry season (DOD)

- MIL - Trafficability Veh. Type 4 dry season (DOD)

- MIL - Trafficability Veh. Type 5 1-pass wet season (DOD)

- NCCPI - NCCPI Corn Submodel (I)

- NCCPI - NCCPI Small Grains Submodel (II)

- NCCPI - NCCPI Soybeans Submodel (I)

- Peony Flowers Site Suitability (AK)

- Pressure Dose Full Depth Septic System (DE)

- REC - Camp Areas; Primitive (AK)

- REC - Paths and Trails (CT)

- Salinity Risk Index (ND)

- SAS - Eastern Oyster Habitat Restoration Suitability

- SAS - Mooring Anchor - Mushroom

- Septic System CO-OP RFS III w/At-Grade Bed (PA)

- Septic System Free Access Sand Filter w/At-Grade Bed (PA)

- Septic System Modified Subsurface Sand Filter (Alt.) (PA)

- Septic System Shallow In Ground Trench (conventional) (WV)

- Septic System Subsurface Sand Filter Bed (conventional) (PA)

- Septic System Subsurface Sand Filter Trench (standard) (PA)

- SOH - Limitations for Aerobic Soil Organisms

- URB - Concrete Driveways and Sidewalks (TX)

- URB - Dwellings on Concrete Slab (TX)

- URB - Lawns and Ornamental Plantings (TX)

- URB/REC - Paths and Trails

- URB/REC - Paths and Trails (GA)

- URB/REC - Playgrounds (MI)

- Vinifera Wine Grape Site Desirability (Long)

- WLF - Crawfish Aquaculture (TX)

- WLF - Desertic Herbaceous Plants (TX)

- WLF - Gopher Tortoise Burrowing Suitability

- WLF - Grain & Seed Crops for Food and Cover (TX)

- WMS - Constructing Grassed Waterways (OH)

- WMS - Irrigation, Surface (graded)

- WMS - Subsurface Drains - Installation (VT)

- WMS - Subsurface Water Management, System Performance

- WMS - Surface Drains (TX)

- WMS - Surface Irrigation Intake Family (TX)

- Septic System Low Pressure Pipe (alternative) (WV)

- Septic System Mound (alternative) (WV)

- Septic System Peat Based Option2 w/Spray Irrigation (PA)

- Septic System Steep Slope Mound (alternative) (WV)

- SOH - Concentration of Salts- Soil Surface

- SOH - Soil Susceptibility to Compaction

- Soil Habitat for Saprophyte Stage of Coccidioides

- Unlined Retention Systems

- URB - Commercial Metal Bldg; w/Reinforced Concrete Slab (TX)

- URB/REC - Picnic Areas (GA)

- URB/REC - Picnic Areas (MI)

- URB/REC - Picnic Areas (OH)

- Vinifera Wine Grape Site Desirability (Short)

- WLF - Burrowing Mammals & Reptiles (TX)

- WLF - Desert Tortoise (CA)

- WLF - Domestic Grasses & Legumes for Food and Cover (TX)

- WLF - Irrigated Grain & Seed Crops for Food & Cover (TX)

- WMS - Excavated Ponds (Aquifer-fed)

- WMS - Excavated Ponds (Aquifer-fed) (VT)

- WMS - Irrigation, General

- WMS - Irrigation, Micro (above ground)

- WMS - Irrigation, Micro (above ground) (VT)

- WMS - Irrigation, Micro (subsurface drip)

- WMS - Irrigation, Sprinkler (general) (VT)

- WMS - Pond Reservoir Area

- WMS - Pond Reservoir Area (OH)

- WMS - Subsurface Water Management, System Installation

- WMS - Constructing Terraces & Diversions (TX)

- WMS - Drainage (OH)

- WMS - Excavated Ponds (Aquifer-fed) (OH)

- WMS - Grape Production with Drip Irrigation (TX)

- WMS - Irrigation, Micro (subsurface drip) (VT)

- WMS - Irrigation, Surface (level)

- WMS - Pond Reservoir Area (MI)

- WMS - Pond Reservoir Area (VT)

- WMS - Sprinkler Irrigation (MT)

- WMS - Sprinkler Irrigation RDC (IL)

- WMS - Subsurface Drains - Performance (VT)

- WMS - Subsurface Water Management, Outflow Quality

- WMS - Surface Water Management, System

- WMS-Subsurface Water Management, Performance (ND)

## Author

Jason Nemecek, Chad Ferguson, Andrew Brown

## Examples

``` r
# \donttest{
  # get two forestry interpretations for CA630
  get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
                           "FOR - Road Suitability (Natural Surface)"),
                         method = "Dominant Condition",
                         areasymbols = "CA630")
#> single result set, returning a data.frame
#>       mukey areasymbol musym
#> 1   1865918      CA630  3046
#> 2   1865926      CA630  7088
#> 3   1865927      CA630  7155
#> 4   1865928      CA630  7156
#> 5   1865929      CA630  8033
#> 6   1865930      CA630  8034
#> 7   1865931      CA630  8036
#> 8   1900697      CA630  6038
#> 9   1906347      CA630  7159
#> 10  1913590      CA630  6029
#> 11  1913591      CA630  6034
#> 12  1913592      CA630  6037
#> 13  1913600      CA630  7207
#> 14  1913601      CA630  8173
#> 15  1913602      CA630  8160
#> 16  1913605      CA630  8177
#> 17  1913606      CA630  8178
#> 18  1913607      CA630  3020
#> 19  2220266      CA630  3038
#> 20  2220269      CA630  4048
#> 21  2220270      CA630  4046
#> 22  2220271      CA630  6043
#> 23  2220273      CA630  6041
#> 24  2220301      CA630  8161
#> 25  2374651      CA630  4136
#> 26  2383083      CA630  8171
#> 27  2383084      CA630  8172
#> 28  2399766      CA630  6070
#> 29  2399769      CA630  6074
#> 30  2399770      CA630  6075
#> 31  2399771      CA630  6076
#> 32  2399780      CA630  8110
#> 33  2399783      CA630  8115
#> 34  2403696      CA630  5100
#> 35  2403709      CA630  1012
#> 36  2403710      CA630  8314
#> 37  2403711      CA630  8312
#> 38  2403719      CA630  9013
#> 39  2403720      CA630  9012
#> 40  2403721      CA630  9011
#> 41  2403722      CA630  9010
#> 42  2403747      CA630  6202
#> 43  2424959      CA630  8317
#> 44  2424960      CA630  8318
#> 45  2424961      CA630  8319
#> 46  2424962      CA630  8175
#> 47  2424963      CA630  8176
#> 48  2424975      CA630  9014
#> 49  2425107      CA630  3021
#> 50  2426355      CA630  8026
#> 51  2426483      CA630  7096
#> 52  2436790      CA630  8307
#> 53  2436792      CA630  8302
#> 54  2440240      CA630  7086
#> 55  2440242      CA630  7087
#> 56  2441253      CA630  6071
#> 57  2441798      CA630  7085
#> 58  2450478      CA630  5101
#> 59  2450843      CA630  7089
#> 60  2450844      CA630  7083
#> 61  2450845      CA630  3058
#> 62  2452459      CA630  8162
#> 63  2455490      CA630  8190
#> 64  2455492      CA630  8286
#> 65  2455494      CA630  8289
#> 66  2455495      CA630  8287
#> 67  2462630      CA630     W
#> 68  2480973      CA630  3033
#> 69  2482710      CA630  6039
#> 70  2483494      CA630  8194
#> 71  2600456      CA630  7210
#> 72  2600457      CA630  7211
#> 73  2600458      CA630  7212
#> 74  2600460      CA630  4040
#> 75  2600465      CA630  5051
#> 76  2600467      CA630  5053
#> 77  2600469      CA630  5057
#> 78  2600480      CA630  9015
#> 79  2600481      CA630  9016
#> 80  2600527      CA630  4200
#> 81  2600528      CA630  4201
#> 82  2600529      CA630  5013
#> 83  2600534      CA630  7165
#> 84  2600537      CA630  9017
#> 85  2600538      CA630  4202
#> 86  2766830      CA630  7166
#> 87  2766836      CA630  5201
#> 88  2766837      CA630  5202
#> 89  2766838      CA630  5012
#> 90  2766850      CA630  5015
#> 91  2924701      CA630  6078
#> 92  2924738      CA630  6072
#> 93  2924739      CA630  6079
#> 94  2924751      CA630  7074
#> 95  2924752      CA630  7076
#> 96  2924753      CA630  7078
#> 97  2924754      CA630  7079
#> 98  2924831      CA630  6036
#> 99  2924832      CA630  6033
#> 100 2924833      CA630  1090
#> 101 2924834      CA630  1091
#> 102 2924835      CA630  9018
#> 103 2924879      CA630   206
#> 104 2924880      CA630   207
#> 105 2924881      CA630   208
#> 106 2924882      CA630   209
#> 107 2924883      CA630   212
#> 108 2924884      CA630   401
#> 109 2924885      CA630   451
#> 110 2924886      CA630   475
#> 111 2924887      CA630  5016
#> 112 2924890      CA630   851
#> 113 2924907      CA630   128
#> 114 2924908      CA630   301
#> 115 2924909      CA630  8120
#> 116 2924912      CA630   DAM
#> 117 2924913      CA630   220
#> 118 2924914      CA630  1013
#> 119 2924955      CA630  8111
#> 120 3225132      CA630  7065
#> 121 3225133      CA630  7066
#> 122 3225134      CA630  7091
#> 123 3225135      CA630  7092
#> 124 3356286      CA630  6054
#> 125 3356287      CA630  6055
#> 126 3356288      CA630  6205
#> 127 3356289      CA630  6206
#> 128 3356290      CA630  6207
#> 129 3431597      CA630  5105
#>                                                                                                                                                 muname
#> 1                                                                                          Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes
#> 2                                                                                              Loafercreek-Gopheridge complex, 30 to 60 percent slopes
#> 3                                                                                                   Crimeahouse-Sixbit complex, 3 to 15 percent slopes
#> 4                                                                                                  Crimeahouse-Sixbit complex, 15 to 30 percent slopes
#> 5                                                                                         Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky
#> 6                                                                                        Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky
#> 7                                                                                       Copperopolis-Whiterock complex, 15 to 30 percent slopes, rocky
#> 8                                                                                  Musick-Wukusick complex, 30 to 60 percent slopes, low precipitation
#> 9                                                                                                  Crimeahouse-Sixbit complex, 30 to 70 percent slopes
#> 10                                                                                    Holland-Chawanakee-Rock outcrop complex, 45 to 90 percent slopes
#> 11                                                                                                     Musick-Wukusick complex, 3 to 15 percent slopes
#> 12                                                                                                    Musick-Wukusick complex, 15 to 30 percent slopes
#> 13                                                                                      Millvilla-Copperopolis-Hetchy complex, 30 to 60 percent slopes
#> 14                                                                                       Nedsgulch-Wallyhill-Arpatutu complex, 30 to 60 percent slopes
#> 15                                                                                                     Nedsgulch-Sites complex, 3 to 15 percent slopes
#> 16                                                                              Moccasinhill-Copperopolis-Sanguinetti complex, 30 to 60 percent slopes
#> 17                                                                                          Moccasinhill-Copperopolis complex, 60 to 90 percent slopes
#> 18                                                                                          Iron Mountain-Rock outcrop complex, 3 to 15 percent slopes
#> 19                                                                              Devilsnose-Lilygap complex, 30 to 60 percent slopes, low precipitation
#> 20                                                                                                 Devilsnose-Lilygap complex, 30 to 60 percent slopes
#> 21                                                                                                   Redapple-Lilygap complex, 15 to 30 percent slopes
#> 22                                                                                                    Mantree ashy sandy loam, 15 to 30 percent slopes
#> 23                                                                                                     Mantree ashy sandy loam, 3 to 15 percent slopes
#> 24                                                                                                    Nedsgulch-Sites complex, 15 to 30 percent slopes
#> 25                                                    Ultic Haploxeralfs, shallow-Ultic Haploxeralfs, moderately deep complex, 10 to 35 percent slopes
#> 26                                                                                                 Nedsgulch-Wallyhill complex, 3 to 15 percent slopes
#> 27                                                                                       Nedsgulch-Wallyhill-Arpatutu complex, 15 to 30 percent slopes
#> 28                                                                                   Sierra-Verjeles-Aquic Haploxeralfs complex, 0 to 8 percent slopes
#> 29                                                                                                        Sierra-Orose complex, 8 to 30 percent slopes
#> 30                                                                                                      Sierra-Flanly complex, 30 to 60 percent slopes
#> 31                                                                                      Auberry-Hurleton-Rock outcrop complex, 20 to 60 percent slopes
#> 32                                                                                        Cumulic Humixerepts-Riverwash complex, 0 to 8 percent slopes
#> 33                                                                                           Sanguinetti-Copperopolis complex, 30 to 60 percent slopes
#> 34                                                     Mokelumne-Buenavista-Aquultic Haploxeralfs, occasionally ponded complex, 1 to 12 percent slopes
#> 35                                                                                                                                          Mined land
#> 36                                                                        Rock outcrop-Tiger Creek-Vertic Haploxerepts complex, 1 to 45 percent slopes
#> 37                                                                                              Aquariusmine-Millvilla complex, 3 to 30 percent slopes
#> 38                                                                                                Urban land-Millvilla complex, 1 to 25 percent slopes
#> 39                                                                                             Urban land-Copperopolis complex, 0 to 15 percent slopes
#> 40                                                                                            Urban land-Sierra-Flanly complex, 3 to 25 percent slopes
#> 41                                                                                                                                          Urban land
#> 42                                                                  Musick-Ultic Haploxeralfs, moderately well drained, complex, 1 to 8 percent slopes
#> 43                                                                                                 Beybek-Rock outcrop complex, 3 to 30 percent slopes
#> 44                                                                                   Aquariusmine-Hetchy-Rock outcrop complex, 30 to 60 percent slopes
#> 45                                                                Mollic Haploxeralfs-Pachic Argixerolls-Rock Outcrop complex, 50 to 90 percent slopes
#> 46                                                                                                 Copperopolis-Hetchy complex, 8 to 30 percent slopes
#> 47                                                                            Sanguinetti-Moccasinhill-Deerflat complex, 60 to 90 percent slopes, cool
#> 48                                                                                             Urban land-Musick-Hotaw complex, 3 to 30 percent slopes
#> 49                                                                                 Iron Mountain-Crozier-Rock outcrop complex, 15 to 60 percent slopes
#> 50                                                                                          Moccasinhill-Copperopolis complex, 30 to 60 percent slopes
#> 51                                                                                              Gopheridge-Jasperpeak complex, 50 to 90 percent slopes
#> 52                                                                                              Tiger Creek-Nedsgulch complex, 15 to 50 percent slopes
#> 53                                                                                               Tiger Creek-Nedsgulch complex, 3 to 15 percent slopes
#> 54                                                                                             Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 55                                                                                       Loafercreek-Gopheridge complex, cool, 15 to 30 percent slopes
#> 56                                                                                                       Sierra-Flanly complex, 3 to 15 percent slopes
#> 57                                                                                                 Bonanza-Loafercreek complex, 3 to 15 percent slopes
#> 58                                                                                                  Hornitos-Red Bluff complex, 2 to 25 percent slopes
#> 59                                                                                  Gardellones-Gopheridge-Motherlode complex, 30 to 60 percent slopes
#> 60                                     Aquic Haploxeralfs, rarely flooded and occasionally ponded-Loafercreek-Dunstone complex, 1 to 12 percent slopes
#> 61                                                                                              Shawsflat-Angelscreek complex, 25 to 60 percent slopes
#> 62                                                                                                 Nedsgulch-Arpatutu complex, 30 to 60 percent slopes
#> 63                                                                                                Lickinfork-Arpatutu complex, 40 to 90 percent slopes
#> 64                                                                                                    Jocal gravelly silt loam, 8 to 30 percent slopes
#> 65                                                                                            Fiddletown-Rock outcrop complex, 40 to 90 percent slopes
#> 66                                                                                                   Jocal-Fiddletown complex, 30 to 60 percent slopes
#> 67                                                                                                                                               Water
#> 68                                                                                Redapple-Lilygap complex, 15 to 30 percent slopes, low precipitation
#> 69                                                                                           Holland-Wukusick-Mantree complex, 30 to 60 percent slopes
#> 70                                                                                         Wallyhill, deep-Lickinfork complex, 40 to 90 percent slopes
#> 71                                                                                                  Deerflat-Millvilla complex, 3 to 15 percent slopes
#> 72                                                                                                Millvilla-Luckymine complex, 15 to 30 percent slopes
#> 73                                                                                               Wardsferry-Millvilla complex, 30 to 60 percent slopes
#> 74                                                                                   Iron Mountain-Redapple-Devilsnose complex, 3 to 15 percent slopes
#> 75                                                                                           Fuches-Lithic Xerorthents complex, 3 to 15 percent slopes
#> 76                                                                                          Fuches-Lithic Xerorthents complex, 15 to 50 percent slopes
#> 77                                                                                                                  Supan loam, 5 to 30 percent slopes
#> 78                                                                                     Urban land-Loafercreek-Dunstone complex, 3 to 15 percent slopes
#> 79                                                                                     Urban land-Nedsgulch-Wallyhill complex, 15 to 30 percent slopes
#> 80                                                                                                    Inks-Angelscreek complex, 3 to 15 percent slopes
#> 81                                                                                                  Angelscreek-Pentz complex, 15 to 30 percent slopes
#> 82                                                                                                 Miltonhills-Amador complex, 15 to 45 percent slopes
#> 83                                                                                                  Sixbit-Crimeahouse complex, 5 to 20 percent slopes
#> 84                                                                                                   Urban land-Amador complex, 2 to 15 percent slopes
#> 85                                                                                                  Angelscreek-Pentz complex, 30 to 60 percent slopes
#> 86                                                                                                Sixbit-Rock outcrop complex, 20 to 45 percent slopes
#> 87                                                                                                       Pardee-Amador complex, 1 to 15 percent slopes
#> 88                                                                                                      Pardee-Amador complex, 15 to 40 percent slopes
#> 89                                                                                                           Amador sandy loam, 2 to 15 percent slopes
#> 90                                                                                                   Ospital-Jennylind complex, 2 to 15 percent slopes
#> 91                                                                                                      Sierra-Flanly complex, 15 to 65 percent slopes
#> 92                                                                                                      Flanly-Verjeles complex, 0 to 8 percent slopes
#> 93                                                                                                                 Flanly loam, 8 to 30 percent slopes
#> 94                                                                                                 Loafercreek-Bonanza complex, 3 to 15 percent slopes
#> 95                                                                                     Bonanza-Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 96                                                                                              Jasperpeak-Gopheridge complex, 30 to 60 percent slopes
#> 97                                                                                             Gopheridge-Loafercreek complex, 30 to 60 percent slopes
#> 98                                                                                 Musick-Wukusick complex, 15 to 30 percent slopes, low precipitation
#> 99                                                                                  Musick-Wukusick complex, 3 to 15 percent slopes, low precipitation
#> 100                                                                             Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
#> 101                                                                              Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
#> 102                                                                                  Urban land-Copperopolis-Whiterock complex, 8 to 30 percent slopes
#> 103                                                                                                           Pentz sandy loam, 2 to 15 percent slopes
#> 104                                                                                                          Pentz sandy loam, 15 to 50 percent slopes
#> 105                                                                                                     Pentz cobbly sandy loam, 2 to 8 percent slopes
#> 106                                                                                                      Pentz-Bellota complex, 2 to 15 percent slopes
#> 107                                                                                                                 Peters clay, 2 to 8 percent slopes
#> 108                                                                                                    Peters-Pentz association, 2 to 8 percent slopes
#> 109                                                                                                   Pentz-Peters association, 2 to 15 percent slopes
#> 110                                                                                                   Pentz-Peters association, 2 to 50 percent slopes
#> 111                                                                                            Jennylind-Rock outcrop complex, 10 to 45 percent slopes
#> 112                                                                                                           Mckeonhills clay, 5 to 15 percent slopes
#> 113                                                                                                        Cogna loam, 0 to 2 percent slopes, overwash
#> 114                                                                                           Archerdale-Hicksville association, 0 to 2 percent slopes
#> 115                                                                        Fluventic Haploxerepts-Oxyaquic Xerofluvents complex, 0 to 8 percent slopes
#> 116                                                                                                                                               Dams
#> 117                                                                                                  Redding gravelly loam, 0 to 8 percent slopes, dry
#> 118                                                                                  Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
#> 119 Psammentic Haploxerolls, rarely flooded-Mollic Fluvaquents, occasionally flooded-Riverwash, very frequently flooded complex, 0 to 8 percent slopes
#> 120                                                                             Bonanza-Loafercreek complex, 3 to 15 percent slopes, low precipitation
#> 121                                                                                    Bonanza-Loafercreek-Jasperpeak complex, 15 to 30 percent slopes
#> 122                                                                                    Trabuco-Jasperpeak-Rock outcrop complex, 8 to 30 percent slopes
#> 123                                                                                Gopheridge-Jasperpeak-Rock outcrop complex, 30 to 60 percent slopes
#> 124                                                                                          Shaver-Holland-Chawanakee complex, 8 to 30 percent slopes
#> 125                                                                                 Shaver-Lithic Humixerepts-Holland complex, 30 to 60 percent slopes
#> 126                                                                                                      Musick fine sandy loam, 3 to 8 percent slopes
#> 127                                                                                                       Musick-Hotaw complex, 8 to 30 percent slopes
#> 128                                                                                           Musick-Hotaw-Chawanakee complex, 30 to 60 percent slopes
#> 129                                                                                                Firebrick-Mokelumne complex, 2 to 30 percent slopes
#>     rating_FORPotentialSeedlingMortality
#> 1                                   1.00
#> 2                                   0.61
#> 3                                   0.80
#> 4                                   0.95
#> 5                                   0.94
#> 6                                   0.80
#> 7                                   0.78
#> 8                                   0.19
#> 9                                   0.79
#> 10                                  0.35
#> 11                                  0.19
#> 12                                  0.05
#> 13                                  0.52
#> 14                                  0.24
#> 15                                  0.00
#> 16                                  0.33
#> 17                                  0.46
#> 18                                  0.33
#> 19                                  0.18
#> 20                                  0.02
#> 21                                  0.01
#> 22                                  0.03
#> 23                                  0.01
#> 24                                  0.07
#> 25                                  0.82
#> 26                                  0.17
#> 27                                  0.21
#> 28                                  0.15
#> 29                                  0.28
#> 30                                  0.20
#> 31                                  0.36
#> 32                                  0.25
#> 33                                  0.27
#> 34                                  0.70
#> 35                                    NA
#> 36                                    NA
#> 37                                  0.12
#> 38                                    NA
#> 39                                    NA
#> 40                                    NA
#> 41                                    NA
#> 42                                  0.00
#> 43                                  0.22
#> 44                                  0.12
#> 45                                  0.44
#> 46                                  0.25
#> 47                                  0.25
#> 48                                    NA
#> 49                                  0.25
#> 50                                  0.72
#> 51                                  0.83
#> 52                                  0.20
#> 53                                  0.06
#> 54                                  0.39
#> 55                                  0.49
#> 56                                  0.12
#> 57                                  0.82
#> 58                                  0.78
#> 59                                  0.38
#> 60                                  1.00
#> 61                                  0.69
#> 62                                  0.14
#> 63                                  0.29
#> 64                                  0.01
#> 65                                  0.08
#> 66                                  0.21
#> 67                                    NA
#> 68                                  0.13
#> 69                                  0.03
#> 70                                  0.10
#> 71                                  0.12
#> 72                                  0.25
#> 73                                  0.27
#> 74                                  0.00
#> 75                                  0.37
#> 76                                  0.34
#> 77                                  0.00
#> 78                                    NA
#> 79                                    NA
#> 80                                  0.55
#> 81                                  0.55
#> 82                                  0.78
#> 83                                  0.95
#> 84                                    NA
#> 85                                  0.52
#> 86                                  0.81
#> 87                                  0.84
#> 88                                  0.75
#> 89                                  0.82
#> 90                                  1.00
#> 91                                  0.27
#> 92                                  0.48
#> 93                                  0.51
#> 94                                  0.50
#> 95                                  0.64
#> 96                                  1.00
#> 97                                  0.77
#> 98                                  0.05
#> 99                                  0.19
#> 100                                 0.29
#> 101                                 0.51
#> 102                                   NA
#> 103                                 0.82
#> 104                                 0.82
#> 105                                 0.87
#> 106                                 0.76
#> 107                                 0.87
#> 108                                 0.74
#> 109                                 0.78
#> 110                                 0.79
#> 111                                 0.20
#> 112                                 0.79
#> 113                                 0.68
#> 114                                 0.69
#> 115                                 0.26
#> 116                                   NA
#> 117                                 0.86
#> 118                                   NA
#> 119                                 0.94
#> 120                                 0.72
#> 121                                 0.71
#> 122                                 0.64
#> 123                                 0.66
#> 124                                 0.13
#> 125                                 0.23
#> 126                                 0.22
#> 127                                 0.12
#> 128                                 0.21
#> 129                                 0.77
#>     total_comppct_FORPotentialSeedlingMortality
#> 1                                            47
#> 2                                            94
#> 3                                            97
#> 4                                            94
#> 5                                            83
#> 6                                            90
#> 7                                            94
#> 8                                            97
#> 9                                            95
#> 10                                           75
#> 11                                           53
#> 12                                           97
#> 13                                          100
#> 14                                           97
#> 15                                           90
#> 16                                           95
#> 17                                           90
#> 18                                           85
#> 19                                           97
#> 20                                           98
#> 21                                           99
#> 22                                           90
#> 23                                           90
#> 24                                          100
#> 25                                          100
#> 26                                          100
#> 27                                           99
#> 28                                           96
#> 29                                           93
#> 30                                           95
#> 31                                           75
#> 32                                           62
#> 33                                           98
#> 34                                           85
#> 35                                          100
#> 36                                           60
#> 37                                          100
#> 38                                           50
#> 39                                           65
#> 40                                           50
#> 41                                           85
#> 42                                           60
#> 43                                           65
#> 44                                           75
#> 45                                           65
#> 46                                          100
#> 47                                           97
#> 48                                           50
#> 49                                           85
#> 50                                           95
#> 51                                           60
#> 52                                           95
#> 53                                           95
#> 54                                           98
#> 55                                           99
#> 56                                           96
#> 57                                           96
#> 58                                           97
#> 59                                           98
#> 60                                           54
#> 61                                           90
#> 62                                           97
#> 63                                           95
#> 64                                           95
#> 65                                           75
#> 66                                           95
#> 67                                          100
#> 68                                           99
#> 69                                           96
#> 70                                           95
#> 71                                          100
#> 72                                           99
#> 73                                          100
#> 74                                           55
#> 75                                           95
#> 76                                           95
#> 77                                           80
#> 78                                           50
#> 79                                           50
#> 80                                           97
#> 81                                           94
#> 82                                           95
#> 83                                           95
#> 84                                           60
#> 85                                           90
#> 86                                           88
#> 87                                           96
#> 88                                           95
#> 89                                           88
#> 90                                           60
#> 91                                          100
#> 92                                           99
#> 93                                           95
#> 94                                           93
#> 95                                           99
#> 96                                           49
#> 97                                           98
#> 98                                           97
#> 99                                           53
#> 100                                          97
#> 101                                         100
#> 102                                          65
#> 103                                          98
#> 104                                          98
#> 105                                          98
#> 106                                         100
#> 107                                          97
#> 108                                         100
#> 109                                          99
#> 110                                          99
#> 111                                          70
#> 112                                         100
#> 113                                         100
#> 114                                         100
#> 115                                          86
#> 116                                         100
#> 117                                          85
#> 118                                          70
#> 119                                          47
#> 120                                          94
#> 121                                          92
#> 122                                          86
#> 123                                          84
#> 124                                          96
#> 125                                          95
#> 126                                          91
#> 127                                          94
#> 128                                          95
#> 129                                          90
#>     class_FORPotentialSeedlingMortality
#> 1                                  High
#> 2                              Moderate
#> 3                              Moderate
#> 4                              Moderate
#> 5                              Moderate
#> 6                              Moderate
#> 7                              Moderate
#> 8                              Moderate
#> 9                              Moderate
#> 10                             Moderate
#> 11                             Moderate
#> 12                             Moderate
#> 13                             Moderate
#> 14                             Moderate
#> 15                                  Low
#> 16                             Moderate
#> 17                             Moderate
#> 18                             Moderate
#> 19                             Moderate
#> 20                             Moderate
#> 21                             Moderate
#> 22                             Moderate
#> 23                             Moderate
#> 24                             Moderate
#> 25                             Moderate
#> 26                             Moderate
#> 27                             Moderate
#> 28                             Moderate
#> 29                             Moderate
#> 30                             Moderate
#> 31                             Moderate
#> 32                             Moderate
#> 33                             Moderate
#> 34                             Moderate
#> 35                            Not rated
#> 36                             Moderate
#> 37                             Moderate
#> 38                            Not rated
#> 39                            Not rated
#> 40                            Not rated
#> 41                            Not rated
#> 42                             Moderate
#> 43                             Moderate
#> 44                             Moderate
#> 45                             Moderate
#> 46                             Moderate
#> 47                             Moderate
#> 48                            Not rated
#> 49                             Moderate
#> 50                             Moderate
#> 51                             Moderate
#> 52                             Moderate
#> 53                             Moderate
#> 54                             Moderate
#> 55                             Moderate
#> 56                             Moderate
#> 57                             Moderate
#> 58                             Moderate
#> 59                             Moderate
#> 60                             Moderate
#> 61                             Moderate
#> 62                             Moderate
#> 63                             Moderate
#> 64                             Moderate
#> 65                             Moderate
#> 66                             Moderate
#> 67                            Not rated
#> 68                             Moderate
#> 69                             Moderate
#> 70                             Moderate
#> 71                             Moderate
#> 72                             Moderate
#> 73                             Moderate
#> 74                                  Low
#> 75                             Moderate
#> 76                             Moderate
#> 77                                  Low
#> 78                            Not rated
#> 79                            Not rated
#> 80                             Moderate
#> 81                             Moderate
#> 82                             Moderate
#> 83                             Moderate
#> 84                            Not rated
#> 85                             Moderate
#> 86                             Moderate
#> 87                             Moderate
#> 88                             Moderate
#> 89                             Moderate
#> 90                                 High
#> 91                             Moderate
#> 92                             Moderate
#> 93                             Moderate
#> 94                             Moderate
#> 95                             Moderate
#> 96                                 High
#> 97                             Moderate
#> 98                             Moderate
#> 99                             Moderate
#> 100                            Moderate
#> 101                            Moderate
#> 102                           Not rated
#> 103                            Moderate
#> 104                            Moderate
#> 105                            Moderate
#> 106                            Moderate
#> 107                            Moderate
#> 108                            Moderate
#> 109                            Moderate
#> 110                            Moderate
#> 111                            Moderate
#> 112                            Moderate
#> 113                            Moderate
#> 114                            Moderate
#> 115                            Moderate
#> 116                           Not rated
#> 117                            Moderate
#> 118                           Not rated
#> 119                            Moderate
#> 120                            Moderate
#> 121                            Moderate
#> 122                            Moderate
#> 123                            Moderate
#> 124                            Moderate
#> 125                            Moderate
#> 126                            Moderate
#> 127                            Moderate
#> 128                            Moderate
#> 129                            Moderate
#>                                                                                                                                                                         reason_FORPotentialSeedlingMortality
#> 1                                                                                                 FOR - Wetness Limitation (8) "Wetness" (1); FOR - Available Water Limitation "Moisture supply low" (0.971)
#> 2                                                                                                                                  FOR - Available Water Limitation "Moderately low moisture supply" (0.468)
#> 3                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.869)
#> 4                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.914)
#> 5                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.906)
#> 6                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.906)
#> 7                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.886)
#> 8                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.324)
#> 9                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.833)
#> 10                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.131)
#> 11                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.013)
#> 12                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.104)
#> 13                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.258)
#> 14                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.176)
#> 15                                                                                                                                                                                                      <NA>
#> 16                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.387)
#> 17                                                                                                                                   FOR - Available Water Limitation "Moderately low moisture supply" (0.6)
#> 18                                                                                                                                   FOR - Available Water Limitation "Moderately low moisture supply" (0.5)
#> 19                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.044)
#> 20                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.001)
#> 21                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.01)
#> 22                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.007)
#> 23                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.007)
#> 24                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.008)
#> 25                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.687)
#> 26                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.198)
#> 27                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.091)
#> 28                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.073)
#> 29                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.11)
#> 30                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.098)
#> 31                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.253)
#> 32                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.101)
#> 33                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.352)
#> 34                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.645)
#> 35  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 36  Null Horizon Data "Not rated" (); FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" ()
#> 37                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.078)
#> 38  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 39  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 40  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 41  Null Horizon Data "Not rated" (); FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" ()
#> 42                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.001)
#> 43                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.344)
#> 44                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.035)
#> 45                                                                           FOR - Soil Reaction Limitation "Soil reaction" (0.3); FOR - Available Water Limitation "Moderately low moisture supply" (0.431)
#> 46                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.365)
#> 47                                                                                                                                  FOR - Available Water Limitation "Moderately low moisture supply" (0.46)
#> 48  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 49                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.529)
#> 50                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.879)
#> 51                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.988)
#> 52                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.023)
#> 53                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.071)
#> 54                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.352)
#> 55                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.466)
#> 56                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.13)
#> 57                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.927)
#> 58                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.824)
#> 59                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.514)
#> 60                                                                                     FOR - Wetness Limitation (8) "Wetness" (1); FOR - Available Water Limitation "Moderately low moisture supply" (0.458)
#> 61                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.745)
#> 62                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.055)
#> 63                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.31)
#> 64                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.002)
#> 65                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.107)
#> 66                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.059)
#> 67  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 68                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.006)
#> 69                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.001)
#> 70                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.001)
#> 71                                                                                                                                 FOR - Available Water Limitation "Moderately high moisture supply" (0.05)
#> 72                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.099)
#> 73                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.232)
#> 74                                                                                                                                   FOR - Available Water Limitation "Moderately low moisture supply" (0.5)
#> 75                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.489)
#> 76                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.506)
#> 77                                                                                                                                                                                                      <NA>
#> 78  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 79  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 80                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.559)
#> 81                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.532)
#> 82                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.685)
#> 83                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.985)
#> 84  FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 85                                                                                                                                   FOR - Available Water Limitation "Moderately low moisture supply" (0.5)
#> 86                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.815)
#> 87                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.886)
#> 88                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.978)
#> 89                                                                                                                                            FOR - Available Water Limitation "Moisture supply low" (0.925)
#> 90                                                                                                FOR - Wetness Limitation (8) "Wetness" (1); FOR - Available Water Limitation "Moisture supply low" (0.983)
#> 91                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.244)
#> 92                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.502)
#> 93                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.503)
#> 94                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.503)
#> 95                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.628)
#> 96                                                                                                                                           FOR - Available Water Limitation "Moisture supply limiting" (1)
#> 97                                                                                                                                             FOR - Available Water Limitation "Moisture supply low" (0.97)
#> 98                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.104)
#> 99                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.013)
#> 100                                                                                                                                FOR - Available Water Limitation "Moderately low moisture supply" (0.395)
#> 101                                                                                                                                FOR - Available Water Limitation "Moderately low moisture supply" (0.422)
#> 102 FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 103                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.903)
#> 104                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.935)
#> 105                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.977)
#> 106                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.929)
#> 107                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.845)
#> 108                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.893)
#> 109                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.963)
#> 110                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.986)
#> 111                                                                                                                               FOR - Available Water Limitation "Moderately high moisture supply" (0.253)
#> 112                                                                          FOR - Available Water Limitation "Moderately low moisture supply" (0.635); FOR - Soil Reaction Limitation "Soil reaction" (0.6)
#> 113                                                                                                                                 FOR - Available Water Limitation "Moderately low moisture supply" (0.66)
#> 114                                                                                                                                FOR - Available Water Limitation "Moderately low moisture supply" (0.554)
#> 115                                                                                                                               FOR - Available Water Limitation "Moderately high moisture supply" (0.139)
#> 116 FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 117                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.858)
#> 118 FOR - Soil Reaction Limitation "Not rated" (); FOR - Available Water Limitation "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 119                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.947)
#> 120                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.914)
#> 121                                                                                                                                FOR - Available Water Limitation "Moderately low moisture supply" (0.607)
#> 122                                                                                                                                           FOR - Available Water Limitation "Moisture supply low" (0.752)
#> 123                                                                                                                                FOR - Available Water Limitation "Moderately low moisture supply" (0.563)
#> 124                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.08)
#> 125                                                                                                                               FOR - Available Water Limitation "Moderately high moisture supply" (0.096)
#> 126                                                                                                                               FOR - Available Water Limitation "Moderately high moisture supply" (0.057)
#> 127                                                                                                                               FOR - Available Water Limitation "Moderately high moisture supply" (0.083)
#> 128                                                                                                                                FOR - Available Water Limitation "Moderately high moisture supply" (0.12)
#> 129                                                                                   FOR - Available Water Limitation "Moisture supply low" (0.894); FOR - Soil Reaction Limitation "Soil reaction" (0.333)
#>     rating_FORRoadSuitabilityNaturalSurface
#> 1                                      1.00
#> 2                                      1.00
#> 3                                      0.74
#> 4                                      1.00
#> 5                                      0.30
#> 6                                      0.94
#> 7                                      1.00
#> 8                                      1.00
#> 9                                      1.00
#> 10                                     1.00
#> 11                                     0.00
#> 12                                     1.00
#> 13                                     1.00
#> 14                                     1.00
#> 15                                     0.85
#> 16                                     1.00
#> 17                                     1.00
#> 18                                     1.00
#> 19                                     1.00
#> 20                                     1.00
#> 21                                     1.00
#> 22                                     1.00
#> 23                                     0.22
#> 24                                     1.00
#> 25                                     1.00
#> 26                                     0.76
#> 27                                     1.00
#> 28                                     0.08
#> 29                                     1.00
#> 30                                     1.00
#> 31                                     1.00
#> 32                                     1.00
#> 33                                     1.00
#> 34                                     0.48
#> 35                                       NA
#> 36                                     1.00
#> 37                                     1.00
#> 38                                       NA
#> 39                                       NA
#> 40                                       NA
#> 41                                       NA
#> 42                                     0.17
#> 43                                     1.00
#> 44                                     1.00
#> 45                                     1.00
#> 46                                     0.21
#> 47                                     1.00
#> 48                                       NA
#> 49                                     1.00
#> 50                                     1.00
#> 51                                     1.00
#> 52                                     1.00
#> 53                                     1.00
#> 54                                     1.00
#> 55                                     1.00
#> 56                                     0.21
#> 57                                     0.21
#> 58                                     1.00
#> 59                                     1.00
#> 60                                     1.00
#> 61                                     1.00
#> 62                                     1.00
#> 63                                     1.00
#> 64                                     1.00
#> 65                                     1.00
#> 66                                     1.00
#> 67                                       NA
#> 68                                     1.00
#> 69                                     1.00
#> 70                                     1.00
#> 71                                     0.86
#> 72                                     1.00
#> 73                                     1.00
#> 74                                     0.75
#> 75                                     0.81
#> 76                                     1.00
#> 77                                     1.00
#> 78                                       NA
#> 79                                       NA
#> 80                                     0.12
#> 81                                     1.00
#> 82                                     1.00
#> 83                                     0.48
#> 84                                       NA
#> 85                                     1.00
#> 86                                     1.00
#> 87                                     0.00
#> 88                                     1.00
#> 89                                     0.03
#> 90                                     1.00
#> 91                                     1.00
#> 92                                     0.13
#> 93                                     1.00
#> 94                                     1.00
#> 95                                     1.00
#> 96                                     1.00
#> 97                                     1.00
#> 98                                     1.00
#> 99                                     0.00
#> 100                                    1.00
#> 101                                    0.00
#> 102                                      NA
#> 103                                    0.72
#> 104                                    1.00
#> 105                                    0.04
#> 106                                    0.11
#> 107                                    1.00
#> 108                                    1.00
#> 109                                    0.21
#> 110                                    1.00
#> 111                                    1.00
#> 112                                    1.00
#> 113                                    0.02
#> 114                                    0.37
#> 115                                    0.76
#> 116                                      NA
#> 117                                    0.68
#> 118                                      NA
#> 119                                    0.00
#> 120                                    0.66
#> 121                                    1.00
#> 122                                    1.00
#> 123                                    1.00
#> 124                                    1.00
#> 125                                    1.00
#> 126                                    0.00
#> 127                                    1.00
#> 128                                    1.00
#> 129                                    0.21
#>     total_comppct_FORRoadSuitabilityNaturalSurface
#> 1                                               80
#> 2                                               94
#> 3                                               97
#> 4                                               94
#> 5                                               83
#> 6                                               85
#> 7                                               94
#> 8                                               97
#> 9                                               95
#> 10                                              75
#> 11                                              50
#> 12                                              97
#> 13                                             100
#> 14                                              97
#> 15                                              90
#> 16                                              95
#> 17                                              90
#> 18                                              85
#> 19                                              97
#> 20                                              98
#> 21                                              99
#> 22                                             100
#> 23                                             100
#> 24                                             100
#> 25                                             100
#> 26                                              75
#> 27                                              99
#> 28                                              91
#> 29                                              96
#> 30                                              95
#> 31                                              75
#> 32                                              67
#> 33                                              98
#> 34                                              45
#> 35                                             100
#> 36                                              60
#> 37                                              68
#> 38                                              50
#> 39                                              65
#> 40                                              50
#> 41                                              85
#> 42                                              90
#> 43                                              65
#> 44                                              75
#> 45                                              65
#> 46                                              50
#> 47                                              97
#> 48                                              50
#> 49                                              85
#> 50                                              95
#> 51                                              95
#> 52                                              95
#> 53                                              55
#> 54                                              98
#> 55                                              99
#> 56                                              96
#> 57                                              54
#> 58                                              72
#> 59                                              98
#> 60                                              45
#> 61                                              90
#> 62                                              97
#> 63                                              95
#> 64                                              95
#> 65                                              75
#> 66                                              95
#> 67                                             100
#> 68                                              99
#> 69                                              96
#> 70                                              95
#> 71                                              95
#> 72                                              99
#> 73                                             100
#> 74                                              95
#> 75                                              95
#> 76                                              95
#> 77                                              85
#> 78                                              50
#> 79                                              50
#> 80                                              87
#> 81                                              94
#> 82                                              95
#> 83                                              65
#> 84                                              60
#> 85                                              90
#> 86                                              88
#> 87                                              71
#> 88                                              95
#> 89                                              86
#> 90                                              60
#> 91                                             100
#> 92                                              99
#> 93                                              95
#> 94                                              58
#> 95                                              99
#> 96                                              94
#> 97                                              98
#> 98                                              97
#> 99                                              50
#> 100                                             92
#> 101                                             80
#> 102                                             65
#> 103                                             87
#> 104                                             87
#> 105                                             86
#> 106                                             86
#> 107                                             88
#> 108                                             62
#> 109                                             63
#> 110                                             92
#> 111                                             80
#> 112                                             98
#> 113                                             93
#> 114                                             65
#> 115                                             66
#> 116                                            100
#> 117                                             85
#> 118                                             70
#> 119                                             47
#> 120                                             88
#> 121                                             94
#> 122                                             86
#> 123                                             84
#> 124                                             98
#> 125                                             95
#> 126                                             96
#> 127                                             96
#> 128                                             95
#> 129                                             50
#>     class_FORRoadSuitabilityNaturalSurface
#> 1                            Poorly suited
#> 2                            Poorly suited
#> 3                        Moderately suited
#> 4                            Poorly suited
#> 5                        Moderately suited
#> 6                        Moderately suited
#> 7                            Poorly suited
#> 8                            Poorly suited
#> 9                            Poorly suited
#> 10                           Poorly suited
#> 11                             Well suited
#> 12                           Poorly suited
#> 13                           Poorly suited
#> 14                           Poorly suited
#> 15                       Moderately suited
#> 16                           Poorly suited
#> 17                           Poorly suited
#> 18                           Poorly suited
#> 19                           Poorly suited
#> 20                           Poorly suited
#> 21                           Poorly suited
#> 22                           Poorly suited
#> 23                             Well suited
#> 24                           Poorly suited
#> 25                           Poorly suited
#> 26                       Moderately suited
#> 27                           Poorly suited
#> 28                             Well suited
#> 29                           Poorly suited
#> 30                           Poorly suited
#> 31                           Poorly suited
#> 32                           Poorly suited
#> 33                           Poorly suited
#> 34                             Well suited
#> 35                               Not rated
#> 36                           Poorly suited
#> 37                           Poorly suited
#> 38                               Not rated
#> 39                               Not rated
#> 40                               Not rated
#> 41                               Not rated
#> 42                             Well suited
#> 43                           Poorly suited
#> 44                           Poorly suited
#> 45                           Poorly suited
#> 46                           Poorly suited
#> 47                           Poorly suited
#> 48                               Not rated
#> 49                           Poorly suited
#> 50                           Poorly suited
#> 51                           Poorly suited
#> 52                           Poorly suited
#> 53                           Poorly suited
#> 54                           Poorly suited
#> 55                           Poorly suited
#> 56                             Well suited
#> 57                             Well suited
#> 58                           Poorly suited
#> 59                           Poorly suited
#> 60                           Poorly suited
#> 61                           Poorly suited
#> 62                           Poorly suited
#> 63                           Poorly suited
#> 64                           Poorly suited
#> 65                           Poorly suited
#> 66                           Poorly suited
#> 67                               Not rated
#> 68                           Poorly suited
#> 69                           Poorly suited
#> 70                           Poorly suited
#> 71                       Moderately suited
#> 72                           Poorly suited
#> 73                           Poorly suited
#> 74                       Moderately suited
#> 75                       Moderately suited
#> 76                           Poorly suited
#> 77                           Poorly suited
#> 78                               Not rated
#> 79                               Not rated
#> 80                             Well suited
#> 81                           Poorly suited
#> 82                           Poorly suited
#> 83                       Moderately suited
#> 84                               Not rated
#> 85                           Poorly suited
#> 86                           Poorly suited
#> 87                             Well suited
#> 88                           Poorly suited
#> 89                             Well suited
#> 90                           Poorly suited
#> 91                           Poorly suited
#> 92                             Well suited
#> 93                           Poorly suited
#> 94                           Poorly suited
#> 95                           Poorly suited
#> 96                           Poorly suited
#> 97                           Poorly suited
#> 98                           Poorly suited
#> 99                             Well suited
#> 100                          Poorly suited
#> 101                            Well suited
#> 102                              Not rated
#> 103                      Moderately suited
#> 104                          Poorly suited
#> 105                            Well suited
#> 106                            Well suited
#> 107                          Poorly suited
#> 108                          Poorly suited
#> 109                            Well suited
#> 110                          Poorly suited
#> 111                          Poorly suited
#> 112                          Poorly suited
#> 113                            Well suited
#> 114                      Moderately suited
#> 115                      Moderately suited
#> 116                              Not rated
#> 117                      Moderately suited
#> 118                              Not rated
#> 119                            Well suited
#> 120                      Moderately suited
#> 121                          Poorly suited
#> 122                          Poorly suited
#> 123                          Poorly suited
#> 124                          Poorly suited
#> 125                          Poorly suited
#> 126                            Well suited
#> 127                          Poorly suited
#> 128                          Poorly suited
#> 129                            Well suited
#>                                                                                                                                                                                                                          reason_FORRoadSuitabilityNaturalSurface
#> 1                                                                                                                         FOR - Rock Fragments Limitation (1) "Rock fragments" (1); Ponding Limitation "Ponding" (1); FOR - Wetness Limitation (1) "Wetness" (1)
#> 2                                                                                                                                                         FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.008)
#> 3                                                                                                                                                                                                           FOR - Slope Limitation (<6% to >12%) "Slope" (0.999)
#> 4                                                                                                                                                                                                               FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 5                                                                                                                                                                                                     FOR - Rock Fragments Limitation (1) "Rock fragments" (0.3)
#> 6                                                                                                                                               FOR - Slope Limitation (<6% to >12%) "Slope" (0.935); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.3)
#> 7                                                                                                                                                 FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.367)
#> 8                                                                                                                                                                                                               FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 9                                                                                                                                                                                                               FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 10                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 11                                                                                                                                                                                                                                                          <NA>
#> 12                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 13                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.143)
#> 14                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 15                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (0.935); FOR - Strength Limitation (2) "Low strength" (0.055)
#> 16                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 17                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 18                                                                                                                                                FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Slope Limitation (<6% to >12%) "Slope" (0.484)
#> 19                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 20                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 21                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.229); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193)
#> 22                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 23                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
#> 24                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.169)
#> 25                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.034); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.014)
#> 26                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (0.763); FOR - Strength Limitation (2) "Low strength" (0.047)
#> 27                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.012)
#> 28                                                                                                                                                                                                          FOR - Strength Limitation (2) "Low strength" (0.031)
#> 29                                                                                                                                                         FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.02)
#> 30                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.068)
#> 31                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 32                                                                                                                                                                      Flooding Limitation "Flooding" (1); FOR - Strength Limitation (2) "Low strength" (0.018)
#> 33                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 34                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.484)
#> 35            FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 36    FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 37                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.078)
#> 38        FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.054)
#> 39        FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.484)
#> 40        Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.215); FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" ()
#> 41        FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
#> 42                                                                                                                                            FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193); FOR - Strength Limitation (2) "Low strength" (0.109)
#> 43                 FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.275); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.218)
#> 44                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Strength Limitation (2) "Low strength" (0.262)
#> 45                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Strength Limitation (2) "Low strength" (0.039)
#> 46                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
#> 47                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 48                                                              FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 49                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 50                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 51                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 52                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.105)
#> 53                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Strength Limitation (2) "Low strength" (0.157)
#> 54                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.123)
#> 55                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.055)
#> 56                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
#> 57                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (0.215); FOR - Strength Limitation (2) "Low strength" (0.174)
#> 58                                                                                                                                                         FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.85)
#> 59                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.039)
#> 60                                                                                                                            Ponding Limitation "Ponding" (1); FOR - Wetness Limitation (1) "Wetness" (1); FOR - Strength Limitation (2) "Low strength" (0.133)
#> 61                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Strength Limitation (2) "Low strength" (0.078)
#> 62                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.096)
#> 63                                                                                                                                                        FOR - Strength Limitation (2) "Low strength" (0.016); FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 64                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 65                                                                                                       FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Sand Limitation (1) "Sandiness" (0.5)
#> 66                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.018)
#> 67                                                              FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 68                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.229); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193)
#> 69                                                                                                                                                FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193)
#> 70                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 71                                                                                                                                                     FOR - Slope Limitation (<6% to >12%) "Slope" (0.999); FOR - Strength Limitation (2) "Low strength" (0.08)
#> 72                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 73                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.202)
#> 74                                                                                                                                                           FOR - Slope Limitation (<6% to >12%) "Slope" (0.999); FOR - Sand Limitation (1) "Sandiness" (0.014)
#> 75                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.999)
#> 76                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 77                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.209)
#> 78        FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.484)
#> 79        FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (0.935)
#> 80                                                                                                                                                                                                          FOR - Strength Limitation (2) "Low strength" (0.022)
#> 81                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.033)
#> 82                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 83                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.484)
#> 84                                                              FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 85                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.078)
#> 86                                                                                                                                                    FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1)
#> 87                                                                                                                                                                                                                                                          <NA>
#> 88                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 89                                                                                                                                                                                                          FOR - Slope Limitation (<6% to >12%) "Slope" (0.054)
#> 90                                                                                                                                                                                  Ponding Limitation "Ponding" (1); FOR - Wetness Limitation (1) "Wetness" (1)
#> 91                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 92                                                                                                                                                                                                           FOR - Strength Limitation (2) "Low strength" (0.09)
#> 93                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.108)
#> 94                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 95                                                                                                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.018)
#> 96                                                                                                                                                FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193)
#> 97                                                                                                                                                FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (0.193)
#> 98                                                                                                                                                                                                              FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 99                                                                                                                                                                                                                                                          <NA>
#> 100                                                                                                                                                                                                             FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 101                                                                                                                                                                                                                                                         <NA>
#> 102           FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 103                                                                                                                                                                                                         FOR - Slope Limitation (<6% to >12%) "Slope" (0.763)
#> 104                                                                                                                                                                                                             FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 105                                                                                                                                            FOR - Rock Fragments Limitation (1) "Rock fragments" (0.08); FOR - Slope Limitation (<6% to >12%) "Slope" (0.054)
#> 106                                                                                                                                                   FOR - Slope Limitation (<6% to >12%) "Slope" (0.215); FOR - Strength Limitation (2) "Low strength" (0.055)
#> 107                                                                                                                                  FOR - Strength Limitation (2) "Low strength" (1); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.5)
#> 108                                                                                                                                 FOR - Strength Limitation (2) "Low strength" (1); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.49)
#> 109                                                                                                                                                   FOR - Slope Limitation (<6% to >12%) "Slope" (0.215); FOR - Strength Limitation (2) "Low strength" (0.065)
#> 110                                                                                                                                                       FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.065)
#> 111                                                                                                                                                   FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 112                                                                            FOR - Strength Limitation (2) "Low strength" (1); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.5); FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
#> 113                                                                                                                                                                                                         FOR - Strength Limitation (2) "Low strength" (0.039)
#> 114                                                                                                                            FOR - Strength Limitation (2) "Low strength" (0.371); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.028)
#> 115                                                                                                                                                                                                 FOR - Rock Fragments Limitation (1) "Rock fragments" (0.762)
#> 116 FOR - Slope Limitation (<6% to >12%) "Not rated; slope" (); FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" ()
#> 117                                                                                                                                                                                                               FOR - Wetness Limitation (1) "Wetness" (0.677)
#> 118           FOR - Sand Limitation (1) "Not rated" (); FOR - Strength Limitation (2) "Not rated" (); Component Kind Misc. and CHIID is null "Not rated, no horizon data" (); Null Horizon Data "Not rated" (); FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 119                                                                                                                                                                                                                                                         <NA>
#> 120                                                                                                                                                   FOR - Slope Limitation (<6% to >12%) "Slope" (0.999); FOR - Strength Limitation (2) "Low strength" (0.192)
#> 121                                                                                                                                                       FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.183)
#> 122                                                                                             FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.143)
#> 123                                                                                             FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Rock Fragments Limitation (1) "Rock fragments" (1); FOR - Strength Limitation (2) "Low strength" (0.005)
#> 124                                                                                                                                                                                                             FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 125                                                                                                                                                                                                             FOR - Slope Limitation (<6% to >12%) "Slope" (1)
#> 126                                                                                                                                                                                                                                                         <NA>
#> 127                                                                                                                                                       FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.043)
#> 128                                                                                                                                                       FOR - Slope Limitation (<6% to >12%) "Slope" (1); FOR - Strength Limitation (2) "Low strength" (0.201)
#> 129                                                                                                                                                                                                         FOR - Slope Limitation (<6% to >12%) "Slope" (0.215)
# }
```
