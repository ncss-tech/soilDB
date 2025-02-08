# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_interpretations.R: translation of SDA_Interps.py into soilDB-style R function by andrew brown
# created: 2021/04/03
# last update: 2021/05/30

#' Get map unit interpretations from Soil Data Access by rule name
#'
#' @param rulename character vector of interpretation rule names (matching `mrulename` in `cointerp` table)
#' @param method aggregation method. One of: "Dominant Component", "Dominant Condition", "Weighted Average", "None". If "None" is selected one row will be returned per component, otherwise one row will be returned per map unit.
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param WHERE character containing SQL WHERE clause specified in terms of fields in `legend`, `mapunit`, or `component` tables, used in lieu of `mukeys` or `areasymbols`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param include_minors logical. Include minor components? Default: `TRUE`.
#' @param miscellaneous_areas _logical_. Include miscellaneous areas (non-soil components) in results? Default: `TRUE`. 
#' @param not_rated_value used where rating class is "Not Rated". Default: `NA_real_`
#' @param wide_reason Default: `FALSE`; if  `TRUE` apply post-processing to all columns with prefix `"reason_"` to create additional columns for sub-rule ratings.
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE)
#' \donttest{
#'   # get two forestry interpretations for CA630
#'   get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
#'                            "FOR - Road Suitability (Natural Surface)"),
#'                          method = "Dominant Condition",
#'                          areasymbols = "CA630")
#' }
#' @details
#'
#' ## Rule Names in `cointerp` table
#'
#' - AGR - Avocado Root Rot Hazard (CA)
#' - AGR - California Revised Storie Index (CA)
#' - AGR - Hops Site Suitability (WA)
#' - AGR - Map Unit Cropland Productivity (MN)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (WA)
#' - AGR - No Till (TX)
#' - AGR - Pesticide Loss Potential-Soil Surface Runoff (NE)
#' - AGR - Ridge Till (TX)
#' - AGR - Selenium Leaching Potential (CO)
#' - AGR - Water Erosion Potential (NE)
#' - AGR - Wind Erosion Potential (TX)
#' - AGR - Winter Wheat Yield (MT)
#' - AGR-Pesticide and Nutrient Runoff Potential (ND)
#' - AGR-Rooting Depth (ND)
#' - American Wine Grape Varieties Site Desirability (Long)
#' - American Wine Grape Varieties Site Desirability (Medium)
#' - American Wine Grape Varieties Site Desirability (Very Long)
#' - AWM - Animal Mortality Disposal (Catastrophic) (MO)
#' - AWM - Irrigation Disposal of Wastewater (OH)
#' - AWM - Irrigation Disposal of Wastewater (VT)s
#' - AWM - Land Application of Municipal Biosolids, summer (OR)
#' - AWM - Manure and Food Processing Waste (MD)
#' - AWM - Manure and Food Processing Waste (OH)
#' - AWM - Overland Flow Process Treatment of Wastewater (VT)
#' - AWM - Rapid Infil Disposal of Wastewater (DE)
#' - AWM - Sensitive Soil Features (MN)
#' - AWM - Sensitive Soil Features (WI)
#' - BLM - Fencing
#' - BLM - Fire Damage Susceptibility
#' - BLM - Mechanical Treatment, Rolling Drum
#' - BLM - Rangeland Drill
#' - BLM - Rangeland Seeding, Colorado Plateau Ecoregion
#' - BLM - Rangeland Seeding, Great Basin Ecoregion
#' - BLM-Reclamation Suitability (MT)
#' - CLASS RULE - Depth to lithic bedrock (5 classes) (NPS)
#' - CLASS RULE - Soil Inorganic Carbon kg/m2 to 2m (NPS)
#' - CLASS RULE - Soil Organic Carbon kg/m2 to 2m (NPS)
#' - CLR-pastureland limitation (IN)
#' - Commodity Crop Productivity Index (Soybeans) (TN)
#' - CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - CPI - Barley, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, IRR - Klamath Valleys and Basins (OR)
#' - CPI - Small Grains, IRR - Snake River Plains (ID)
#' - CPI - Wheat, IRR - Eastern Idaho Plateaus (ID)
#' - CZSS - Salinization due to Coastal Saltwater Inundation (CT)
#' - DHS - Catastrophic Event, Large Animal Mortality, Burial
#' - DHS - Catastrophic Mortality, Large Animal Disposal, Pit
#' - DHS - Catastrophic Mortality, Large Animal Disposal, Trench
#' - DHS - Potential for Radioactive Bioaccumulation
#' - DHS - Potential for Radioactive Sequestration
#' - DHS - Suitability for Composting Medium and Final Cover
#' - ENG - Construction Materials; Gravel Source
#' - ENG - Construction Materials; Gravel Source (AK)
#' - ENG - Construction Materials; Gravel Source (ID)
#' - ENG - Construction Materials; Gravel Source (OH)
#' - ENG - Construction Materials; Gravel Source (VT)
#' - ENG - Construction Materials; Gravel Source (WA)
#' - ENG - Construction Materials; Roadfill (OH)
#' - ENG - Construction Materials; Sand Source (OR)
#' - ENG - Construction Materials; Sand Source (WA)
#' - ENG - Construction Materials; Topsoil (GA)
#' - ENG - Construction Materials; Topsoil (MD)
#' - ENG - Daily Cover for Landfill
#' - ENG - Daily Cover for Landfill (AK)
#' - ENG - Disposal Field Suitability Class (NJ)
#' - ENG - Dwellings W/O Basements (OH)
#' - ENG - Dwellings with Basements (AK)
#' - ENG - Large Animal Disposal, Pit (CT)
#' - ENG - Lawn, landscape, golf fairway (CT)
#' - ENG - Lined Retention Systems
#' - ENG - Local Roads and Streets (OH)
#' - ENG - On-Site Waste Water Absorption Fields (MO)
#' - ENG - Septic Tank Absorption Fields
#' - ENG - Septic Tank Absorption Fields (MD)
#' - ENG - Septic Tank Absorption Fields (TX)
#' - ENG - Septic Tank, Gravity Disposal (TX)
#' - ENG - Sewage Lagoons
#' - ENG - Small Commercial Buildings (OH)
#' - ENG - Soil Potential Ratings of SSDS (CT)
#' - FOR (USFS) - Road Construction/Maintenance (Natural Surface)
#' - FOR - Compaction Potential (WA)
#' - FOR - Conservation Tree/Shrub Groups (MT)
#' - FOR - Damage by Fire (OH)
#' - FOR - General Harvest Season (VT)
#' - FOR - Hand Planting Suitability
#' - FOR - Hand Planting Suitability, MO13 (DE)
#' - FOR - Hand Planting Suitability, MO13 (MD)
#' - FOR - Log Landing Suitability
#' - FOR - Log Landing Suitability (ME)
#' - FOR - Log Landing Suitability (VT)
#' - FOR - Log Landing Suitability (WA)
#' - FOR - Mechanical Planting Suitability (CT)
#' - FOR - Mechanical Planting Suitability, MO13 (MD)
#' - FOR - Mechanical Site Preparation (Deep)
#' - FOR - Mechanical Site Preparation (Deep) (DE)
#' - FOR - Mechanical Site Preparation (Surface) (DE)
#' - FOR - Mechanical Site Preparation (Surface) (MI)
#' - FOR - Mechanical Site Preparation; Surface (ME)
#' - FOR - Potential Erosion Hazard, Road/Trail, Spring Thaw (AK)
#' - FOR - Potential Seedling Mortality (PIA)
#' - FOR - Potential Seedling Mortality(ME)
#' - FOR - Puddling Hazard
#' - FOR - Road Suitability (Natural Surface) (ME)
#' - FOR - Road Suitability (Natural Surface) (WA)
#' - FOR - Soil Rutting Hazard (OH)
#' - FOR - Soil Sustainability Forest Biomass Harvesting (CT)
#' - FOR - White Oak Suitability (MO)
#' - FOR-Biomass Harvest (WI)
#' - FOTG - Indiana Corn Yield Calculation (IN)
#' - GRL - Excavations to 24 inches for Plastic Pipelines (TX)
#' - GRL - Fencing, 24 inch Post Depth (MT)
#' - GRL - NV range seeding (Wind C = 100) (NV)
#' - GRL - NV range seeding (Wind C = 40) (NV)
#' - GRL - NV range seeding (Wind C = 60) (NV)
#' - GRL - NV range seeding (Wind C = 80) (NV)
#' - GRL - NV range seeding (Wind C >= 160) (NV)
#' - GRL - Rangeland Planting by Mechanical Seeding (TX)
#' - GRL - Rangeland Root Plowing (TX)
#' - Hybrid Wine Grape Varieties Site Desirability (Long)
#' - Low Pressure Pipe Septic System (DE)
#' - MIL - Bivouac Areas (DOD)
#' - MIL - Excavations Crew-Served Weapon Fighting Position (DOD)
#' - MIL - Excavations for Individual Fighting Position (DOD)
#' - MIL - Trafficability Veh. Type 1 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 2 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 4 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 4 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 6 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 7 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 7 dry season (DOD)
#' - NCCPI - Irrigated National Commodity Crop Productivity Index
#' - Nitrogen Loss Potential (ND)
#' - Potential Windthrow Hazard (TN)
#' - REC - Foot and ATV Trails (AK)
#' - REC - Playgrounds (AK)
#' - Reclamation Suitability (ND)
#' - RSK-risk assessment for manure application (OH)
#' - SAS - CMECS Substrate Origin
#' - SAS - CMECS Substrate Subclass/Group/Subgroup
#' - SAS - Mooring Anchor - Deadweight
#' - Septic System A/B Soil System (Alternate) (PA)
#' - Septic System CO-OP RFS III w/Spray Irrigation (PA)
#' - Septic System Dual Field Trench (conventional) (WV)
#' - Septic System Elevated Field (alternative) (WV)
#' - Septic System In Ground Trench (conventional) (PA)
#' - Septic System In Ground Trench (conventional) (WV)
#' - AGR - Filter Strips (TX)
#' - AGR - Hops Site Suitability (ID)
#' - AGR - Mulch Till (TX)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (MT)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (WV)
#' - AGR - No Till (VT)
#' - AGR - Oats Yield (MT)
#' - AGR - Pesticide Loss Potential-Leaching
#' - AGR - Pesticide Loss Potential-Leaching (NE)
#' - AGR - Rutting Hazard =< 10,000 Pounds per Wheel (TX)
#' - AGR - S. Highbush Blueberry Suitability MLRA 153 (SC)
#' - AGR - Wind Erosion Potential (NE)
#' - AGR-Available Water Capacity (ND)
#' - AGR-Physical Limitations (ND)
#' - AGR-Sodicity (ND)
#' - AGR-Surface Crusting (ND)
#' - AGR-Wind Erosion (ND)
#' - AWM - Irrigation Disposal of Wastewater (DE)
#' - AWM - Land App of Municipal Sewage Sludge (DE)
#' - AWM - Land App of Municipal Sewage Sludge (MD)
#' - AWM - Land Application of Milk (CT)
#' - AWM - Land Application of Municipal Biosolids, spring (OR)
#' - AWM - Land Application of Municipal Sewage Sludge
#' - AWM - Land Application of Municipal Sewage Sludge (OH)
#' - AWM - Land Application of Municipal Sewage Sludge (VT)
#' - AWM - Large Animal Disposal, Pit (MN)
#' - AWM - Manure and Food Processing Waste
#' - AWM - Manure and Food Processing Waste (VT)
#' - AWM - Rapid Infil Disposal of Wastewater (MD)
#' - AWM - Rapid Infiltration Disposal of Wastewater (VT)
#' - AWM - Slow Rate Process Treatment of Wastewater (VT)
#' - BLM - Chaining Suitability
#' - BLM - Fugitive Dust Resistance
#' - BLM - Soil Restoration Potential
#' - BLM - Yellow Star-thistle Invasion Susceptibility
#' - CLASS RULE - Depth to non-lithic bedrock (5 classes) (NPS)
#' - CLR-cropland limitation for corn and soybeans (IN)
#' - Commodity Crop Productivity Index (Corn) (WI)
#' - CPI - Grass Hay, NIRR - Klamath Valleys and Basins (OR)
#' - CPI - Potatoes Productivity Index (AK)
#' - CPI - Potatoes, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Small Grains, NIRR - Palouse Prairies (ID)
#' - DHS - Emergency Animal Mortality Disposal by Shallow Burial
#' - DHS - Rubble and Debris Disposal, Large-Scale Event
#' - ENG - Aquifer Assessment - 7081 (MN)
#' - ENG - Construction Materials - Gravel Source (MN)
#' - ENG - Construction Materials; Gravel Source (MI)
#' - ENG - Construction Materials; Gravel Source (OR)
#' - ENG - Construction Materials; Reclamation
#' - ENG - Construction Materials; Reclamation (OH)
#' - ENG - Construction Materials; Sand Source
#' - ENG - Construction Materials; Sand Source (AK)
#' - ENG - Construction Materials; Sand Source (ID)
#' - ENG - Construction Materials; Sand Source (IN)
#' - ENG - Construction Materials; Sand Source (OH)
#' - ENG - Construction Materials; Topsoil
#' - ENG - Construction Materials; Topsoil (WA)
#' - ENG - Ground-based Solar Arrays, Soil-based Anchor Systems
#' - ENG - Local Roads and Streets
#' - ENG - New Ohio Septic Rating (OH)
#' - ENG - Sanitary Landfill (Trench) (OH)
#' - ENG - Septic Tank Absorption Fields (AK)
#' - ENG - Septic Tank Absorption Fields (DE)
#' - ENG - Septic Tank Absorption Fields (NY)
#' - ENG - Sewage Lagoons (OH)
#' - ENG - Shallow Excavations (AK)
#' - ENG - Shallow Excavations (MI)
#' - ENG - Unpaved Local Roads and Streets
#' - FOR - Black Walnut Suitability Index (MO)
#' - FOR - Conservation Tree and Shrub Groups (TX)
#' - FOR - Construction Limitations - Haul Roads/Log Landing (OH)
#' - FOR - Construction Limitations For Haul Roads (MI)
#' - FOR - Hand Planting Suitability (ME)
#' - FOR - Harvest Equipment Operability (MD)
#' - FOR - Harvest Equipment Operability (OH)
#' - FOR - Harvest Equipment Operability (VT)
#' - FOR - Mechanical Planting Suitability
#' - FOR - Mechanical Planting Suitability (ME)
#' - FOR - Mechanical Planting Suitability, MO13 (DE)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail)
#' - FOR - Potential Erosion Hazard (Road/Trail) (PIA)
#' - FOR - Potential Seedling Mortality (VT)
#' - FOR - Potential Windthrow Hazard (NY)
#' - FOR - Potential Windthrow Hazard (VT)
#' - FOR - Puddling Potential (WA)
#' - FOR - Road Suitability (Natural Surface)
#' - FOR - Road Suitability (Natural Surface) (OH)
#' - FOR - Road Suitability (Natural Surface) (OR)
#' - FOR - Rutting Hazard by Season
#' - FOR - Shortleaf pine littleleaf disease susceptibility
#' - FOR - Soil Compactibility Risk
#' - FOR - Soil Rutting Hazard (ME)
#' - FOR - Windthrow Hazard
#' - FOR-Construction Limitations for Haul Roads/Log Landings(ME)
#' - FOTG - Indiana Slippage Potential (IN)
#' - Gravity Full Depth Septic System (DE)
#' - GRL - Fencing, Post Depth =<36 inches
#' - GRL - NV range seeding (Wind C = 50) (NV)
#' - GRL - Ranch Access Roads (TX)
#' - GRL - Rangeland Roller Chopping (TX)
#' - Ground Penetrating Radar Penetration
#' - Ground-based Solar Arrays_bedrock(ME)
#' - Ground-based Solar Arrays_bedrock_slope_ballast(ME)
#' - Hybrid Wine Grape Varieties Site Desirability (Short)
#' - ISDH Septic Tank Interpretation (IN)
#' - Land Application of Municipal Sewage Sludge (PA)
#' - MIL - Helicopter Landing Zones (DOD)
#' - MIL - Trafficability Veh. Type 2 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 5 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 5 dry season (DOD)
#' - MIL - Trafficability Veh. Type 7 1-pass wet season (DOD)
#' - NCCPI - National Commodity Crop Productivity Index (Ver 3.0)
#' - REC - Camp and Picnic Areas (AK)
#' - REC - Picnic Areas (CT)
#' - REC - Playgrounds (CT)
#' - SAS - CMECS Substrate Subclass
#' - Septic System Drip Irrigation (Alternate) (PA)
#' - Septic System Free Access Sand Filter w/Drip Irrigation (PA)
#' - Septic System In Ground Bed (conventional) (PA)
#' - Septic System Peat Based Option1 (UV & At-Grade Bed)Alt (PA)
#' - Septic System Peat Sys Opt3 w/Subsurface Sand Filter (PA)
#' - Septic System Sand Mound Bed or Trench (PA)
#' - Septic System Shallow Placement Pressure Dosed (Alt.) (PA)
#' - SOH - Aggregate Stability (ND)
#' - SOH - Agricultural Organic Soil Subsidence
#' - SOH - Dynamic Soil Properties Response to Biochar
#' - SOH - Organic Matter Depletion
#' - SOIL HEALTH ASSESSMENT (NJ)
#' - URB - Commercial Brick Bldg; w/Reinforced Concrete Slab (TX)
#' - URB - Reinforced Concrete Slab (TX)
#' - URB/REC - Camp Areas
#' - URB/REC - Camp Areas (OH)
#' - URB/REC - Off-Road Motorcycle Trails (OH)
#' - URB/REC - Paths and Trails (OH)
#' - URB/REC - Picnic Areas
#' - URB/REC - Playgrounds
#' - URB/REC - Playgrounds (GA)
#' - Vinifera Wine Grape Site Desirability (Short to Medium)
#' - WLF - Irr. Domestic Grasses & Legumes for Food & Cover (TX)
#' - WLF - Upland Coniferous Trees (TX)
#' - WLF - Upland Deciduous Trees (TX)
#' - WLF - Upland Desertic Shrubs & Trees (TX)
#' - WLF - Upland Native Herbaceous Plants (TX)
#' - WLF - Upland Shrubs & Vines (TX)
#' - WLF-Soil Suitability - Karner Blue Butterfly (WI)
#' - WMS - Drainage (IL)
#' - WMS - Drainage - (MI)
#' - WMS - Embankments, Dikes, and Levees
#' - WMS - Embankments, Dikes, and Levees (OH)
#' - WMS - Grassed Waterways - (MI)
#' - AGR - Air Quality; PM10 (TX)
#' - AGR - Air Quality; PM2_5 (TX)
#' - AGR - Aronia Berry Suitability (SD)
#' - AGR - Farmland of Statewide Importance (TX)
#' - AGR - Index for alfalfa hay, irrigated (NV)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (MA)
#' - AGR - Rangeland Grass/Herbaceous Productivity Index (TX)
#' - AGR - Rutting Hazard > 10,000 Pounds per Wheel (TX)
#' - AGR - Water Erosion Potential (TX)
#' - AGR - Wine Grape Site Suitability (WA)
#' - AGR-Natural Fertility (ND)
#' - AGR-Subsurface Salinity (ND)
#' - AWM - Filter Group (OH)
#' - AWM - Irrigation Disposal of Wastewater
#' - AWM - Land Application of Dry and Slurry Manure (TX)
#' - AWM - Land Application of Municipal Biosolids, winter (OR)
#' - AWM - Overland Flow Process Treatment of Wastewater
#' - AWM - Rapid Infiltration Disposal of Wastewater
#' - AWM - Vegetated Treatment Area (PIA)
#' - AWM - Waste Field Storage Area (VT)
#' - BLM - Mechanical Treatment, Shredder
#' - BLM - Medusahead Invasion Susceptibility
#' - BLM - Soil Compaction Resistance
#' - Capping Fill Gravity Septic System (DE)
#' - CLASS RULE - Depth to any bedrock kind (5 classes) (NPS)
#' - CPI - Alfalfa Hay, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Alfalfa Hay, IRR - Klamath Valley and Basins (OR)
#' - CPI - Alfalfa Hay, IRR - Snake River Plains (ID)
#' - CPI - Alfalfa Hay, NIRR- Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - CPI - Small Grains Productivity Index (AK)
#' - DHS - Catastrophic Event, Large Animal Mortality, Incinerate
#' - DHS - Emergency Land Disposal of Milk
#' - DHS - Site for Composting Facility - Subsurface
#' - DHS - Suitability for Clay Liner Material
#' - ENG - Cohesive Soil Liner (MN)
#' - ENG - Construction Materials - Sand Source (MN)
#' - ENG - Construction Materials; Gravel Source (CT)
#' - ENG - Construction Materials; Gravel Source (NY)
#' - ENG - Construction Materials; Reclamation (DE)
#' - ENG - Construction Materials; Roadfill
#' - ENG - Construction Materials; Roadfill (AK)
#' - ENG - Construction Materials; Sand Source (NY)
#' - ENG - Construction Materials; Sand Source (VT)
#' - ENG - Construction Materials; Topsoil (AK)
#' - ENG - Construction Materials; Topsoil (DE)
#' - ENG - Construction Materials; Topsoil (MI)
#' - ENG - Construction Materials; Topsoil (OR)
#' - ENG - Conventional On-Site Septic Systems (TN)
#' - ENG - Deep Infiltration Systems
#' - ENG - Disposal Field Gravity (DE)
#' - ENG - Dwellings With Basements (OH)
#' - ENG - Ground-based Solar Arrays, Ballast Anchor Systems
#' - ENG - Large Animal Disposal, Trench (CT)
#' - ENG - Lawn, Landscape, Golf Fairway (MI)
#' - ENG - Lawn, Landscape, Golf Fairway (VT)
#' - ENG - Sanitary Landfill (Area) (OH)
#' - ENG - Sanitary Landfill (Trench)
#' - ENG - Sanitary Landfill (Trench) (AK)
#' - ENG - Septage Application - Surface (MN)
#' - ENG - Septic Tank Absorption Fields - At-Grade (MN)
#' - ENG - Septic Tank Absorption Fields - Mound (MN)
#' - ENG - Septic Tank Leaching Chamber (TX)
#' - ENG - Septic Tank, Subsurface Drip Irrigation (TX)
#' - ENG - Shallow Excavations
#' - ENG - Shallow Infiltration Systems
#' - ENG - Small Commercial Buildings
#' - ENG - Soil Potential of Road Salt Applications (CT)
#' - ENG - Source of Caliche (TX)
#' - ENG - Stormwater Management / Ponds (NY)
#' - ENG - Unlined Retention Systems
#' - Farm and Garden Composting Facility - Surface
#' - FOR - Biomass Harvest (MA)
#' - FOR - Black Walnut Suitability Index (KS)
#' - FOR - Displacement Potential (WA)
#' - FOR - Drought Vulnerable Soils
#' - FOR - General Harvest Season (ME)
#' - FOR - Harvest Equipment Operability
#' - FOR - Mechanical Site Preparation (Deep) (MD)
#' - FOR - Mechanical Site Preparation (Surface)
#' - FOR - Mechanical Site Preparation; Deep (CT)
#' - FOR - Potential Erosion Hazard (Road/Trail)
#' - FOR - Potential Fire Damage Hazard
#' - FOR - Potential Seedling Mortality
#' - FOR - Potential Seedling Mortality (MI)
#' - FOR - Potential Windthrow Hazard (ME)
#' - FOR - Potential Windthrow Hazard (MI)
#' - FOR - Road Suitability (Natural Surface) (ID)
#' - FOR - Rutting Hazard by Month
#' - FOR - Windthrow Hazard (WA)
#' - FOTG - NLI Interp Calculation - (IN)
#' - Fragile Soil Index
#' - GRL - Juniper Encroachment Potential (NM)
#' - GRL - NV range seeding (Wind C = 20) (NV)
#' - GRL - Pasture and Hayland SG (OH)
#' - GRL - Rangeland Prescribed Burning (TX)
#' - GRL - Rangeland Soil Seed Bank Suitability (NM)
#' - GRL-FSG-NP-W (MT)
#' - GRL-SHSI Soil Health Sustainability Index (MT)
#' - Ground-based Solar Arrays_saturationt(ME)
#' - Ground-based Solar Arrays_slope(ME)
#' - Inland Wetlands (CT)
#' - IRR-restrictive features for irrigation (OH)
#' - MIL - Excavations for Vehicle Fighting Position (DOD)
#' - MIL - Trafficability Veh. Type 1 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 2 dry season (DOD)
#' - MIL - Trafficability Veh. Type 3 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 6 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 6 dry season (DOD)
#' - Muscadine Wine Grape Site Desirability (Very Long)
#' - NCCPI - NCCPI Cotton Submodel (II)
#' - Permafrost Sensitivity (AK)
#' - Pressure Dose Capping Fill Septic System (DE)
#' - REC - Camp Areas (CT)
#' - REC - Off-Road Motorcycle Trails (CT)
#' - SAS - CMECS Substrate Class
#' - SAS - CMECS Substrate Subclass/Group
#' - SAS - Eelgrass Restoration Suitability
#' - SAS - Land Utilization of Dredged Materials
#' - SAS - Northern Quahog (Hard Clam) Habitat Suitability
#' - Septic System At Grade Shallow Field (alternative) (WV)
#' - Septic System At-Grade Bed (Alternate) (PA)
#' - Septic System CO-OP RFS III w/Drip Irrigation (PA)
#' - Septic System Drip Irrigation (alternative) (WV)
#' - Septic System Free Access Sand Filterw/Spray Irrigation (PA)
#' - Septic System Peat Based Option1 w/At-Grade Bed (Alt.) (PA)
#' - Septic System Spray Irrigation (PA)
#' - Septic System Steep Slope Sand Mound (Alternate) (PA)
#' - Shallow Infiltration Systems
#' - SOH - Organic Matter Depletion Potential, Irrigated (CA)
#' - SOH - Soil Surface Sealing
#' - TROP - Plantains Productivity
#' - URB/REC - Camp Areas (GA)
#' - URB/REC - Camp Areas (MI)
#' - URB/REC - Golf Fairways (OH)
#' - URB/REC - Off-Road Motorcycle Trails
#' - URB/REC - Paths and Trails (MI)
#' - URB/REC - Playgrounds (OH)
#' - Vinifera Wine Grape Site Desirability (Long to Medium)
#' - WLF - Chufa for Turkey Forage (LA)
#' - WLF - Food Plots for Upland Wildlife < 2 Acres (TX)
#' - WLF - Freshwater Wetland Plants (TX)
#' - WLF - Irrigated Saline Water Wetland Plants (TX)
#' - WLF - Riparian Herbaceous Plants (TX)
#' - WLF - Riparian Shrubs, Vines, & Trees (TX)
#' - WLF - Saline Water Wetland Plants (TX)
#' - WLF - Upland Mixed Deciduous & Coniferous Trees (TX)
#' - WMS - Constructing Grassed Waterways (TX)
#' - WMS - Constructing Terraces and Diversions (OH)
#' - WMS - Embankments, Dikes, and Levees (VT)
#' - WMS - Irrigation, Sprinkler (close spaced outlet drops)
#' - WMS - Irrigation, Sprinkler (general)
#' - WMS - Pond Reservoir Area (GA)
#' - WMS-Subsurface Water Management, Installation (ND)
#' - WMS-Subsurface Water Management, Outflow Quality (ND)
#' - AGR - Barley Yield (MT)
#' - AGR - Conventional Tillage (TX)
#' - AGR - Grape non-irrigated (MO)
#' - AGR - Industrial Hemp for Fiber and Seed Production
#' - AGR - Nitrate Leaching Potential, Irrigated (WA)
#' - AGR - Pasture hayland (MO)
#' - AGR - Pesticide Loss Potential-Soil Surface Runoff
#' - AGR - Prime Farmland (TX)
#' - AGR - Spring Wheat Yield (MT)
#' - AGR-Agronomic Concerns (ND)
#' - AGR-Pesticide and Nutrient Leaching Potential, NIRR (ND)
#' - AGR-Surface Salinity (ND)
#' - AGR-Water Erosion Potential (ND)
#' - Alaska Exempt Wetland Potential (AK)
#' - American Wine Grape Varieties Site Desirability (Short)
#' - AWM - Irrigation Disposal of Wastewater (MD)
#' - AWM - Manure and Food Processing Waste (DE)
#' - AWM - Manure Stacking - Site Evaluation (TX)
#' - AWM - Phosphorus Management (TX)
#' - AWM - Slow Rate Process Treatment of Wastewater
#' - BLM - Pygmy Rabbit Habitat Potential
#' - BLM - Rangeland Tillage
#' - BLM - Site Degradation Susceptibility
#' - CA Prime Farmland (CA)
#' - CLASS RULE - Depth to root limiting layer (5 classes) (NPS)
#' - Commodity Crop Productivity Index (Corn) (TN)
#' - CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Barley, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Potatoes, IRR - Snake River Plains (ID)
#' - CPI - Small Grains, NIRR - Palouse Prairies (OR)
#' - CPI - Small Grains, NIRR - Palouse Prairies (WA)
#' - CPI - Small Grains, NIRR - Snake River Plains (ID)
#' - CPI - Wheat, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Wild Hay, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - Deep Infiltration Systems
#' - DHS - Site for Composting Facility - Surface
#' - Elevated Sand Mound Septic System (DE)
#' - ENG - Animal Disposal by Composting (Catastrophic) (WV)
#' - ENG - Application of Municipal Sludge (TX)
#' - ENG - Closed-Loop Horizontal Geothermal Heat Pump (CT)
#' - ENG - Construction Materials; Gravel Source (IN)
#' - ENG - Construction Materials; Gravel Source (NE)
#' - ENG - Construction Materials; Reclamation (MD)
#' - ENG - Construction Materials; Reclamation (MI)
#' - ENG - Construction Materials; Roadfill (GA)
#' - ENG - Construction Materials; Sand Source (CT)
#' - ENG - Construction Materials; Sand Source (GA)
#' - ENG - Construction Materials; Topsoil (ID)
#' - ENG - Construction Materials; Topsoil (OH)
#' - ENG - Daily Cover for Landfill (OH)
#' - ENG - Disposal Field (NJ)
#' - ENG - Disposal Field Type Inst (NJ)
#' - ENG - Dwellings W/O Basements
#' - ENG - Dwellings With Basements
#' - ENG - Dwellings without Basements (AK)
#' - ENG - Lawn and Landscape (OH)
#' - ENG - Lawn, Landscape, Golf Fairway
#' - ENG - Local Roads and Streets (AK)
#' - ENG - Local Roads and Streets (GA)
#' - ENG - On-Site Waste Water Lagoons (MO)
#' - ENG - Pier Beam Building Foundations (TX)
#' - ENG - Sanitary Landfill (Area)
#' - ENG - Sanitary Landfill (Area) (AK)
#' - ENG - Septage Application - Incorporation or Injection (MN)
#' - ENG - Septic System; Disinfection, Surface Application (TX)
#' - ENG - Septic Tank Absorption Fields (FL)
#' - ENG - Septic Tank Absorption Fields (OH)
#' - ENG - Septic Tank Absorption Fields - Trench (MN)
#' - ENG - Sewage Lagoons (AK)
#' - ENG - Shallow Excavations (OH)
#' - ENG - Soil Suitability for SLAMM Marsh Migration (CT)
#' - ENG - Stormwater Management / Infiltration (NY)
#' - ENG - Stormwater Management / Wetlands (NY)
#' - FOR - Black Walnut Suitability (WI)
#' - FOR - Black Walnut Suitability (WV)
#' - FOR - Construction Limitations for Haul Roads/Log Landings
#' - FOR - Displacement Hazard
#' - FOR - Harvest Equipment Operability (DE)
#' - FOR - Harvest Equipment Operability (ME)
#' - FOR - Harvest Equipment Operability (MI)
#' - FOR - Log Landing Suitability (ID)
#' - FOR - Log Landing Suitability (MI)
#' - FOR - Log Landing Suitability (OR)
#' - FOR - Mechanical Planting Suitability (OH)
#' - FOR - Mechanical Site Preparation (Surface) (MD)
#' - FOR - Mechanical Site Preparation (Surface) (OH)
#' - FOR - Mechanical Site Preparation; Surface (CT)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (MI)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (OH)
#' - FOR - Potential Seedling Mortality (FL)
#' - FOR - Potential Seedling Mortality (OH)
#' - FOR - Road Suitability (Natural Surface) (VT)
#' - FOR - Soil Rutting Hazard
#' - FOTG - Indiana Soy Bean Yield Calculation (IN)
#' - FOTG - Indiana Wheat Yield Calculation (IN)
#' - FOTG - NLI report Calculation - (IN)
#' - GRL - Fencing, Post Depth =<24 inches
#' - GRL - Fencing, Post Depth Less Than 24 inches (TX)
#' - GRL - Fencing, Post Depth Less Than 36 inches (TX)
#' - GRL - NV range seeding (Wind C = 10) (NV)
#' - GRL - NV range seeding (Wind C = 30) (NV)
#' - GRL - Rangeland Chaining (TX)
#' - GRL - Rangeland Disking (TX)
#' - GRL - Rangeland Dozing/Grubbing (TX)
#' - GRL - Utah Juniper Encroachment Potential
#' - GRL - Western Juniper Encroachment Potential (OR)
#' - Ground-based Solar Arrays_bedrock_slope_anchor(ME)
#' - Ground-based Solar Arrays_saturation_flooding_Frost(ME)
#' - Hybrid Wine Grape Varieties Site Desirability (Medium)
#' - Lined Retention Systems
#' - MIL - Trafficability Veh. Type 1 dry season (DOD)
#' - MIL - Trafficability Veh. Type 3 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 3 dry season (DOD)
#' - MIL - Trafficability Veh. Type 4 dry season (DOD)
#' - MIL - Trafficability Veh. Type 5 1-pass wet season (DOD)
#' - NCCPI - NCCPI Corn Submodel (I)
#' - NCCPI - NCCPI Small Grains Submodel (II)
#' - NCCPI - NCCPI Soybeans Submodel (I)
#' - Peony Flowers Site Suitability (AK)
#' - Pressure Dose Full Depth Septic System (DE)
#' - REC - Camp Areas; Primitive (AK)
#' - REC - Paths and Trails (CT)
#' - Salinity Risk Index (ND)
#' - SAS - Eastern Oyster Habitat Restoration Suitability
#' - SAS - Mooring Anchor - Mushroom
#' - Septic System CO-OP RFS III w/At-Grade Bed (PA)
#' - Septic System Free Access Sand Filter w/At-Grade Bed (PA)
#' - Septic System Modified Subsurface Sand Filter (Alt.) (PA)
#' - Septic System Shallow In Ground Trench (conventional) (WV)
#' - Septic System Subsurface Sand Filter Bed (conventional) (PA)
#' - Septic System Subsurface Sand Filter Trench (standard) (PA)
#' - SOH - Limitations for Aerobic Soil Organisms
#' - URB - Concrete Driveways and Sidewalks (TX)
#' - URB - Dwellings on Concrete Slab (TX)
#' - URB - Lawns and Ornamental Plantings (TX)
#' - URB/REC - Paths and Trails
#' - URB/REC - Paths and Trails (GA)
#' - URB/REC - Playgrounds (MI)
#' - Vinifera Wine Grape Site Desirability (Long)
#' - WLF - Crawfish Aquaculture (TX)
#' - WLF - Desertic Herbaceous Plants (TX)
#' - WLF - Gopher Tortoise Burrowing Suitability
#' - WLF - Grain & Seed Crops for Food and Cover (TX)
#' - WMS - Constructing Grassed Waterways (OH)
#' - WMS - Irrigation, Surface (graded)
#' - WMS - Subsurface Drains - Installation (VT)
#' - WMS - Subsurface Water Management, System Performance
#' - WMS - Surface Drains (TX)
#' - WMS - Surface Irrigation Intake Family (TX)
#' - Septic System Low Pressure Pipe (alternative) (WV)
#' - Septic System Mound (alternative) (WV)
#' - Septic System Peat Based Option2 w/Spray Irrigation (PA)
#' - Septic System Steep Slope Mound (alternative) (WV)
#' - SOH - Concentration of Salts- Soil Surface
#' - SOH - Soil Susceptibility to Compaction
#' - Soil Habitat for Saprophyte Stage of Coccidioides
#' - Unlined Retention Systems
#' - URB - Commercial Metal Bldg; w/Reinforced Concrete Slab (TX)
#' - URB/REC - Picnic Areas (GA)
#' - URB/REC - Picnic Areas (MI)
#' - URB/REC - Picnic Areas (OH)
#' - Vinifera Wine Grape Site Desirability (Short)
#' - WLF - Burrowing Mammals & Reptiles (TX)
#' - WLF - Desert Tortoise (CA)
#' - WLF - Domestic Grasses & Legumes for Food and Cover (TX)
#' - WLF - Irrigated Grain & Seed Crops for Food & Cover (TX)
#' - WMS - Excavated Ponds (Aquifer-fed)
#' - WMS - Excavated Ponds (Aquifer-fed) (VT)
#' - WMS - Irrigation, General
#' - WMS - Irrigation, Micro (above ground)
#' - WMS - Irrigation, Micro (above ground) (VT)
#' - WMS - Irrigation, Micro (subsurface drip)
#' - WMS - Irrigation, Sprinkler (general) (VT)
#' - WMS - Pond Reservoir Area
#' - WMS - Pond Reservoir Area (OH)
#' - WMS - Subsurface Water Management, System Installation
#' - WMS - Constructing Terraces & Diversions (TX)
#' - WMS - Drainage (OH)
#' - WMS - Excavated Ponds (Aquifer-fed) (OH)
#' - WMS - Grape Production with Drip Irrigation (TX)
#' - WMS - Irrigation, Micro (subsurface drip) (VT)
#' - WMS - Irrigation, Surface (level)
#' - WMS - Pond Reservoir Area (MI)
#' - WMS - Pond Reservoir Area (VT)
#' - WMS - Sprinkler Irrigation (MT)
#' - WMS - Sprinkler Irrigation RDC (IL)
#' - WMS - Subsurface Drains - Performance (VT)
#' - WMS - Subsurface Water Management, Outflow Quality
#' - WMS - Surface Water Management, System
#' - WMS-Subsurface Water Management, Performance (ND)
#'
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
get_SDA_interpretation <- function(rulename,
                                   method = c("Dominant Component",
                                              "Dominant Condition",
                                              "Weighted Average",
                                              "None"),
                                   areasymbols = NULL,
                                   mukeys = NULL,
                                   WHERE = NULL,
                                   include_minors = TRUE,
                                   miscellaneous_areas = TRUE, 
                                   query_string = FALSE,
                                   not_rated_value = NA_real_,
                                   wide_reason = FALSE,
                                   dsn = NULL) {
  q <- .constructInterpQuery(
      method = method,
      interp = rulename,
      areasymbols = areasymbols,
      mukeys = mukeys,
      WHERE = WHERE,
      miscellaneous_areas = miscellaneous_areas, 
      include_minors = include_minors,
      sqlite = !is.null(dsn)
    )

  if (query_string) return(q)

  # execute query
  res <- SDA_query(q, dsn = dsn)

  # return if bad
  if (inherits(res, 'try-error')) {
    return(res)
  }

  # check rating column values
  ratingcols <- colnames(res)[grep("^rating_", colnames(res))]
  res[] <- lapply(colnames(res), function(x) {
    y <- res[[x]]
    if (x %in% ratingcols) {
      # SQL will set 99 rating value for class == "Not rated"
      y[is.na(y) | y == 99] <- not_rated_value
      return(y)
    }
    y
  })
  
  if (wide_reason) {
    res <- .create_wide_reason(res)
  }
  
  return(res)
}

.interpretationAggMethod <-  function(method) {
  # match to one of the available aggregation methods
  labels <- c("Dominant Component",
              "Weighted Average",
              "Dominant Condition",
              "None")
  method <- match.arg(toupper(method), toupper(labels))

  # determine column name suffix for method
  suffixes <- c('_dom_comp_',
                '_wtd_avg',
                '_dom_cond',
                '')
  modifier <- suffixes[match(method, toupper(labels))]

  # return list with method and modifier
  return(list(method = method,
              modifier = modifier))
}

.constructInterpQuery <- function(method, interp, areasymbols = NULL, mukeys = NULL, WHERE = NULL, miscellaneous_areas = FALSE, include_minors = TRUE, sqlite = FALSE) {

  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }

  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  }

  # todo check method and interp against lut
  agg_method <- .interpretationAggMethod(method)
  areasymbols <- soilDB::format_SQL_in_statement(areasymbols)
  switch(agg_method$method,
         "DOMINANT COMPONENT" = .interpretation_aggregation(interp, WHERE, dominant = TRUE, miscellaneous_areas = miscellaneous_areas, include_minors = include_minors, sqlite = sqlite),
         "DOMINANT CONDITION" = .interpretation_by_condition(interp, WHERE, dominant = TRUE, miscellaneous_areas = miscellaneous_areas, include_minors = include_minors, sqlite = sqlite),
         "WEIGHTED AVERAGE" =   .interpretation_weighted_average(interp, WHERE, miscellaneous_areas = miscellaneous_areas, include_minors = include_minors, sqlite = sqlite),
         "NONE" =               .interpretation_aggregation(interp, WHERE, miscellaneous_areas = miscellaneous_areas, include_minors = include_minors, sqlite = sqlite)
  )
}

.cleanRuleColumnName <- function(x) gsub("[^A-Za-z0-9]", "", gsub(">", "GT", gsub("<", "LT", gsub("=", "EQ", x, fixed = TRUE), fixed = TRUE), fixed = TRUE))

.interpretation_by_condition <- function(interp, where_clause, dominant = TRUE, miscellaneous_areas = FALSE, include_minors = TRUE, sqlite = FALSE) {
  aggfun <- "STRING_AGG(CONCAT(rulename, ' \"', interphrc, '\" (', interphr, ')'), '; ')"
  if (sqlite) aggfun <- "(GROUP_CONCAT(rulename || '  \"' || interphrc || '\" (' || interphr || ')', '; ') || '; ')"
  .q0 <- function(q, x) .LIMIT_N(sprintf(q, ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"), x),
                                 n = 1, sqlite = sqlite)
  .q1 <- function(x) .q0("SELECT ROUND (AVG(interphr) OVER (PARTITION BY interphrc), 2) FROM mapunit AS mu 
                          INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                          LEFT JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s' 
                          GROUP BY interphrc, interphr 
                          ORDER BY SUM (comppct_r) DESC", x)
  .q2 <- function(x) .q0("SELECT SUM(comppct_r) AS sum_comppct_r FROM mapunit AS mu 
                          INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                          LEFT JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s' 
                          GROUP BY interphrc
                          ORDER BY sum_comppct_r DESC", x)
  .q3 <- function(x) .q0("SELECT interphrc FROM mapunit AS mu 
                          INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                          LEFT JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s' 
                          GROUP BY interphrc, comppct_r 
                          ORDER BY SUM(comppct_r) OVER (PARTITION BY interphrc) DESC", x)
  sprintf("SELECT mapunit.mukey, areasymbol, musym, muname, 
  %s
  FROM legend
  INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
  INNER JOIN component ON component.mukey = mapunit.mukey %s %s
  ORDER BY mapunit.mukey, areasymbol, musym, muname",
          paste0(sapply(interp, function(x) sprintf("
    (%s) AS [rating_%s],
    (%s) AS [total_comppct_%s],
    (%s) AS [class_%s],
    (SELECT %s FROM mapunit AS mu 
     INNER JOIN component AS c ON c.mukey = mu.mukey %s AND c.cokey = component.cokey
     INNER JOIN cointerp ON c.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth != 0 AND mrulename LIKE '%s') AS [reason_%s]",
   .q1(x), .cleanRuleColumnName(x),
   .q2(x), .cleanRuleColumnName(x),
   .q3(x), .cleanRuleColumnName(x),
   aggfun, ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
   x, .cleanRuleColumnName(x))), collapse = ", "), where_clause, ifelse(miscellaneous_areas, "", "AND component.compkind != 'miscellaneous area'"),
  ifelse(dominant, paste0("AND component.cokey = (", .LIMIT_N(sprintf("SELECT c1.cokey FROM component AS c1 
                                                               INNER JOIN mapunit AS mu ON c1.mukey = mu.mukey AND c1.mukey = mapunit.mukey %s
                                                               ORDER BY c1.comppct_r DESC, c1.cokey", ifelse(miscellaneous_areas, "", "AND c1.compkind != 'miscellaneous area'")), 
                                                              n = 1, sqlite = sqlite), ")", "")))
}

.interpretation_aggregation <- function(interp, where_clause, dominant = FALSE, miscellaneous_areas = FALSE, include_minors = TRUE, sqlite = FALSE) {
  aggfun <- "STRING_AGG(CONCAT(rulename, ' \"', interphrc, '\" (', interphr, ')'), '; ')"
  if (sqlite) aggfun <- "(GROUP_CONCAT(rulename || '  \"' || interphrc || '\" (' || interphr || ')', '; ') || '; ')"
  sprintf("SELECT mapunit.mukey, component.cokey, areasymbol, musym, muname, compname, compkind, comppct_r, majcompflag,
                %s
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
                INNER JOIN component ON component.mukey = mapunit.mukey %s",
                paste0(sapply(interp, function(x) sprintf("
  (SELECT interphr FROM component AS c0 
   INNER JOIN cointerp ON c0.cokey = cointerp.cokey AND component.cokey = c0.cokey AND ruledepth = 0 AND mrulename LIKE '%s') as [rating_%s],
  (SELECT interphrc FROM component AS c1 
   INNER JOIN cointerp ON c1.cokey = cointerp.cokey AND c1.cokey = component.cokey AND ruledepth = 0 AND mrulename LIKE '%s') as [class_%s],
  (SELECT %s FROM mapunit AS mu 
   INNER JOIN component AS c ON c.mukey = mu.mukey %s AND c.cokey = component.cokey AND mu.mukey = mapunit.mukey 
   INNER JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth != 0 AND mrulename = '%s') as [reason_%s]",
                                      x, .cleanRuleColumnName(x),
                                      x, .cleanRuleColumnName(x),
                                      aggfun, ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
                                      x, .cleanRuleColumnName(x))),
                                      collapse = ", "), where_clause,
  ifelse(dominant, sprintf("AND component.cokey = (%s)", .LIMIT_N(sprintf("SELECT c1.cokey FROM component AS c1
                                   INNER JOIN mapunit AS mu ON c1.mukey = mu.mukey AND c1.mukey = mapunit.mukey %s
                                   ORDER BY c1.comppct_r DESC, c1.cokey", ifelse(miscellaneous_areas, "", "AND c1.compkind != 'miscellaneous area'")), 
                                                                  n = 1, sqlite = sqlite)), ""))
}

.interpretation_weighted_average <- function(interp, where_clause, miscellaneous_areas = FALSE, include_minors = TRUE, sqlite = FALSE) {
  stopifnot(!sqlite)
  sprintf("SELECT mapunit.mukey, areasymbol, musym, muname,
                %s
                INTO #main
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
                INNER JOIN component ON component.mukey = mapunit.mukey %s
                GROUP BY areasymbol, musym, muname, mapunit.mukey
                SELECT areasymbol, musym, muname, mukey,
                %s,
                %s,
                %s
                FROM #main
                DROP TABLE #main",
          paste0(sapply(interp, function(x) sprintf("(SELECT TOP 1 CASE WHEN ruledesign = 1 THEN 'limitation'
                  WHEN ruledesign = 2 THEN 'suitability' END
                  FROM mapunit AS mu
                  INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                  INNER JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s'
                  GROUP BY mu.mukey, ruledesign) AS [design_%s],
                  ROUND ((SELECT SUM (interphr * comppct_r)
                  FROM mapunit AS mu
                  INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                  INNER JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s'
                  GROUP BY mu.mukey),2) AS [rating_%s],
                  ROUND ((SELECT SUM (comppct_r)
                  FROM mapunit AS mu
                  INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                  INNER JOIN cointerp ON c.cokey = cointerp.cokey AND ruledepth = 0 AND mrulename LIKE '%s'
                  AND (interphr) IS NOT NULL GROUP BY mu.mukey),2) AS [sum_com_%s],
                  (SELECT STRING_AGG(CONCAT(interphrc, ' (', interphr, ')'), '; ')
                   FROM mapunit AS mu
                   INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s
                   INNER JOIN cointerp ON c.cokey = cointerp.cokey 
                   AND ruledepth != 0 AND mrulename LIKE '%s'
                   GROUP BY mu.mukey) AS [reason_%s]",
                                                    ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
                                                    x, .cleanRuleColumnName(x),
                                                    ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
                                                    x, .cleanRuleColumnName(x),
                                                    ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
                                                    x, .cleanRuleColumnName(x),
                                                    ifelse(miscellaneous_areas, "", "AND c.compkind != 'miscellaneous area'"),
                                                    x, .cleanRuleColumnName(x))), collapse = ", "),
           where_clause,
          ifelse(miscellaneous_areas, "", "AND compkind != 'miscellaneous area'"),
          paste0(sapply(interp,
                        function(x) sprintf("ISNULL(ROUND(([rating_%s] / [sum_com_%s]),2), 99) AS [rating_%s]",
                                            .cleanRuleColumnName(x), .cleanRuleColumnName(x), .cleanRuleColumnName(x))),
                 collapse = ", "),
          paste0(sapply(interp,
                        function(x) sprintf(gsub("design", paste0("[design_", .cleanRuleColumnName(x),"]"),
                                                 gsub("sum_com", paste0("[sum_com_", .cleanRuleColumnName(x), "]"),
                                                      gsub("rating", paste0("[rating_", .cleanRuleColumnName(x), "]"),
                       "CASE WHEN rating IS NULL THEN 'Not Rated'
                  WHEN design = 'suitability' AND ROUND((rating/sum_com),2) <= 0 THEN 'Not suited'
                  WHEN design = 'suitability' AND ROUND((rating/sum_com),2) > 0.001 and ROUND((rating/sum_com),2) <=0.333 THEN 'Poorly suited'
                  WHEN design = 'suitability' AND ROUND((rating/sum_com),2) > 0.334 and ROUND((rating/sum_com),2) <=0.666  THEN 'Moderately suited'
                  WHEN design = 'suitability' AND ROUND((rating/sum_com),2) > 0.667 and ROUND((rating/sum_com),2) <=0.999  THEN 'Moderately well suited'
                  WHEN design = 'suitability' AND ROUND((rating/sum_com),2) = 1 THEN 'Well suited'
                  WHEN design = 'limitation' AND ROUND((rating/sum_com),2) <= 0 THEN 'Not limited'
                  WHEN design = 'limitation' AND ROUND((rating/sum_com),2) > 0.001 and ROUND((rating/sum_com),2) <=0.333 THEN 'Slightly limited'
                  WHEN design = 'limitation' AND ROUND((rating/sum_com),2) > 0.334 and ROUND((rating/sum_com),2) <=0.666 THEN 'Somewhat limited'
                  WHEN design = 'limitation' AND ROUND((rating/sum_com),2) > 0.667 and ROUND((rating/sum_com),2) <=0.999 THEN 'Moderately limited'
                  WHEN design = 'limitation' AND ROUND((rating/sum_com),2) = 1 THEN 'Very limited' END AS [class_%s]"))),
                       .cleanRuleColumnName(x))),
                 collapse = ", "), paste0(sapply(interp, function(x) sprintf("[reason_%s]", .cleanRuleColumnName(x))), collapse = ", "))
}

.LIMIT_N <- function(query, n = 1, sqlite = FALSE) {
  if (!sqlite) return(gsub("SELECT ", paste("SELECT TOP", n, ""), query))
  paste(query, paste("LIMIT", n, ""))
}

.create_wide_reason <- function(x, not_rated_value = NA_real_) {
  cn <- colnames(x)[grepl("^reason_", colnames(x))]
  for (n in cn) {
    x <- cbind(x, data.table::rbindlist(lapply(strsplit(as.character(x[[n]]), "; "), function(x) {
      x3 <- as.data.frame(data.table::rbindlist(lapply(
          strsplit(gsub(
            "(.*) \"(.*)\" \\((.*)\\)", "\\1;\\2;\\3", x
          ), ";"), function(y) as.data.frame(t(y))
        ), fill = TRUE))
        if (ncol(x3) == 3) {
          x4 <- as.data.frame(as.list(x3[, 3]))
          colnames(x4) <- paste0("rating_", n , "_", .cleanRuleColumnName(x3[, 1]))
        } else {
          x4 <- data.frame(NA_real_) # numeric ratings are always NA if not rated, class "Not rated"
          colnames(x4) <- paste0("rating_", n , "_Notrated")
        }
        x4
    }), fill = TRUE))
  }
  x
}
