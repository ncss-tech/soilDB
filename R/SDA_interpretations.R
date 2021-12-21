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
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param not_rated_value used where rating class is "Not Rated". Default: `NA_real`
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'
#'  # get two forestry interpretations for CA630
#'  get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
#'                           "FOR - Road Suitability (Natural Surface)"),
#'                         method = "Dominant Condition",
#'                         areasymbols = "CA630")
#' }
#' }
#'
#'
#' @details
#'
#' ## Rule Names in `cointerp` table
#'
#' - AGR-Agronomic Concerns (ND)
#' - AGR-Available Water Capacity (ND)
#' - AGR-Natural Fertility (ND)
#' - AGR-Pesticide and Nutrient Leaching Potential, NIRR (ND)
#' - AGR-Pesticide and Nutrient Runoff Potential (ND)
#' - AGR-Physical Limitations (ND)
#' - AGR-Rooting Depth (ND)
#' - AGR-Sodicity (ND)
#' - AGR-Subsurface Salinity (ND)
#' - AGR-Surface Crusting (ND)
#' - AGR-Surface Salinity (ND)
#' - AGR-Water Erosion (ND)
#' - AGR-Wind Erosion (ND)
#' - AGR - Air Quality; PM10 (TX)
#' - AGR - Air Quality; PM2_5 (TX)
#' - AGR - Avocado Root Rot Hazard (CA)
#' - AGR - Barley Yield (MT)
#' - AGR - California Revised Storie Index (CA)
#' - AGR - Conventional Tillage (TX)
#' - AGR - Filter Strips (TX)
#' - AGR - Grape non-irrigated (MO)
#' - AGR - Hops Site Suitability (WA)
#' - AGR - Index for alfalfa hay, irrigated (NV)
#' - AGR - Map Unit Cropland Productivity (MN)
#' - AGR - Mulch Till (TX)
#' - AGR - Nitrate Leaching Potential, Irrigated (WA)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (MA)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (MT)
#' - AGR - Nitrate Leaching Potential, Nonirrigated (WA)
#' - AGR - No Till (TX)
#' - AGR - No Till (VT)
#' - AGR - No Till, Tile Drained (TX)
#' - AGR - Oats Yield (MT)
#' - AGR - Orchard Groups (TX)
#' - AGR - Pasture hayland (MO)
#' - AGR - Pesticide Loss Potential-Leaching
#' - AGR - Pesticide Loss Potential-Leaching (NE)
#' - AGR - Pesticide Loss Potential-Soil Surface Runoff
#' - AGR - Pesticide Loss Potential-Soil Surface Runoff (NE)
#' - AGR - Plant Growth Index PGI no Climate Adj. (TX)
#' - AGR - Plant Growth Index PGI with Climate Adj. (TX)
#' - AGR - Plant Growth Index PGI with Climate Adj. MAP,MAAT (TX)
#' - AGR - Rangeland Grass/Herbaceous Productivity Index (TX)
#' - AGR - Ridge Till (TX)
#' - AGR - Rutting Hazard =< 10,000 Pounds per Wheel (TX)
#' - AGR - Rutting Hazard > 10,000 Pounds per Wheel (TX)
#' - AGR - Selenium Leaching Potential (CO)
#' - AGR - Spring Wheat Yield (MT)
#' - AGR - Water Erosion Potential (TX)
#' - AGR - Water Erosion Potential Wide Ratings Array (TX)
#' - AGR - Wind Erosion Potential (TX)
#' - AGR - Wind Erosion Potential Wide Ratings Array (TX)
#' - AGR - Wine Grape Site Suitability (WA)
#' - AGR - Winter Wheat Yield (MT)
#' - Alaska Exempt Wetland Potential (AK)
#' - American Wine Grape Varieties Site Desirability (Long)
#' - American Wine Grape Varieties Site Desirability (Medium)
#' - American Wine Grape Varieties Site Desirability (Short)
#' - American Wine Grape Varieties Site Desirability (Very Long)
#' - AWM - Animal Mortality Disposal (Catastrophic) (MO)
#' - AWM - Filter Group (OH)
#' - AWM - Irrigation Disposal of Wastewater
#' - AWM - Irrigation Disposal of Wastewater (DE)
#' - AWM - Irrigation Disposal of Wastewater (MD)
#' - AWM - Irrigation Disposal of Wastewater (OH)
#' - AWM - Irrigation Disposal of Wastewater (VT)
#' - AWM - Land App of Municipal Sewage Sludge (DE)
#' - AWM - Land App of Municipal Sewage Sludge (MD)
#' - AWM - Land Application of Dry and Slurry Manure (TX)
#' - AWM - Land Application of Milk (CT)
#' - AWM - Land Application of Municipal Biosolids, spring (OR)
#' - AWM - Land Application of Municipal Biosolids, summer (OR)
#' - AWM - Land Application of Municipal Biosolids, winter (OR)
#' - AWM - Land Application of Municipal Sewage Sludge
#' - AWM - Land Application of Municipal Sewage Sludge (OH)
#' - AWM - Land Application of Municipal Sewage Sludge (VT)
#' - AWM - Large Animal Disposal, Pit (MN)
#' - AWM - Manure and Food Processing Waste
#' - AWM - Manure and Food Processing Waste (DE)
#' - AWM - Manure and Food Processing Waste (MD)
#' - AWM - Manure and Food Processing Waste (OH)
#' - AWM - Manure and Food Processing Waste (VT)
#' - AWM - Manure Stacking - Site Evaluation (TX)
#' - AWM - Overland Flow Process Treatment of Wastewater
#' - AWM - Overland Flow Process Treatment of Wastewater (VT)
#' - AWM - Phosphorus Management (TX)
#' - AWM - Rapid Infil Disposal of Wastewater (DE)
#' - AWM - Rapid Infil Disposal of Wastewater (MD)
#' - AWM - Rapid Infiltration Disposal of Wastewater
#' - AWM - Rapid Infiltration Disposal of Wastewater (VT)
#' - AWM - Sensitive Soil Features (MN)
#' - AWM - Slow Rate Process Treatment of Wastewater
#' - AWM - Slow Rate Process Treatment of Wastewater (VT)
#' - AWM - Vegetated Treatment Area (PIA)
#' - AWM - Waste Field Storage Area (VT)
#' - BLM-Reclamation Suitability (MT)
#' - BLM - Chaining Suitability
#' - BLM - Fencing
#' - BLM - Fire Damage Susceptibility
#' - BLM - Fugitive Dust Resistance
#' - BLM - Mechanical Treatment, Rolling Drum
#' - BLM - Mechanical Treatment, Shredder
#' - BLM - Medusahead Invasion Susceptibility
#' - BLM - Pygmy Rabbit Habitat Potential
#' - BLM - Rangeland Drill
#' - BLM - Rangeland Seeding, Colorado Plateau Ecoregion
#' - BLM - Rangeland Seeding, Great Basin Ecoregion
#' - BLM - Rangeland Tillage
#' - BLM - Site Degradation Susceptibility
#' - BLM - Soil Compaction Resistance
#' - BLM - Soil Restoration Potential
#' - BLM - Yellow Star-thistle Invasion Susceptibility
#' - CA Prime Farmland (CA)
#' - Capping Fill Gravity Septic System (DE)
#' - CLASS RULE - Depth to any bedrock kind (5 classes) (NPS)
#' - CLASS RULE - Depth to lithic bedrock (5 classes) (NPS)
#' - CLASS RULE - Depth to non-lithic bedrock (5 classes) (NPS)
#' - CLASS RULE - Depth to root limiting layer (5 classes) (NPS)
#' - CLASS RULE - Soil Inorganic Carbon kg/m2 to 2m (NPS)
#' - CLASS RULE - Soil Organic Carbon kg/m2 to 2m (NPS)
#' - CLR-cropland limitation for corn and soybeans (IN)
#' - CLR-pastureland limitation (IN)
#' - Commodity Crop Productivity Index (Corn) (WI)
#' - CPI - Alfalfa Hay, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Alfalfa Hay, IRR - Klamath Valley and Basins (OR)
#' - CPI - Alfalfa Hay, IRR - Snake River Plains (ID)
#' - CPI - Alfalfa Hay, NIRR- Eastern Idaho Plateaus (ID)
#' - CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - CPI - Barley, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Barley, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Grass Hay, IRR - Klamath Valleys and Basins (OR)
#' - CPI - Grass Hay, NIRR - Klamath Valleys and Basins (OR)
#' - CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - CPI - Potatoes, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Potatoes, IRR - Snake River Plains (ID)
#' - CPI - Small Grains Productivity Index (AK)
#' - CPI - Small Grains, IRR - Snake River Plains (ID)
#' - CPI - Small Grains, NIRR - Palouse Prairies (ID)
#' - CPI - Small Grains, NIRR - Palouse Prairies (OR)
#' - CPI - Small Grains, NIRR - Palouse Prairies (WA)
#' - CPI - Small Grains, NIRR - Snake River Plains (ID)
#' - CPI - Wheat, IRR - Eastern Idaho Plateaus (ID)
#' - CPI - Wheat, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Wild Hay, NIRR - Eastern Idaho Plateaus (ID)
#' - CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)
#' - CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)
#' - Deep Infiltration Systems
#' - DHS - Catastrophic Event, Large Animal Mortality, Burial
#' - DHS - Catastrophic Event, Large Animal Mortality, Incinerate
#' - DHS - Catastrophic Mortality, Large Animal Disposal, Pit
#' - DHS - Catastrophic Mortality, Large Animal Disposal, Trench
#' - DHS - Emergency Animal Mortality Disposal by Shallow Burial
#' - DHS - Emergency Land Disposal of Milk
#' - DHS - Potential for Radioactive Bioaccumulation
#' - DHS - Potential for Radioactive Sequestration
#' - DHS - Rubble and Debris Disposal, Large-Scale Event
#' - DHS - Site for Composting Facility - Subsurface
#' - DHS - Site for Composting Facility - Surface
#' - DHS - Suitability for Clay Liner Material
#' - DHS - Suitability for Composting Medium and Final Cover
#' - Elevated Sand Mound Septic System (DE)
#' - ENG - Animal Disposal by Composting (Catastrophic) (WV)
#' - ENG - Application of Municipal Sludge (TX)
#' - ENG - Aquifer Assessment - 7081 (MN)
#' - ENG - Closed-Loop Horizontal Geothermal Heat Pump (CT)
#' - ENG - Cohesive Soil Liner (MN)
#' - ENG - Construction Materials - Gravel Source (MN)
#' - ENG - Construction Materials - Sand Source (MN)
#' - ENG - Construction Materials; Gravel Source
#' - ENG - Construction Materials; Gravel Source (AK)
#' - ENG - Construction Materials; Gravel Source (CT)
#' - ENG - Construction Materials; Gravel Source (ID)
#' - ENG - Construction Materials; Gravel Source (IN)
#' - ENG - Construction Materials; Gravel Source (MI)
#' - ENG - Construction Materials; Gravel Source (NE)
#' - ENG - Construction Materials; Gravel Source (NY)
#' - ENG - Construction Materials; Gravel Source (OH)
#' - ENG - Construction Materials; Gravel Source (OR)
#' - ENG - Construction Materials; Gravel Source (VT)
#' - ENG - Construction Materials; Gravel Source (WA)
#' - ENG - Construction Materials; Reclamation
#' - ENG - Construction Materials; Reclamation (DE)
#' - ENG - Construction Materials; Reclamation (MD)
#' - ENG - Construction Materials; Reclamation (MI)
#' - ENG - Construction Materials; Reclamation (OH)
#' - ENG - Construction Materials; Roadfill
#' - ENG - Construction Materials; Roadfill (AK)
#' - ENG - Construction Materials; Roadfill (GA)
#' - ENG - Construction Materials; Roadfill (OH)
#' - ENG - Construction Materials; Sand Source
#' - ENG - Construction Materials; Sand Source (AK)
#' - ENG - Construction Materials; Sand Source (CT)
#' - ENG - Construction Materials; Sand Source (GA)
#' - ENG - Construction Materials; Sand Source (ID)
#' - ENG - Construction Materials; Sand Source (IN)
#' - ENG - Construction Materials; Sand Source (NY)
#' - ENG - Construction Materials; Sand Source (OH)
#' - ENG - Construction Materials; Sand Source (OR)
#' - ENG - Construction Materials; Sand Source (VT)
#' - ENG - Construction Materials; Sand Source (WA)
#' - ENG - Construction Materials; Topsoil
#' - ENG - Construction Materials; Topsoil (AK)
#' - ENG - Construction Materials; Topsoil (DE)
#' - ENG - Construction Materials; Topsoil (GA)
#' - ENG - Construction Materials; Topsoil (ID)
#' - ENG - Construction Materials; Topsoil (MD)
#' - ENG - Construction Materials; Topsoil (MI)
#' - ENG - Construction Materials; Topsoil (OH)
#' - ENG - Construction Materials; Topsoil (OR)
#' - ENG - Construction Materials; Topsoil (WA)
#' - ENG - Daily Cover for Landfill
#' - ENG - Daily Cover for Landfill (AK)
#' - ENG - Daily Cover for Landfill (OH)
#' - ENG - Disposal Field (NJ)
#' - ENG - Disposal Field Gravity (DE)
#' - ENG - Disposal Field Suitability Class (NJ)
#' - ENG - Disposal Field Type Inst (NJ)
#' - ENG - Dwellings W/O Basements
#' - ENG - Dwellings W/O Basements (OH)
#' - ENG - Dwellings With Basements
#' - ENG - Dwellings with Basements (AK)
#' - ENG - Dwellings With Basements (OH)
#' - ENG - Dwellings without Basements (AK)
#' - ENG - Large Animal Disposal, Pit (CT)
#' - ENG - Large Animal Disposal, Trench (CT)
#' - ENG - Lawn and Landscape (OH)
#' - ENG - Lawn, Landscape, Golf Fairway
#' - ENG - Lawn, landscape, golf fairway (CT)
#' - ENG - Lawn, Landscape, Golf Fairway (MI)
#' - ENG - Lawn, Landscape, Golf Fairway (VT)
#' - ENG - Local Roads and Streets
#' - ENG - Local Roads and Streets (AK)
#' - ENG - Local Roads and Streets (GA)
#' - ENG - Local Roads and Streets (OH)
#' - ENG - New Ohio Septic Rating (OH)
#' - ENG - On-Site Waste Water Absorption Fields (MO)
#' - ENG - On-Site Waste Water Lagoons (MO)
#' - ENG - OSHA Soil Types (TX)
#' - ENG - Pier Beam Building Foundations (TX)
#' - ENG - Sanitary Landfill (Area)
#' - ENG - Sanitary Landfill (Area) (AK)
#' - ENG - Sanitary Landfill (Area) (OH)
#' - ENG - Sanitary Landfill (Trench)
#' - ENG - Sanitary Landfill (Trench) (AK)
#' - ENG - Sanitary Landfill (Trench) (OH)
#' - ENG - Septage Application - Incorporation or Injection (MN)
#' - ENG - Septage Application - Surface (MN)
#' - ENG - Septic System; Disinfection, Surface Application (TX)
#' - ENG - Septic Tank Absorption Fields
#' - ENG - Septic Tank Absorption Fields - At-Grade (MN)
#' - ENG - Septic Tank Absorption Fields - Mound (MN)
#' - ENG - Septic Tank Absorption Fields - Trench (MN)
#' - ENG - Septic Tank Absorption Fields (AK)
#' - ENG - Septic Tank Absorption Fields (DE)
#' - ENG - Septic Tank Absorption Fields (FL)
#' - ENG - Septic Tank Absorption Fields (MD)
#' - ENG - Septic Tank Absorption Fields (NY)
#' - ENG - Septic Tank Absorption Fields (OH)
#' - ENG - Septic Tank Absorption Fields (TX)
#' - ENG - Septic Tank Leaching Chamber (TX)
#' - ENG - Septic Tank, Gravity Disposal (TX)
#' - ENG - Septic Tank, Subsurface Drip Irrigation (TX)
#' - ENG - Sewage Lagoons
#' - ENG - Sewage Lagoons (AK)
#' - ENG - Sewage Lagoons (OH)
#' - ENG - Shallow Excavations
#' - ENG - Shallow Excavations (AK)
#' - ENG - Shallow Excavations (MI)
#' - ENG - Shallow Excavations (OH)
#' - ENG - Small Commercial Buildings
#' - ENG - Small Commercial Buildings (OH)
#' - ENG - Soil Potential of Road Salt Applications (CT)
#' - ENG - Soil Potential Ratings of SSDS (CT)
#' - ENG - Source of Caliche (TX)
#' - ENG - Stormwater Management / Infiltration (NY)
#' - ENG - Stormwater Management / Ponds (NY)
#' - ENG - Stormwater Management / Wetlands (NY)
#' - ENG - Unpaved Local Roads and Streets
#' - Farm and Garden Composting Facility - Surface
#' - FOR-Biomass Harvest (WI)
#' - FOR-Construction Limitations for Haul Roads/Log Landings(ME)
#' - FOR - Biomass Harvest (MA)
#' - FOR - Black Walnut Suitability Index (KS)
#' - FOR - Black Walnut Suitability Index (MO)
#' - FOR - Compaction Potential (WA)
#' - FOR - Construction Limitations - Haul Roads/Log Landing (OH)
#' - FOR - Construction Limitations For Haul Roads (MI)
#' - FOR - Construction Limitations for Haul Roads/Log Landings
#' - FOR - Damage by Fire (OH)
#' - FOR - Displacement Hazard
#' - FOR - Displacement Potential (WA)
#' - FOR - General Harvest Season (ME)
#' - FOR - General Harvest Season (VT)
#' - FOR - Hand Planting Suitability
#' - FOR - Hand Planting Suitability (ME)
#' - FOR - Hand Planting Suitability, MO13 (DE)
#' - FOR - Hand Planting Suitability, MO13 (MD)
#' - FOR - Harvest Equipment Operability
#' - FOR - Harvest Equipment Operability (DE)
#' - FOR - Harvest Equipment Operability (MD)
#' - FOR - Harvest Equipment Operability (ME)
#' - FOR - Harvest Equipment Operability (MI)
#' - FOR - Harvest Equipment Operability (OH)
#' - FOR - Harvest Equipment Operability (VT)
#' - FOR - Log Landing Suitability
#' - FOR - Log Landing Suitability (ID)
#' - FOR - Log Landing Suitability (ME)
#' - FOR - Log Landing Suitability (MI)
#' - FOR - Log Landing Suitability (OR)
#' - FOR - Log Landing Suitability (VT)
#' - FOR - Log Landing Suitability (WA)
#' - FOR - Mechanical Planting Suitability
#' - FOR - Mechanical Planting Suitability (CT)
#' - FOR - Mechanical Planting Suitability (ME)
#' - FOR - Mechanical Planting Suitability (OH)
#' - FOR - Mechanical Planting Suitability, MO13 (DE)
#' - FOR - Mechanical Planting Suitability, MO13 (MD)
#' - FOR - Mechanical Site Preparation (Deep)
#' - FOR - Mechanical Site Preparation (Deep) (DE)
#' - FOR - Mechanical Site Preparation (Deep) (MD)
#' - FOR - Mechanical Site Preparation (Surface)
#' - FOR - Mechanical Site Preparation (Surface) (DE)
#' - FOR - Mechanical Site Preparation (Surface) (MD)
#' - FOR - Mechanical Site Preparation (Surface) (MI)
#' - FOR - Mechanical Site Preparation (Surface) (OH)
#' - FOR - Mechanical Site Preparation; Deep (CT)
#' - FOR - Mechanical Site Preparation; Surface (ME)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (MI)
#' - FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (OH)
#' - FOR - Potential Erosion Hazard (Road/Trail)
#' - FOR - Potential Erosion Hazard (Road/Trail) (PIA)
#' - FOR - Potential Erosion Hazard, Road/Trail, Spring Thaw (AK)
#' - FOR - Potential Fire Damage Hazard
#' - FOR - Potential Seedling Mortality
#' - FOR - Potential Seedling Mortality (FL)
#' - FOR - Potential Seedling Mortality (MI)
#' - FOR - Potential Seedling Mortality (OH)
#' - FOR - Potential Seedling Mortality (PIA)
#' - FOR - Potential Seedling Mortality (VT)
#' - FOR - Potential Seedling Mortality(ME)
#' - FOR - Potential Windthrow Hazard (ME)
#' - FOR - Potential Windthrow Hazard (MI)
#' - FOR - Potential Windthrow Hazard (NY)
#' - FOR - Potential Windthrow Hazard (VT)
#' - FOR - Puddling Hazard
#' - FOR - Puddling Potential (WA)
#' - FOR - Road Suitability (Natural Surface)
#' - FOR - Road Suitability (Natural Surface) (ID)
#' - FOR - Road Suitability (Natural Surface) (ME)
#' - FOR - Road Suitability (Natural Surface) (OH)
#' - FOR - Road Suitability (Natural Surface) (OR)
#' - FOR - Road Suitability (Natural Surface) (VT)
#' - FOR - Road Suitability (Natural Surface) (WA)
#' - FOR - Rutting Hazard by Month
#' - FOR - Rutting Hazard by Season
#' - FOR - Shortleaf pine littleleaf disease susceptibility
#' - FOR - Soil Compactibility Risk
#' - FOR - Soil Rutting Hazard
#' - FOR - Soil Rutting Hazard (ME)
#' - FOR - Soil Rutting Hazard (OH)
#' - FOR - Soil Sustainability Forest Biomass Harvesting (CT)
#' - FOR - White Oak Suitability (MO)
#' - FOR - Windthrow Hazard
#' - FOR - Windthrow Hazard (WA)
#' - FOR (USFS) - Road Construction/Maintenance (Natural Surface)
#' - FOTG - Indiana Corn Yield Calculation (IN)
#' - FOTG - Indiana Slippage Potential (IN)
#' - FOTG - Indiana Soy Bean Yield Calculation (IN)
#' - FOTG - Indiana Wheat Yield Calculation (IN)
#' - Fragile Soil Index
#' - Gravity Full Depth Septic System (DE)
#' - GRL-FSG-NP-W (MT)
#' - GRL - Excavations to 24 inches for Plastic Pipelines (TX)
#' - GRL - Fencing, 24 inch Post Depth (MT)
#' - GRL - Fencing, Post Depth =<24 inches
#' - GRL - Fencing, Post Depth =<36 inches
#' - GRL - Fencing, Post Depth Less Than 24 inches (TX)
#' - GRL - Fencing, Post Depth Less Than 36 inches (TX)
#' - GRL - Juniper Encroachment Potential (NM)
#' - GRL - NV range seeding (Wind C = 10) (NV)
#' - GRL - NV range seeding (Wind C = 100) (NV)
#' - GRL - NV range seeding (Wind C = 20) (NV)
#' - GRL - NV range seeding (Wind C = 30) (NV)
#' - GRL - NV range seeding (Wind C = 40) (NV)
#' - GRL - NV range seeding (Wind C = 50) (NV)
#' - GRL - NV range seeding (Wind C = 60) (NV)
#' - GRL - NV range seeding (Wind C = 80) (NV)
#' - GRL - NV range seeding (Wind C >= 160) (NV)
#' - GRL - Pasture and Hayland SG (OH)
#' - GRL - Ranch Access Roads (TX)
#' - GRL - Rangeland Chaining (TX)
#' - GRL - Rangeland Disking (TX)
#' - GRL - Rangeland Dozing/Grubbing (TX)
#' - GRL - Rangeland Planting by Mechanical Seeding (TX)
#' - GRL - Rangeland Prescribed Burning (TX)
#' - GRL - Rangeland Roller Chopping (TX)
#' - GRL - Rangeland Root Plowing (TX)
#' - GRL - Utah Juniper Encroachment Potential
#' - GRL - Western Juniper Encroachment Potential (OR)
#' - Ground-based Solar Arrays, Ballast Anchor Systems
#' - Ground-based Solar Arrays, Soil-penetrating Anchor Systems
#' - Ground Penetrating Radar Penetration
#' - Hybrid Wine Grape Varieties Site Desirability (Long)
#' - Hybrid Wine Grape Varieties Site Desirability (Medium)
#' - Hybrid Wine Grape Varieties Site Desirability (Short)
#' - Inland Wetlands (CT)
#' - IRR-restrictive features for irrigation (OH)
#' - ISDH Septic Tank Interpretation (IN)
#' - Land Application of Municipal Sewage Sludge (PA)
#' - Lined Retention Systems
#' - Low Pressure Pipe Septic System (DE)
#' - MIL - Bivouac Areas (DOD)
#' - MIL - Excavations Crew-Served Weapon Fighting Position (DOD)
#' - MIL - Excavations for Individual Fighting Position (DOD)
#' - MIL - Excavations for Vehicle Fighting Position (DOD)
#' - MIL - Helicopter Landing Zones (DOD)
#' - MIL - Trafficability Veh. Type 1 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 1 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 1 dry season (DOD)
#' - MIL - Trafficability Veh. Type 2 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 2 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 2 dry season (DOD)
#' - MIL - Trafficability Veh. Type 3 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 3 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 3 dry season (DOD)
#' - MIL - Trafficability Veh. Type 4 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 4 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 4 dry season (DOD)
#' - MIL - Trafficability Veh. Type 5 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 5 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 5 dry season (DOD)
#' - MIL - Trafficability Veh. Type 6 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 6 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 6 dry season (DOD)
#' - MIL - Trafficability Veh. Type 7 1-pass wet season (DOD)
#' - MIL - Trafficability Veh. Type 7 50-passes wet season (DOD)
#' - MIL - Trafficability Veh. Type 7 dry season (DOD)
#' - MT - Conservation Tree/Shrub Groups (MT)
#' - Muscadine Wine Grape Site Desirability (Very Long)
#' - NCCPI - Irrigated National Commodity Crop Productivity Index
#' - NCCPI - National Commodity Crop Productivity Index (Ver 3.0)
#' - NCCPI - NCCPI Corn Submodel (I)
#' - NCCPI - NCCPI Cotton Submodel (II)
#' - NCCPI - NCCPI Small Grains Submodel (II)
#' - NCCPI - NCCPI Soybeans Submodel (I)
#' - Nitrogen Loss Potential (ND)
#' - Permafrost Sensitivity (AK)
#' - Pressure Dose Capping Fill Septic System (DE)
#' - Pressure Dose Full Depth Septic System (DE)
#' - REC - Camp and Picnic Areas (AK)
#' - REC - Camp Areas (CT)
#' - REC - Camp Areas; Primitive (AK)
#' - REC - Foot and ATV Trails (AK)
#' - REC - Off-Road Motorcycle Trails (CT)
#' - REC - Paths and Trails (CT)
#' - REC - Picnic Areas (CT)
#' - REC - Playgrounds (AK)
#' - REC - Playgrounds (CT)
#' - RSK-risk assessment for manure application (OH)
#' - Salinity Risk Index, Discharge Model (ND)
#' - SAS - CMECS Substrate Class
#' - SAS - CMECS Substrate Origin
#' - SAS - CMECS Substrate Subclass
#' - SAS - CMECS Substrate Subclass/Group
#' - SAS - CMECS Substrate Subclass/Group/Subgroup
#' - SAS - Eastern Oyster Habitat Restoration Suitability
#' - SAS - Eelgrass Restoration Suitability
#' - SAS - Land Utilization of Dredged Materials
#' - SAS - Mooring Anchor - Deadweight
#' - SAS - Mooring Anchor - Mushroom
#' - SAS - Northern Quahog (Hard Clam) Habitat Suitability
#' - Septic System A/B Soil System (Alternate) (PA)
#' - Septic System At-Grade Bed (Alternate) (PA)
#' - Septic System At Grade Shallow Field (alternative) (WV)
#' - Septic System CO-OP RFS III w/At-Grade Bed (PA)
#' - Septic System CO-OP RFS III w/Drip Irrigation (PA)
#' - Septic System CO-OP RFS III w/Spray Irrigation (PA)
#' - Septic System Drip Irrigation (Alternate) (PA)
#' - Septic System Drip Irrigation (alternative) (WV)
#' - Septic System Dual Field Trench (conventional) (WV)
#' - Septic System Elevated Field (alternative) (WV)
#' - Septic System Free Access Sand Filter w/At-Grade Bed (PA)
#' - Septic System Free Access Sand Filter w/Drip Irrigation (PA)
#' - Septic System Free Access Sand Filterw/Spray Irrigation (PA)
#' - Septic System In Ground Bed (conventional) (PA)
#' - Septic System In Ground Trench (conventional) (PA)
#' - Septic System In Ground Trench (conventional) (WV)
#' - Septic System Low Pressure Pipe (alternative) (WV)
#' - Septic System Modified Subsurface Sand Filter (Alt.) (PA)
#' - Septic System Mound (alternative) (WV)
#' - Septic System Peat Based Option1 (UV & At-Grade Bed)Alt (PA)
#' - Septic System Peat Based Option1 w/At-Grade Bed (Alt.) (PA)
#' - Septic System Peat Based Option2 w/Spray Irrigation (PA)
#' - Septic System Peat Sys Opt3 w/Subsurface Sand Filter (PA)
#' - Septic System Sand Mound Bed or Trench (PA)
#' - Septic System Shallow In Ground Trench (conventional) (WV)
#' - Septic System Shallow Placement Pressure Dosed (Alt.) (PA)
#' - Septic System Spray Irrigation (PA)
#' - Septic System Steep Slope Mound (alternative) (WV)
#' - Septic System Steep Slope Sand Mound (Alternate) (PA)
#' - Septic System Subsurface Sand Filter Bed (conventional) (PA)
#' - Septic System Subsurface Sand Filter Trench (standard) (PA)
#' - Shallow Infiltration Systems
#' - SOH -  Suitability for Aerobic Soil Organisms
#' - SOH - Agricultural Organic Soil Subsidence
#' - SOH - Concentration of Salts- Soil Surface
#' - SOH - Organic Matter Depletion
#' - SOH - Soil Surface Sealing
#' - SOH - Soil Susceptibility to Compaction
#' - Soil Habitat for Saprophyte Stage of Coccidioides
#' - SOIL HEALTH ASSESSMENT (NJ)
#' - Soil Vegetative Groups (CA)
#' - Surface Runoff Class (CA)
#' - Unlined Retention Systems
#' - URB - Commercial Brick Bldg; w/Reinforced Concrete Slab (TX)
#' - URB - Commercial Brick Buildings w/Concrete Slab (TX)
#' - URB - Commercial Metal Bldg; w/Concrete Slab (TX)
#' - URB - Commercial Metal Bldg; w/Reinforced Concrete Slab (TX)
#' - URB - Commercial Metal Buildings w/o Concrete Slab (TX)
#' - URB - Concrete Driveways and Sidewalks (TX)
#' - URB - Dwellings on Concrete Slab (TX)
#' - URB - Dwellings With Basements (TX)
#' - URB - Lawns and Ornamental Plantings (TX)
#' - URB - Reinforced Concrete Slab (TX)
#' - URB - Rural Residential Development on Concrete Slab (TX)
#' - URB - Rural Residential Development w/Basement (TX)
#' - URB - Urban Residential Development on Concrete Slab (TX)
#' - URB - Urban Residential Development w/Basement (TX)
#' - URB/REC - Camp Areas
#' - URB/REC - Camp Areas (GA)
#' - URB/REC - Camp Areas (HI)
#' - URB/REC - Camp Areas (MI)
#' - URB/REC - Camp Areas (OH)
#' - URB/REC - Golf Fairways (OH)
#' - URB/REC - Off-Road Motorcycle Trails
#' - URB/REC - Off-Road Motorcycle Trails (OH)
#' - URB/REC - Paths and Trails
#' - URB/REC - Paths and Trails (GA)
#' - URB/REC - Paths and Trails (MI)
#' - URB/REC - Paths and Trails (OH)
#' - URB/REC - Picnic Areas
#' - URB/REC - Picnic Areas (GA)
#' - URB/REC - Picnic Areas (MI)
#' - URB/REC - Picnic Areas (OH)
#' - URB/REC - Playgrounds
#' - URB/REC - Playgrounds (GA)
#' - URB/REC - Playgrounds (MI)
#' - URB/REC - Playgrounds (OH)
#' - Vinifera Wine Grape Site Desirability (Long to Medium)
#' - Vinifera Wine Grape Site Desirability (Long)
#' - Vinifera Wine Grape Site Desirability (Short to Medium)
#' - Vinifera Wine Grape Site Desirability (Short)
#' - WAQ - Soil Pesticide Absorbed Runoff Potential (TX)
#' - WAQ - Soil Pesticide Leaching Potential (TX)
#' - WAQ - Soil Pesticide Solution Runoff Potential (TX)
#' - WLF-Soil Suitability - Karner Blue Butterfly (WI)
#' - WLF - Burrowing Mammals & Reptiles (TX)
#' - WLF - Chufa for Turkey Forage (LA)
#' - WLF - Crawfish Aquaculture (TX)
#' - WLF - Desert Tortoise (CA)
#' - WLF - Desertic Herbaceous Plants (TX)
#' - WLF - Domestic Grasses & Legumes for Food and Cover (TX)
#' - WLF - Food Plots for Upland Wildlife < 2 Acres (TX)
#' - WLF - Freshwater Wetland Plants (TX)
#' - WLF - Gopher Tortoise Burrowing Suitability
#' - WLF - Grain & Seed Crops for Food and Cover (TX)
#' - WLF - Irr. Domestic Grasses & Legumes for Food & Cover (TX)
#' - WLF - Irrigated Freshwater Wetland Plants (TX)
#' - WLF - Irrigated Grain & Seed Crops for Food & Cover (TX)
#' - WLF - Irrigated Saline Water Wetland Plants (TX)
#' - WLF - Riparian Herbaceous Plants (TX)
#' - WLF - Riparian Shrubs, Vines, & Trees (TX)
#' - WLF - Saline Water Wetland Plants (TX)
#' - WLF - Upland Coniferous Trees (TX)
#' - WLF - Upland Deciduous Trees (TX)
#' - WLF - Upland Desertic Shrubs & Trees (TX)
#' - WLF - Upland Mixed Deciduous & Coniferous Trees (TX)
#' - WLF - Upland Native Herbaceous Plants (TX)
#' - WLF - Upland Shrubs & Vines (TX)
#' - WMS-Subsurface Water Management, Installation (ND)
#' - WMS-Subsurface Water Management, Outflow Quality (ND)
#' - WMS-Subsurface Water Management, Performance (ND)
#' - WMS - Constructing Grassed Waterways (OH)
#' - WMS - Constructing Grassed Waterways (TX)
#' - WMS - Constructing Terraces & Diversions (TX)
#' - WMS - Constructing Terraces and Diversions (OH)
#' - WMS - Drainage - (MI)
#' - WMS - Drainage (IL)
#' - WMS - Drainage (OH)
#' - WMS - Embankments, Dikes, and Levees
#' - WMS - Embankments, Dikes, and Levees (OH)
#' - WMS - Embankments, Dikes, and Levees (VT)
#' - WMS - Excavated Ponds (Aquifer-fed)
#' - WMS - Excavated Ponds (Aquifer-fed) (OH)
#' - WMS - Excavated Ponds (Aquifer-fed) (VT)
#' - WMS - Grape Production with Drip Irrigation (TX)
#' - WMS - Grassed Waterways - (MI)
#' - WMS - Irrigation, General
#' - WMS - Irrigation, Micro (above ground)
#' - WMS - Irrigation, Micro (above ground) (VT)
#' - WMS - Irrigation, Micro (subsurface drip)
#' - WMS - Irrigation, Micro (subsurface drip) (VT)
#' - WMS - Irrigation, Sprinkler (close spaced outlet drops)
#' - WMS - Irrigation, Sprinkler (general)
#' - WMS - Irrigation, Sprinkler (general) (VT)
#' - WMS - Irrigation, Surface (graded)
#' - WMS - Irrigation, Surface (level)
#' - WMS - Pond Reservoir Area
#' - WMS - Pond Reservoir Area (GA)
#' - WMS - Pond Reservoir Area (MI)
#' - WMS - Pond Reservoir Area (OH)
#' - WMS - Sprinkler Irrigation (MT)
#' - WMS - Sprinkler Irrigation RDC (IL)
#' - WMS - Subsurface Drains - Installation (VT)
#' - WMS - Subsurface Drains - Performance (VT)
#' - WMS - Subsurface Drains < 3 Feet Deep (TX)
#' - WMS - Subsurface Drains > 3 Feet Deep (TX)
#' - WMS - Subsurface Water Management, Outflow Quality
#' - WMS - Subsurface Water Management, System Installation
#' - WMS - Subsurface Water Management, System Performance
#' - WMS - Surface Drains (TX)
#' - WMS - Surface Irrigation Intake Family (TX)
#' - WMS - Surface Water Management, System
#'
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
#' @importFrom soilDB format_SQL_in_statement SDA_query
#'
get_SDA_interpretation <- function(rulename,
                                   method = c("Dominant Component",
                                              "Dominant Condition",
                                              "Weighted Average",
                                              "None"),
                                   areasymbols = NULL,
                                   mukeys = NULL, 
                                   query_string = FALSE,
                                   not_rated_value = NA_real_) {
  q <- .constructInterpQuery(
      method = method,
      interp = rulename,
      areasymbols = areasymbols,
      mukeys = mukeys
    )

  if (query_string) return(q)

  # execute query
  res <- suppressMessages(soilDB::SDA_query(q))

  # stop if bad
  if (inherits(res, 'try-error')) {
    warnings()
    stop(attr(res, 'condition'))
  }
  
  # check rating column values
  ratingcols <- colnames(res)[grep("^rating_", colnames(res))]
  res[] <- lapply(colnames(res), function(x) {
    y <- res[[x]]
    if(x %in% ratingcols) {
      # SQL will set 99 rating value for class == "Not rated"
      y[is.na(y) | y == 99] <- not_rated_value
      return(y)
    }
    y
  })
  return(res)
}

.cointerpRuleNames <- function() {
  # dput(soilDB::SDA_query("select distinct mrulename from cointerp"))
  c("AGR - Barley Yield (MT)", "AGR - Conventional Tillage (TX)",
    "AGR - Grape non-irrigated (MO)", "AGR - Nitrate Leaching Potential, Irrigated (WA)",
    "AGR - No Till, Tile Drained (TX)", "AGR - Pasture hayland (MO)",
    "AGR - Pesticide Loss Potential-Soil Surface Runoff", "AGR - Plant Growth Index PGI no Climate Adj. (TX)",
    "AGR - Spring Wheat Yield (MT)", "AGR - Water Erosion Potential Wide Ratings Array (TX)",
    "AGR - Wind Erosion Potential Wide Ratings Array (TX)", "AGR-Agronomic Concerns (ND)",
    "AGR-Pesticide and Nutrient Leaching Potential, NIRR (ND)", "AGR-Surface Salinity (ND)",
    "Alaska Exempt Wetland Potential (AK)", "American Wine Grape Varieties Site Desirability (Short)",
    "AWM - Irrigation Disposal of Wastewater (MD)", "AWM - Manure and Food Processing Waste (DE)",
    "AWM - Manure Stacking - Site Evaluation (TX)", "AWM - Phosphorus Management (TX)",
    "AWM - Slow Rate Process Treatment of Wastewater", "BLM - Pygmy Rabbit Habitat Potential",
    "BLM - Rangeland Tillage", "BLM - Site Degradation Susceptibility",
    "CA Prime Farmland (CA)", "CLASS RULE - Depth to root limiting layer (5 classes) (NPS)",
    "CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)",
    "CPI - Barley, NIRR - Eastern Idaho Plateaus (ID)", "CPI - Grass Hay, IRR - Eastern Idaho Plateaus (ID)",
    "CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)",
    "CPI - Potatoes, IRR - Snake River Plains (ID)", "CPI - Small Grains, NIRR - Palouse Prairies (OR)",
    "CPI - Small Grains, NIRR - Palouse Prairies (WA)", "CPI - Small Grains, NIRR - Snake River Plains (ID)",
    "CPI - Wheat, NIRR - Eastern Idaho Plateaus (ID)", "CPI - Wild Hay, NIRR - Eastern Idaho Plateaus (ID)",
    "CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (ID)",
    "CPI - Wild Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)",
    "Deep Infiltration Systems", "DHS - Site for Composting Facility - Surface",
    "Elevated Sand Mound Septic System (DE)", "ENG - Animal Disposal by Composting (Catastrophic) (WV)",
    "ENG - Application of Municipal Sludge (TX)", "ENG - Closed-Loop Horizontal Geothermal Heat Pump (CT)",
    "ENG - Construction Materials; Gravel Source (IN)", "ENG - Construction Materials; Gravel Source (NE)",
    "ENG - Construction Materials; Reclamation (MD)", "ENG - Construction Materials; Reclamation (MI)",
    "ENG - Construction Materials; Roadfill (GA)", "ENG - Construction Materials; Sand Source (CT)",
    "ENG - Construction Materials; Sand Source (GA)", "ENG - Construction Materials; Topsoil (ID)",
    "ENG - Construction Materials; Topsoil (OH)", "ENG - Daily Cover for Landfill (OH)",
    "ENG - Disposal Field (NJ)", "ENG - Disposal Field Type Inst (NJ)",
    "ENG - Dwellings W/O Basements", "ENG - Dwellings With Basements",
    "ENG - Dwellings without Basements (AK)", "ENG - Lawn and Landscape (OH)",
    "ENG - Lawn, Landscape, Golf Fairway", "ENG - Local Roads and Streets (AK)",
    "ENG - Local Roads and Streets (GA)", "ENG - On-Site Waste Water Lagoons (MO)",
    "ENG - OSHA Soil Types (TX)", "ENG - Pier Beam Building Foundations (TX)",
    "ENG - Sanitary Landfill (Area)", "ENG - Sanitary Landfill (Area) (AK)",
    "ENG - Septage Application - Incorporation or Injection (MN)",
    "ENG - Septic System; Disinfection, Surface Application (TX)",
    "ENG - Septic Tank Absorption Fields (FL)", "ENG - Septic Tank Absorption Fields (OH)",
    "ENG - Septic Tank Absorption Fields - Trench (MN)", "ENG - Sewage Lagoons (AK)",
    "ENG - Shallow Excavations (OH)", "ENG - Stormwater Management / Infiltration (NY)",
    "ENG - Stormwater Management / Wetlands (NY)", "FOR - Construction Limitations for Haul Roads/Log Landings",
    "FOR - Displacement Hazard", "FOR - Harvest Equipment Operability (DE)",
    "FOR - Harvest Equipment Operability (ME)", "FOR - Harvest Equipment Operability (MI)",
    "FOR - Log Landing Suitability (ID)", "FOR - Log Landing Suitability (MI)",
    "FOR - Log Landing Suitability (OR)", "FOR - Mechanical Planting Suitability (OH)",
    "FOR - Mechanical Site Preparation (Surface) (MD)", "FOR - Mechanical Site Preparation (Surface) (OH)",
    "FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (MI)", "FOR - Potential Erosion Hazard (Off-Road/Off-Trail) (OH)",
    "FOR - Potential Seedling Mortality (FL)", "FOR - Potential Seedling Mortality (OH)",
    "FOR - Road Suitability (Natural Surface) (VT)", "FOR - Soil Rutting Hazard",
    "FOTG - Indiana Soy Bean Yield Calculation (IN)", "FOTG - Indiana Wheat Yield Calculation (IN)",
    "GRL - Fencing, Post Depth =<24 inches", "GRL - Fencing, Post Depth Less Than 24 inches (TX)",
    "GRL - Fencing, Post Depth Less Than 36 inches (TX)", "GRL - NV range seeding (Wind C = 10) (NV)",
    "GRL - NV range seeding (Wind C = 30) (NV)", "GRL - Rangeland Chaining (TX)",
    "GRL - Rangeland Disking (TX)", "GRL - Rangeland Dozing/Grubbing (TX)",
    "GRL - Utah Juniper Encroachment Potential", "GRL - Western Juniper Encroachment Potential (OR)",
    "Hybrid Wine Grape Varieties Site Desirability (Medium)", "Lined Retention Systems",
    "MIL - Trafficability Veh. Type 1 dry season (DOD)", "MIL - Trafficability Veh. Type 3 1-pass wet season (DOD)",
    "MIL - Trafficability Veh. Type 3 dry season (DOD)", "MIL - Trafficability Veh. Type 4 dry season (DOD)",
    "MIL - Trafficability Veh. Type 5 1-pass wet season (DOD)", "NCCPI - NCCPI Corn Submodel (I)",
    "NCCPI - NCCPI Small Grains Submodel (II)", "NCCPI - NCCPI Soybeans Submodel (I)",
    "Pressure Dose Full Depth Septic System (DE)", "REC - Camp Areas; Primitive (AK)",
    "REC - Paths and Trails (CT)", "SAS - Eastern Oyster Habitat Restoration Suitability",
    "SAS - Mooring Anchor - Mushroom", "Septic System CO-OP RFS III w/At-Grade Bed (PA)",
    "Septic System Free Access Sand Filter w/At-Grade Bed (PA)",
    "Septic System Modified Subsurface Sand Filter (Alt.) (PA)",
    "Septic System Shallow In Ground Trench (conventional) (WV)",
    "Septic System Subsurface Sand Filter Bed (conventional) (PA)",
    "Septic System Subsurface Sand Filter Trench (standard) (PA)",
    "URB - Commercial Brick Buildings w/Concrete Slab (TX)", "URB - Commercial Metal Bldg; w/Concrete Slab (TX)",
    "URB - Concrete Driveways and Sidewalks (TX)", "URB - Dwellings on Concrete Slab (TX)",
    "URB - Dwellings With Basements (TX)", "URB - Lawns and Ornamental Plantings (TX)",
    "URB - Rural Residential Development w/Basement (TX)", "URB - Urban Residential Development w/Basement (TX)",
    "URB/REC - Paths and Trails", "URB/REC - Paths and Trails (GA)",
    "URB/REC - Playgrounds (MI)", "Vinifera Wine Grape Site Desirability (Long)",
    "WAQ - Soil Pesticide Leaching Potential (TX)", "WLF - Crawfish Aquaculture (TX)",
    "WLF - Desertic Herbaceous Plants (TX)", "WLF - Gopher Tortoise Burrowing Suitability",
    "WLF - Grain & Seed Crops for Food and Cover (TX)", "WMS - Constructing Grassed Waterways (OH)",
    "WMS - Constructing Terraces & Diversions (TX)", "WMS - Drainage (OH)",
    "AGR - Avocado Root Rot Hazard (CA)", "AGR - California Revised Storie Index (CA)",
    "AGR - Hops Site Suitability (WA)", "AGR - Map Unit Cropland Productivity (MN)",
    "AGR - Nitrate Leaching Potential, Nonirrigated (WA)", "AGR - No Till (TX)",
    "AGR - Pesticide Loss Potential-Soil Surface Runoff (NE)", "AGR - Plant Growth Index PGI with Climate Adj. (TX)",
    "AGR - Plant Growth Index PGI with Climate Adj. MAP,MAAT (TX)",
    "AGR - Ridge Till (TX)", "AGR - Selenium Leaching Potential (CO)",
    "AGR - Wind Erosion Potential (TX)", "AGR - Winter Wheat Yield (MT)",
    "AGR-Pesticide and Nutrient Runoff Potential (ND)", "AGR-Rooting Depth (ND)",
    "American Wine Grape Varieties Site Desirability (Long)", "American Wine Grape Varieties Site Desirability (Medium)",
    "American Wine Grape Varieties Site Desirability (Very Long)",
    "AWM - Animal Mortality Disposal (Catastrophic) (MO)", "AWM - Irrigation Disposal of Wastewater (OH)",
    "AWM - Irrigation Disposal of Wastewater (VT)", "AWM - Land Application of Municipal Biosolids, summer (OR)",
    "AWM - Manure and Food Processing Waste (MD)", "AWM - Manure and Food Processing Waste (OH)",
    "AWM - Overland Flow Process Treatment of Wastewater (VT)", "AWM - Rapid Infil Disposal of Wastewater (DE)",
    "AWM - Sensitive Soil Features (MN)", "BLM - Fencing", "BLM - Fire Damage Susceptibility",
    "BLM - Mechanical Treatment, Rolling Drum", "BLM - Rangeland Drill",
    "BLM - Rangeland Seeding, Colorado Plateau Ecoregion", "BLM - Rangeland Seeding, Great Basin Ecoregion",
    "BLM-Reclamation Suitability (MT)", "CLASS RULE - Depth to lithic bedrock (5 classes) (NPS)",
    "CLASS RULE - Soil Inorganic Carbon kg/m2 to 2m (NPS)", "CLASS RULE - Soil Organic Carbon kg/m2 to 2m (NPS)",
    "CLR-pastureland limitation (IN)", "CPI - Alfalfa Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)",
    "CPI - Barley, IRR - Eastern Idaho Plateaus (ID)", "CPI - Grass Hay, IRR - Klamath Valleys and Basins (OR)",
    "CPI - Small Grains, IRR - Snake River Plains (ID)", "CPI - Wheat, IRR - Eastern Idaho Plateaus (ID)",
    "DHS - Catastrophic Event, Large Animal Mortality, Burial", "DHS - Catastrophic Mortality, Large Animal Disposal, Pit",
    "DHS - Catastrophic Mortality, Large Animal Disposal, Trench",
    "DHS - Potential for Radioactive Bioaccumulation", "DHS - Potential for Radioactive Sequestration",
    "DHS - Suitability for Composting Medium and Final Cover", "ENG - Construction Materials; Gravel Source",
    "ENG - Construction Materials; Gravel Source (AK)", "ENG - Construction Materials; Gravel Source (ID)",
    "ENG - Construction Materials; Gravel Source (OH)", "ENG - Construction Materials; Gravel Source (VT)",
    "ENG - Construction Materials; Gravel Source (WA)", "ENG - Construction Materials; Roadfill (OH)",
    "ENG - Construction Materials; Sand Source (OR)", "ENG - Construction Materials; Sand Source (WA)",
    "ENG - Construction Materials; Topsoil (GA)", "ENG - Construction Materials; Topsoil (MD)",
    "ENG - Daily Cover for Landfill", "ENG - Daily Cover for Landfill (AK)",
    "ENG - Disposal Field Suitability Class (NJ)", "ENG - Dwellings W/O Basements (OH)",
    "ENG - Dwellings with Basements (AK)", "ENG - Large Animal Disposal, Pit (CT)",
    "ENG - Lawn, landscape, golf fairway (CT)", "ENG - Local Roads and Streets (OH)",
    "ENG - On-Site Waste Water Absorption Fields (MO)", "ENG - Septic Tank Absorption Fields",
    "ENG - Septic Tank Absorption Fields (MD)", "ENG - Septic Tank Absorption Fields (TX)",
    "ENG - Septic Tank, Gravity Disposal (TX)", "ENG - Sewage Lagoons",
    "ENG - Small Commercial Buildings (OH)", "ENG - Soil Potential Ratings of SSDS (CT)",
    "FOR (USFS) - Road Construction/Maintenance (Natural Surface)",
    "FOR - Compaction Potential (WA)", "FOR - Damage by Fire (OH)",
    "FOR - General Harvest Season (VT)", "FOR - Hand Planting Suitability",
    "FOR - Hand Planting Suitability, MO13 (DE)", "FOR - Hand Planting Suitability, MO13 (MD)",
    "FOR - Log Landing Suitability", "FOR - Log Landing Suitability (ME)",
    "FOR - Log Landing Suitability (VT)", "FOR - Log Landing Suitability (WA)",
    "FOR - Mechanical Planting Suitability (CT)", "FOR - Mechanical Planting Suitability, MO13 (MD)",
    "FOR - Mechanical Site Preparation (Deep)", "FOR - Mechanical Site Preparation (Deep) (DE)",
    "FOR - Mechanical Site Preparation (Surface) (DE)", "FOR - Mechanical Site Preparation (Surface) (MI)",
    "FOR - Mechanical Site Preparation; Surface (ME)", "FOR - Potential Erosion Hazard, Road/Trail, Spring Thaw (AK)",
    "FOR - Potential Seedling Mortality (PIA)", "FOR - Potential Seedling Mortality(ME)",
    "FOR - Puddling Hazard", "FOR - Road Suitability (Natural Surface) (ME)",
    "FOR - Road Suitability (Natural Surface) (WA)", "FOR - Soil Rutting Hazard (OH)",
    "FOR - Soil Sustainability Forest Biomass Harvesting (CT)", "FOR - White Oak Suitability (MO)",
    "FOR-Biomass Harvest (WI)", "FOTG - Indiana Corn Yield Calculation (IN)",
    "GRL - Excavations to 24 inches for Plastic Pipelines (TX)",
    "GRL - Fencing, 24 inch Post Depth (MT)", "GRL - NV range seeding (Wind C = 100) (NV)",
    "GRL - NV range seeding (Wind C = 40) (NV)", "GRL - NV range seeding (Wind C = 60) (NV)",
    "GRL - NV range seeding (Wind C = 80) (NV)", "GRL - NV range seeding (Wind C >= 160) (NV)",
    "GRL - Rangeland Planting by Mechanical Seeding (TX)", "GRL - Rangeland Root Plowing (TX)",
    "Hybrid Wine Grape Varieties Site Desirability (Long)", "Low Pressure Pipe Septic System (DE)",
    "MIL - Bivouac Areas (DOD)", "MIL - Excavations Crew-Served Weapon Fighting Position (DOD)",
    "MIL - Excavations for Individual Fighting Position (DOD)", "MIL - Trafficability Veh. Type 1 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 2 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 4 1-pass wet season (DOD)", "MIL - Trafficability Veh. Type 4 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 6 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 7 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 7 dry season (DOD)", "MT - Conservation Tree/Shrub Groups (MT)",
    "NCCPI - Irrigated National Commodity Crop Productivity Index",
    "Nitrogen Loss Potential (ND)", "REC - Foot and ATV Trails (AK)",
    "REC - Playgrounds (AK)", "RSK-risk assessment for manure application (OH)",
    "SAS - CMECS Substrate Origin", "SAS - CMECS Substrate Subclass/Group/Subgroup",
    "SAS - Mooring Anchor - Deadweight", "Septic System A/B Soil System (Alternate) (PA)",
    "Septic System CO-OP RFS III w/Spray Irrigation (PA)", "Septic System Dual Field Trench (conventional) (WV)",
    "Septic System Elevated Field (alternative) (WV)", "Septic System In Ground Trench (conventional) (PA)",
    "Septic System In Ground Trench (conventional) (WV)", "Septic System Low Pressure Pipe (alternative) (WV)",
    "Septic System Mound (alternative) (WV)", "Septic System Peat Based Option2 w/Spray Irrigation (PA)",
    "Septic System Steep Slope Mound (alternative) (WV)", "WMS - Excavated Ponds (Aquifer-fed) (OH)",
    "WMS - Grape Production with Drip Irrigation (TX)", "WMS - Irrigation, Micro (subsurface drip) (VT)",
    "WMS - Irrigation, Surface (level)", "WMS - Pond Reservoir Area (MI)",
    "WMS - Sprinkler Irrigation (MT)", "WMS - Sprinkler Irrigation RDC (IL)",
    "WMS - Subsurface Drains - Performance (VT)", "WMS - Subsurface Water Management, Outflow Quality",
    "WMS - Surface Water Management, System", "WMS-Subsurface Water Management, Performance (ND)",
    "AGR - Air Quality; PM10 (TX)", "AGR - Air Quality; PM2_5 (TX)",
    "AGR - Index for alfalfa hay, irrigated (NV)", "AGR - Nitrate Leaching Potential, Nonirrigated (MA)",
    "AGR - Orchard Groups (TX)", "AGR - Rangeland Grass/Herbaceous Productivity Index (TX)",
    "AGR - Rutting Hazard > 10,000 Pounds per Wheel (TX)", "AGR - Water Erosion Potential (TX)",
    "AGR - Wine Grape Site Suitability (WA)", "AGR-Natural Fertility (ND)",
    "AGR-Subsurface Salinity (ND)", "AGR-Water Erosion (ND)", "AWM - Filter Group (OH)",
    "AWM - Irrigation Disposal of Wastewater", "AWM - Land Application of Dry and Slurry Manure (TX)",
    "AWM - Land Application of Municipal Biosolids, winter (OR)",
    "AWM - Overland Flow Process Treatment of Wastewater", "AWM - Rapid Infiltration Disposal of Wastewater",
    "AWM - Vegetated Treatment Area (PIA)", "AWM - Waste Field Storage Area (VT)",
    "BLM - Mechanical Treatment, Shredder", "BLM - Medusahead Invasion Susceptibility",
    "BLM - Soil Compaction Resistance", "Capping Fill Gravity Septic System (DE)",
    "CLASS RULE - Depth to any bedrock kind (5 classes) (NPS)", "CPI - Alfalfa Hay, IRR - Eastern Idaho Plateaus (ID)",
    "CPI - Alfalfa Hay, IRR - Klamath Valley and Basins (OR)", "CPI - Alfalfa Hay, IRR - Snake River Plains (ID)",
    "CPI - Alfalfa Hay, NIRR- Eastern Idaho Plateaus (ID)", "CPI - Grass Hay, NIRR - Palouse, Northern Rocky Mtns. (WA)",
    "CPI - Small Grains Productivity Index (AK)", "DHS - Catastrophic Event, Large Animal Mortality, Incinerate",
    "DHS - Emergency Land Disposal of Milk", "DHS - Site for Composting Facility - Subsurface",
    "DHS - Suitability for Clay Liner Material", "ENG - Cohesive Soil Liner (MN)",
    "ENG - Construction Materials - Sand Source (MN)", "ENG - Construction Materials; Gravel Source (CT)",
    "ENG - Construction Materials; Gravel Source (NY)", "ENG - Construction Materials; Reclamation (DE)",
    "ENG - Construction Materials; Roadfill", "ENG - Construction Materials; Roadfill (AK)",
    "ENG - Construction Materials; Sand Source (NY)", "ENG - Construction Materials; Sand Source (VT)",
    "ENG - Construction Materials; Topsoil (AK)", "ENG - Construction Materials; Topsoil (DE)",
    "ENG - Construction Materials; Topsoil (MI)", "ENG - Construction Materials; Topsoil (OR)",
    "ENG - Disposal Field Gravity (DE)", "ENG - Dwellings With Basements (OH)",
    "ENG - Large Animal Disposal, Trench (CT)", "ENG - Lawn, Landscape, Golf Fairway (MI)",
    "ENG - Lawn, Landscape, Golf Fairway (VT)", "ENG - Sanitary Landfill (Area) (OH)",
    "ENG - Sanitary Landfill (Trench)", "ENG - Sanitary Landfill (Trench) (AK)",
    "ENG - Septage Application - Surface (MN)", "ENG - Septic Tank Absorption Fields - At-Grade (MN)",
    "ENG - Septic Tank Absorption Fields - Mound (MN)", "ENG - Septic Tank Leaching Chamber (TX)",
    "ENG - Septic Tank, Subsurface Drip Irrigation (TX)", "ENG - Shallow Excavations",
    "ENG - Small Commercial Buildings", "ENG - Soil Potential of Road Salt Applications (CT)",
    "ENG - Source of Caliche (TX)", "ENG - Stormwater Management / Ponds (NY)",
    "Farm and Garden Composting Facility - Surface", "FOR - Biomass Harvest (MA)",
    "FOR - Black Walnut Suitability Index (KS)", "FOR - Displacement Potential (WA)",
    "FOR - General Harvest Season (ME)", "FOR - Harvest Equipment Operability",
    "FOR - Mechanical Site Preparation (Deep) (MD)", "FOR - Mechanical Site Preparation (Surface)",
    "FOR - Mechanical Site Preparation; Deep (CT)", "FOR - Potential Erosion Hazard (Road/Trail)",
    "FOR - Potential Fire Damage Hazard", "FOR - Potential Seedling Mortality",
    "FOR - Potential Seedling Mortality (MI)", "FOR - Potential Windthrow Hazard (ME)",
    "FOR - Potential Windthrow Hazard (MI)", "FOR - Road Suitability (Natural Surface) (ID)",
    "FOR - Rutting Hazard by Month", "FOR - Windthrow Hazard (WA)",
    "Fragile Soil Index", "GRL - Juniper Encroachment Potential (NM)",
    "GRL - NV range seeding (Wind C = 20) (NV)", "GRL - Pasture and Hayland SG (OH)",
    "GRL - Rangeland Prescribed Burning (TX)", "GRL-FSG-NP-W (MT)",
    "Ground-based Solar Arrays, Ballast Anchor Systems", "Inland Wetlands (CT)",
    "IRR-restrictive features for irrigation (OH)", "MIL - Excavations for Vehicle Fighting Position (DOD)",
    "MIL - Trafficability Veh. Type 1 1-pass wet season (DOD)", "MIL - Trafficability Veh. Type 2 dry season (DOD)",
    "MIL - Trafficability Veh. Type 3 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 6 1-pass wet season (DOD)", "MIL - Trafficability Veh. Type 6 dry season (DOD)",
    "Muscadine Wine Grape Site Desirability (Very Long)", "NCCPI - NCCPI Cotton Submodel (II)",
    "Permafrost Sensitivity (AK)", "Pressure Dose Capping Fill Septic System (DE)",
    "REC - Camp Areas (CT)", "REC - Off-Road Motorcycle Trails (CT)",
    "SAS - CMECS Substrate Class", "SAS - CMECS Substrate Subclass/Group",
    "SAS - Eelgrass Restoration Suitability", "SAS - Land Utilization of Dredged Materials",
    "SAS - Northern Quahog (Hard Clam) Habitat Suitability", "Septic System At Grade Shallow Field (alternative) (WV)",
    "Septic System At-Grade Bed (Alternate) (PA)", "Septic System CO-OP RFS III w/Drip Irrigation (PA)",
    "Septic System Drip Irrigation (alternative) (WV)", "Septic System Free Access Sand Filterw/Spray Irrigation (PA)",
    "Septic System Peat Based Option1 w/At-Grade Bed (Alt.) (PA)",
    "Septic System Spray Irrigation (PA)", "Septic System Steep Slope Sand Mound (Alternate) (PA)",
    "Shallow Infiltration Systems", "SOH - Soil Surface Sealing",
    "URB - Rural Residential Development on Concrete Slab (TX)",
    "URB/REC - Camp Areas (GA)", "URB/REC - Camp Areas (MI)", "URB/REC - Golf Fairways (OH)",
    "URB/REC - Off-Road Motorcycle Trails", "URB/REC - Paths and Trails (MI)",
    "URB/REC - Playgrounds (OH)", "Vinifera Wine Grape Site Desirability (Long to Medium)",
    "WAQ - Soil Pesticide Absorbed Runoff Potential (TX)", "WLF - Chufa for Turkey Forage (LA)",
    "WLF - Food Plots for Upland Wildlife < 2 Acres (TX)", "WLF - Freshwater Wetland Plants (TX)",
    "WLF - Irrigated Saline Water Wetland Plants (TX)", "WLF - Riparian Herbaceous Plants (TX)",
    "WLF - Riparian Shrubs, Vines, & Trees (TX)", "WLF - Saline Water Wetland Plants (TX)",
    "WLF - Upland Mixed Deciduous & Coniferous Trees (TX)", "WMS - Constructing Grassed Waterways (TX)",
    "WMS - Constructing Terraces and Diversions (OH)", "WMS - Embankments, Dikes, and Levees (VT)",
    "WMS - Irrigation, Sprinkler (close spaced outlet drops)", "WMS - Irrigation, Surface (graded)",
    "WMS - Subsurface Drains - Installation (VT)", "WMS - Subsurface Drains < 3 Feet Deep (TX)",
    "WMS - Subsurface Drains > 3 Feet Deep (TX)", "WMS - Subsurface Water Management, System Performance",
    "WMS - Surface Drains (TX)", "WMS - Surface Irrigation Intake Family (TX)",
    "SOH - Concentration of Salts- Soil Surface", "SOH - Soil Susceptibility to Compaction",
    "Soil Habitat for Saprophyte Stage of Coccidioides", "Soil Vegetative Groups (CA)",
    "Unlined Retention Systems", "URB - Commercial Metal Bldg; w/Reinforced Concrete Slab (TX)",
    "URB - Commercial Metal Buildings w/o Concrete Slab (TX)", "URB - Urban Residential Development on Concrete Slab (TX)",
    "URB/REC - Picnic Areas (GA)", "URB/REC - Picnic Areas (MI)",
    "URB/REC - Picnic Areas (OH)", "Vinifera Wine Grape Site Desirability (Short)",
    "WAQ - Soil Pesticide Solution Runoff Potential (TX)", "WLF - Burrowing Mammals & Reptiles (TX)",
    "WLF - Desert Tortoise (CA)", "WLF - Domestic Grasses & Legumes for Food and Cover (TX)",
    "WLF - Irrigated Grain & Seed Crops for Food & Cover (TX)", "WMS - Excavated Ponds (Aquifer-fed)",
    "WMS - Excavated Ponds (Aquifer-fed) (VT)", "WMS - Irrigation, General",
    "WMS - Irrigation, Micro (above ground)", "WMS - Irrigation, Micro (above ground) (VT)",
    "WMS - Irrigation, Micro (subsurface drip)", "WMS - Irrigation, Sprinkler (general) (VT)",
    "WMS - Pond Reservoir Area", "WMS - Pond Reservoir Area (OH)",
    "WMS - Subsurface Water Management, System Installation", "AGR - Filter Strips (TX)",
    "AGR - Mulch Till (TX)", "AGR - Nitrate Leaching Potential, Nonirrigated (MT)",
    "AGR - No Till (VT)", "AGR - Oats Yield (MT)", "AGR - Pesticide Loss Potential-Leaching",
    "AGR - Pesticide Loss Potential-Leaching (NE)", "AGR - Rutting Hazard =< 10,000 Pounds per Wheel (TX)",
    "AGR-Available Water Capacity (ND)", "AGR-Physical Limitations (ND)",
    "AGR-Sodicity (ND)", "AGR-Surface Crusting (ND)", "AGR-Wind Erosion (ND)",
    "AWM - Irrigation Disposal of Wastewater (DE)", "AWM - Land App of Municipal Sewage Sludge (DE)",
    "AWM - Land App of Municipal Sewage Sludge (MD)", "AWM - Land Application of Milk (CT)",
    "AWM - Land Application of Municipal Biosolids, spring (OR)",
    "AWM - Land Application of Municipal Sewage Sludge", "AWM - Land Application of Municipal Sewage Sludge (OH)",
    "AWM - Land Application of Municipal Sewage Sludge (VT)", "AWM - Large Animal Disposal, Pit (MN)",
    "AWM - Manure and Food Processing Waste", "AWM - Manure and Food Processing Waste (VT)",
    "AWM - Rapid Infil Disposal of Wastewater (MD)", "AWM - Rapid Infiltration Disposal of Wastewater (VT)",
    "AWM - Slow Rate Process Treatment of Wastewater (VT)", "BLM - Chaining Suitability",
    "BLM - Fugitive Dust Resistance", "BLM - Soil Restoration Potential",
    "BLM - Yellow Star-thistle Invasion Susceptibility", "CLASS RULE - Depth to non-lithic bedrock (5 classes) (NPS)",
    "CLR-cropland limitation for corn and soybeans (IN)", "Commodity Crop Productivity Index (Corn) (WI)",
    "CPI - Grass Hay, NIRR - Klamath Valleys and Basins (OR)", "CPI - Potatoes, IRR - Eastern Idaho Plateaus (ID)",
    "CPI - Small Grains, NIRR - Palouse Prairies (ID)", "DHS - Emergency Animal Mortality Disposal by Shallow Burial",
    "DHS - Rubble and Debris Disposal, Large-Scale Event", "ENG - Aquifer Assessment - 7081 (MN)",
    "ENG - Construction Materials - Gravel Source (MN)", "ENG - Construction Materials; Gravel Source (MI)",
    "ENG - Construction Materials; Gravel Source (OR)", "ENG - Construction Materials; Reclamation",
    "ENG - Construction Materials; Reclamation (OH)", "ENG - Construction Materials; Sand Source",
    "ENG - Construction Materials; Sand Source (AK)", "ENG - Construction Materials; Sand Source (ID)",
    "ENG - Construction Materials; Sand Source (IN)", "ENG - Construction Materials; Sand Source (OH)",
    "ENG - Construction Materials; Topsoil", "ENG - Construction Materials; Topsoil (WA)",
    "ENG - Local Roads and Streets", "ENG - New Ohio Septic Rating (OH)",
    "ENG - Sanitary Landfill (Trench) (OH)", "ENG - Septic Tank Absorption Fields (AK)",
    "ENG - Septic Tank Absorption Fields (DE)", "ENG - Septic Tank Absorption Fields (NY)",
    "ENG - Sewage Lagoons (OH)", "ENG - Shallow Excavations (AK)",
    "ENG - Shallow Excavations (MI)", "ENG - Unpaved Local Roads and Streets",
    "FOR - Black Walnut Suitability Index (MO)", "FOR - Construction Limitations - Haul Roads/Log Landing (OH)",
    "FOR - Construction Limitations For Haul Roads (MI)", "FOR - Hand Planting Suitability (ME)",
    "FOR - Harvest Equipment Operability (MD)", "FOR - Harvest Equipment Operability (OH)",
    "FOR - Harvest Equipment Operability (VT)", "FOR - Mechanical Planting Suitability",
    "FOR - Mechanical Planting Suitability (ME)", "FOR - Mechanical Planting Suitability, MO13 (DE)",
    "FOR - Potential Erosion Hazard (Off-Road/Off-Trail)", "FOR - Potential Erosion Hazard (Road/Trail) (PIA)",
    "FOR - Potential Seedling Mortality (VT)", "FOR - Potential Windthrow Hazard (NY)",
    "FOR - Potential Windthrow Hazard (VT)", "FOR - Puddling Potential (WA)",
    "FOR - Road Suitability (Natural Surface)", "FOR - Road Suitability (Natural Surface) (OH)",
    "FOR - Road Suitability (Natural Surface) (OR)", "FOR - Rutting Hazard by Season",
    "FOR - Shortleaf pine littleleaf disease susceptibility", "FOR - Soil Compactibility Risk",
    "FOR - Soil Rutting Hazard (ME)", "FOR - Windthrow Hazard", "FOR-Construction Limitations for Haul Roads/Log Landings(ME)",
    "FOTG - Indiana Slippage Potential (IN)", "Gravity Full Depth Septic System (DE)",
    "GRL - Fencing, Post Depth =<36 inches", "GRL - NV range seeding (Wind C = 50) (NV)",
    "GRL - Ranch Access Roads (TX)", "GRL - Rangeland Roller Chopping (TX)",
    "Ground Penetrating Radar Penetration", "Ground-based Solar Arrays, Soil-penetrating Anchor Systems",
    "Hybrid Wine Grape Varieties Site Desirability (Short)", "ISDH Septic Tank Interpretation (IN)",
    "Land Application of Municipal Sewage Sludge (PA)", "MIL - Helicopter Landing Zones (DOD)",
    "MIL - Trafficability Veh. Type 2 1-pass wet season (DOD)", "MIL - Trafficability Veh. Type 5 50-passes wet season (DOD)",
    "MIL - Trafficability Veh. Type 5 dry season (DOD)", "MIL - Trafficability Veh. Type 7 1-pass wet season (DOD)",
    "NCCPI - National Commodity Crop Productivity Index (Ver 3.0)",
    "REC - Camp and Picnic Areas (AK)", "REC - Picnic Areas (CT)",
    "REC - Playgrounds (CT)", "Salinity Risk Index, Discharge Model (ND)",
    "SAS - CMECS Substrate Subclass", "Septic System Drip Irrigation (Alternate) (PA)",
    "Septic System Free Access Sand Filter w/Drip Irrigation (PA)",
    "Septic System In Ground Bed (conventional) (PA)", "Septic System Peat Based Option1 (UV & At-Grade Bed)Alt (PA)",
    "Septic System Peat Sys Opt3 w/Subsurface Sand Filter (PA)",
    "Septic System Sand Mound Bed or Trench (PA)", "Septic System Shallow Placement Pressure Dosed (Alt.) (PA)",
    "SOH -  Suitability for Aerobic Soil Organisms", "SOH - Agricultural Organic Soil Subsidence",
    "SOH - Organic Matter Depletion", "SOIL HEALTH ASSESSMENT (NJ)",
    "Surface Runoff Class (CA)", "URB - Commercial Brick Bldg; w/Reinforced Concrete Slab (TX)",
    "URB - Reinforced Concrete Slab (TX)", "URB/REC - Camp Areas",
    "URB/REC - Camp Areas (HI)", "URB/REC - Camp Areas (OH)", "URB/REC - Off-Road Motorcycle Trails (OH)",
    "URB/REC - Paths and Trails (OH)", "URB/REC - Picnic Areas",
    "URB/REC - Playgrounds", "URB/REC - Playgrounds (GA)", "Vinifera Wine Grape Site Desirability (Short to Medium)",
    "WLF - Irr. Domestic Grasses & Legumes for Food & Cover (TX)",
    "WLF - Irrigated Freshwater Wetland Plants (TX)", "WLF - Upland Coniferous Trees (TX)",
    "WLF - Upland Deciduous Trees (TX)", "WLF - Upland Desertic Shrubs & Trees (TX)",
    "WLF - Upland Native Herbaceous Plants (TX)", "WLF - Upland Shrubs & Vines (TX)",
    "WLF-Soil Suitability - Karner Blue Butterfly (WI)", "WMS - Drainage (IL)",
    "WMS - Drainage - (MI)", "WMS - Embankments, Dikes, and Levees",
    "WMS - Embankments, Dikes, and Levees (OH)", "WMS - Grassed Waterways - (MI)",
    "WMS - Irrigation, Sprinkler (general)", "WMS - Pond Reservoir Area (GA)",
    "WMS-Subsurface Water Management, Installation (ND)", "WMS-Subsurface Water Management, Outflow Quality (ND)"
  )
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

.constructInterpQuery <- function(method, interp, areasymbols = NULL, mukeys = NULL) {

  stopifnot(!is.null(areasymbols) | !is.null(mukeys))

  if (!is.null(areasymbols))
    areasymbols <- soilDB::format_SQL_in_statement(areasymbols)

  if (!is.null(mukeys))
    mukeys <- soilDB::format_SQL_in_statement(mukeys)

  where_clause <- switch(as.character(is.null(areasymbols)),
                         "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                         "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))

  # todo check method and interp against lut
  agg_method <- .interpretationAggMethod(method)
  areasymbols <- soilDB::format_SQL_in_statement(areasymbols)
  switch(agg_method$method,
         "DOMINANT COMPONENT" = .interpretation_aggregation(interp, where_clause, dominant = TRUE),
         "DOMINANT CONDITION" = .interpretation_by_condition(interp, where_clause, dominant = TRUE),
         "WEIGHTED AVERAGE" =   .interpretation_weighted_average(interp, where_clause),
         "NONE" =               .interpretation_aggregation(interp, where_clause)
  )
}

.cleanRuleColumnName <- function(x) gsub("[^A-Za-z0-9]", "", x)

.interpretation_by_condition <- function(interp, where_clause, dominant = TRUE) {
  sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey,
  %s
  FROM legend AS l
  INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
  INNER JOIN component AS c ON c.mukey = mu.mukey %s
  ORDER BY areasymbol, musym, muname, mu.mukey",
  paste0(sapply(interp, function(x) sprintf("  (SELECT TOP 1 ROUND (AVG(interphr) OVER (PARTITION BY interphrc), 2)
   FROM mapunit
   INNER JOIN component ON component.mukey = mapunit.mukey
   INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth = 0 AND mrulename LIKE '%s' GROUP BY interphrc, interphr
   ORDER BY SUM (comppct_r) DESC) AS [rating_%s],
  (SELECT TOP 1 interphrc
   FROM mapunit
   INNER JOIN component ON component.mukey = mapunit.mukey
   INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth = 0 AND mrulename LIKE '%s'
   GROUP BY interphrc, comppct_r ORDER BY SUM(comppct_r) OVER (PARTITION BY interphrc) DESC) AS [class_%s],

  (SELECT DISTINCT SUBSTRING((SELECT('; ' + interphrc)
                              FROM mapunit
                              INNER JOIN component ON component.mukey = mapunit.mukey AND compkind != 'miscellaneous area' AND component.cokey = c.cokey
                              INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey
                              AND ruledepth != 0 AND interphrc NOT LIKE 'Not%%' AND mrulename LIKE '%s' GROUP BY interphrc, interphr
                              ORDER BY interphr DESC, interphrc
                              FOR XML PATH('') ), 3, 1000)) AS [reason_%s]",
                              x, .cleanRuleColumnName(x), 
                              x, .cleanRuleColumnName(x), 
                              x, .cleanRuleColumnName(x))), 
         collapse = ", "), where_clause,
  ifelse(dominant, "AND c.cokey =
    (SELECT TOP 1 c1.cokey FROM component AS c1
     INNER JOIN mapunit ON c.mukey = mapunit.mukey AND c1.mukey = mu.mukey ORDER BY c1.comppct_r DESC, c1.cokey)", ""))
}

.interpretation_aggregation <- function(interp, where_clause, dominant = FALSE) {
  sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey, c.cokey AS cokey, compname, comppct_r, majcompflag,
                %s
                FROM legend  AS l
                INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
                INNER JOIN component AS c ON c.mukey = mu.mukey %s",
                paste0(sapply(interp, function(x) sprintf("(SELECT interphr FROM component INNER JOIN cointerp ON component.cokey = cointerp.cokey AND component.cokey = c.cokey AND ruledepth = 0 AND mrulename LIKE '%s') as [rating_%s],
  (SELECT interphrc FROM component INNER JOIN cointerp ON component.cokey = cointerp.cokey AND component.cokey = c.cokey AND ruledepth = 0 AND mrulename LIKE '%s') as [class_%s],
  (SELECT DISTINCT SUBSTRING(  (  SELECT ( '; ' + interphrc)
                                  FROM mapunit
                                  INNER JOIN component ON component.mukey=mu.mukey AND compkind != 'miscellaneous area' AND component.cokey=c.cokey
                                  INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey
                                  AND ruledepth != 0 AND interphrc NOT LIKE 'Not%%' AND mrulename LIKE '%s' GROUP BY interphrc, interphr
                                  ORDER BY interphr DESC, interphrc
                                  FOR XML PATH('') ), 3, 1000)) as [reason_%s]",
                                      x, .cleanRuleColumnName(x),
                                      x, .cleanRuleColumnName(x),
                                      x, .cleanRuleColumnName(x))),
                                      collapse = ", "), where_clause,
  ifelse(dominant, "AND c.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
                                   INNER JOIN mapunit ON c.mukey = mapunit.mukey AND c1.mukey = mu.mukey
                                   ORDER BY c1.comppct_r DESC, c1.cokey)", ""))
}

.interpretation_weighted_average <- function(interp, where_clause) {
  sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey,
                %s
                INTO #main
                FROM legend AS l
                INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
                INNER JOIN component AS c ON c.mukey = mu.mukey
                GROUP BY areasymbol, musym, muname, mu.mukey
                SELECT areasymbol, musym, muname, mukey,
                %s,
                %s,
                %s
                FROM #main
                DROP TABLE #main",
          paste0(sapply(interp, function(x) sprintf("(SELECT TOP 1 CASE WHEN ruledesign = 1 THEN 'limitation'
                  WHEN ruledesign = 2 THEN 'suitability' END
                  FROM mapunit
                  INNER JOIN component ON component.mukey = mapunit.mukey
                  INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth = 0 AND mrulename LIKE '%s'
                  GROUP BY mapunit.mukey, ruledesign) AS [design_%s],
                ROUND ((SELECT SUM (interphr * comppct_r)
                FROM mapunit
                INNER JOIN component ON component.mukey = mapunit.mukey
                INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth = 0 AND mrulename LIKE '%s'
                GROUP BY mapunit.mukey),2) AS [rating_%s],
                ROUND ((SELECT SUM (comppct_r)
                FROM mapunit
                INNER JOIN component ON component.mukey = mapunit.mukey
                INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey AND ruledepth = 0 AND mrulename LIKE '%s'
                AND (interphr) IS NOT NULL GROUP BY mapunit.mukey),2) AS [sum_com_%s],
                (SELECT DISTINCT SUBSTRING((SELECT ( '; ' + interphrc)
                  FROM mapunit
                  INNER JOIN component ON component.mukey = mapunit.mukey AND compkind != 'miscellaneous area'
                  INNER JOIN cointerp ON component.cokey = cointerp.cokey AND mapunit.mukey = mu.mukey
                  AND ruledepth != 0 AND interphrc NOT LIKE 'Not%%' AND mrulename LIKE '%s' GROUP BY interphrc
                  ORDER BY interphrc
                  FOR XML PATH('') ), 3, 1000)) AS [reason_%s]",
                                                    x, .cleanRuleColumnName(x), 
                                                    x, .cleanRuleColumnName(x), 
                                                    x, .cleanRuleColumnName(x), 
                                                    x, .cleanRuleColumnName(x))), collapse=", "),
           where_clause,
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
