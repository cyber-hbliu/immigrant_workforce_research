library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(stringr)
library(tigris)
library(mapview)
library(rnaturalearth)  # For the world shapefile
library(rnaturalearthdata)
library(sf)
library(RColorBrewer)
library(treemapify)
library(RColorBrewer)
library(tmap)        
library(spdep)
library(packcircles)
library(scales)
library(forcats)
library(patchwork)
library(viridis)
library(scales)

# For the world shapefile library(rnaturalearthdata) 
 
#setting up================================== 
setwd("C:/Users/hliu/Desktop/Data@Request/202411_immigrants/")
census_api_key("a9e713a06a0a0f8ec8531e047c9d01e7d9f507d9", install = TRUE) 
options(tigris_use_cache = TRUE)
year <- 2024
# Most recent year of 5-year ACS data available as of now 
survey <- "acs1" 
color_palette <- c( "#dbd7d1",                     
                    "#bfdef5",                    
                    "#73a8d6",                  
                    "#007abd",                 
                    "#13456e") 
color_palette1 <- c("#bfdef5",         
                    "#73a8d6",         
                    "#007abd",        
                    "#13456e") 
mytheme <- theme(   axis.text.x = element_text(angle = 0,        
                                               hjust = 0.15,     
                                               color = "grey70",    
                                               face = "bold",       
                                               size = 8),  
                    plot.title = element_text(size = 12,    
                                              face = "bold"),  
                    axis.text.y = element_text(size = 8,       
                                               color = "grey70"),  
                    axis.title.x = element_text(size = 8,         
                                                color = "black",   
                                                face="bold"),  
                    axis.title.y = element_text(size = 8,      
                                                color = "black", 
                                                face="bold"),    
                    legend.title = element_blank(),     
                    legend.text = element_text(size = 9,   
                                               face = "bold", 
                                               color = "black"), 
                    legend.justification = c("right", "top"),  
                    legend.key.size = unit(0.8,"line"),  
                    legend.direction = "horizontal", 
                    panel.grid.minor = element_line(colour = "grey80",linetype="dashed",linewidth=0.05),
                    panel.grid.major = element_blank() ) 
mytheme2 <- theme(   axis.text.x = element_text(angle = 0,        
                                                hjust = 0.15,     
                                                color = "grey70",    
                                                face = "bold",       
                                                size = 10),
                     plot.title = element_text(size = 14,    
                                               face = "bold"),  
                     axis.text.y = element_text(size = 10,       
                                                color = "grey70"),  
                     axis.title.x = element_blank(),  
                     axis.title.y = element_blank(),  
                     legend.title = element_text(size = 9,      
                                                 face = "bold", 
                                                 color = "black"),
                     legend.text = element_text(size = 8,          
                                                color = "black"), 
                     panel.grid.minor = element_line(colour = "grey80",linetype="dashed",linewidth=0.05), 
                     panel.grid.major = element_blank() ) 
variables <- c(   total_pop = "B05002_001E",   
                  foreign_born = "B05002_013E",  
                  naturalized = "B05002_014E" ) 
# Pull ACS 5-year estimates for Philadelphia County (using 2023 data) 
philly_immigrant_data <- get_acs(   
  geography = "county",  
  state = "PA", 
  county = "Philadelphia",  
  survey = "acs5",  
  variables = variables,  
  output = "wide",  
  year = 2023 ) 

philly_immigrant_data <- philly_immigrant_data %>%  
  mutate(     
    pct_foreign_born = foreign_born / total_pop * 100,    
    pct_naturalized = naturalized / foreign_born * 100   ) 

print(philly_immigrant_data) 


############Trend#######################
library(purrr)
variables <- c(total_pop = "B05002_001E",
               foreign_born = "B05002_013E")
years <- 2013:2023
philly_trend <- map_dfr(years, function(y) {
  get_acs(
    geography = "county",
    state = "PA",
    county = "Philadelphia",
    survey = "acs5",
    variables = variables,
    output = "wide",
    year = y
  ) %>%
    mutate(year = y)
})

philly_trend <- philly_trend %>%
  mutate(total_pop = as.numeric(total_pop),
         foreign_born = as.numeric(foreign_born),
         pct_foreign_born = (foreign_born / total_pop) * 100) %>%
  arrange(year) %>%
  # Calculate year-over-year percent change for foreign_born
  mutate(yoy_change = (foreign_born / lag(foreign_born) - 1) * 100)
philly_trend_long <- philly_trend %>%
  select(year, foreign_born, yoy_change) %>%
  pivot_longer(cols = foreign_born, names_to = "population", values_to = "count") %>%
  mutate(population = recode(population, foreign_born = "Foreign-Born Population"))

ggplot(philly_trend_long, aes(x = year, y = count)) +
  geom_line(size = 1.2, color = "#007abd") +
  geom_point(size = 3, color = "#007abd") +
  # Absolute count labels (placed slightly above each point)
  geom_text(data = philly_trend,
            aes(x = year, y = foreign_born, label = comma(foreign_born)),
            vjust = -1.5, color = "black", size = 3, fontface = "bold") +
  # Year-over-year percent change labels (placed further above)
  geom_text(data = philly_trend %>% filter(!is.na(yoy_change)),
            aes(x = year, y = foreign_born * 1.1,
                label = paste0(round(yoy_change, 1), "%")),
            color = "#007abd", size = 4, fontface = "bold") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_x_continuous(breaks = years) +
  labs(
    title = "Growth in the Foreign-Born Population in Philadelphia County (2013-2023)",
    subtitle = "Year-over-year percent change indicated above each point",
    x = "Year",
    y = "Population Count",
    caption = "Data Source: 2023 ACS 5-year "
  ) +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "top")

ggsave("0_foreignborn_change.png",height = 4, width = 10, units = "in")


######################################### 
#Define PUMS variables & pull data 
######################################### 
# We'll pull from 2023 ACS 1-year PUMS for Pennsylvania.
# You can switch 'acs1' -> 'acs5' if you want 5-year data (bigger sample, but less current). 
# The recode = TRUE option gives you more user-friendly labels for some variables. 
# dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2023.pdf 
# https://www.census.gov/naics/?58967?yearbck=2022 
pums_vars <- c(   "AGEP",  # Age   
                  "NATIVITY", #Nativity 1 .Native; 2 .Foreign born   
                  "SCHL",  # Educational attainment  
                  "ESR",   # Employment status   
                  "CIT",   # Citizenship status  
                  "POBP",  # Place of birth  
                  "POVPIP",# Income-to-poverty ratio recode,
                  "NP", # Number of persons in this household
                  "WAGP",  # Wages   
                  "NAICSP",# Industry codes (NAICS-based)   
                  "INDP",  # Detailed industry recode  
                  "ENG",   # English speaking ability   
                  "HHL",  # Household language
                  "LNGI", #Limited English speaking household  
                  "YOEP",   # Year of entry into the US  
                  "PUMA",
                  "COW" ,# Class of Worker
                  "HHLDRRAC1P",# Detailed race code of the householder
                  "WAOB"       # World Area of Birth
                  ) 

pums_data <- get_pums(   
  variables = pums_vars,   
  state = "PA",   
  survey = "acs5",   
  year = 2023,  
  recode = TRUE)

#write.csv(pums_data, "pums_data_2023.csv")
philly_pums_data <- pums_data %>%
  filter(PUMA %in% c(
    "03216", "03221", "03222", "03223", "03224", 
    "03225", "03227", "03228", "03229", "03230", "03231"
  ))

waob_labels <- c(
  "1" = "US state (POBP = 001-059)",
  "2" = "PR and US Island Areas (POBP = 061-099)",
  "3" = "Latin America (POBP = 303,310-399)",
  "4" = "Asia (POBP = 158-159,161,200-299)",
  "5" = "Europe (POBP = 100-157,160,162-199)",
  "6" = "Africa (POBP = 400-499)",
  "7" = "Northern America (POBP = 300-302,304-309)",
  "8" = "Oceania and at Sea (POBP = 060,500-554)"
)

hhl_drac_labels <- c(
  "b" = "N/A (GQ/vacant)",
  "1" = "White alone",
  "2" = "Black or African American alone",
  "3" = "American Indian alone",
  "4" = "Alaska Native alone",
  "5" = "American Indian/Alaska Native, not specified/no other races",
  "6" = "Asian alone",
  "7" = "Native Hawaiian and Other Pacific Islander alone",
  "8" = "Some Other Race alone",
  "9" = "Two or More Races"
)

cow_labels <- c(
  "b" = "N/A (less than 16 or NILF >5 yrs/never worked)",
  "1" = "Private For-Profit Employee",
  "2" = "Private Non-Profit Employee",
  "3" = "Local Government Employee",
  "4" = "State Government Employee",
  "5" = "Federal Government Employee",
  "6" = "Self-Employed (Not Inc.)",
  "7" = "Self-Employed (Incorp.)",
  "8" = "Working without Pay in Family Biz",
  "9" = "Unemployed (>5 yrs/never worked)"
)

nativity_labels <- c(
  "1" = "Native",
  "2" = "Foreign born"
)

eng_labels <- c(
  "b" = "N/A (<5 years old)",
  "1" = "Very well",
  "2" = "Well",
  "3" = "Not well",
  "4" = "Not at all"
)

hhl_labels <- c(
  "b" = "N/A (GQ/vacant)",
  "1" = "English only",
  "2" = "Spanish",
  "3" = "Other Indo-European languages",
  "4" = "Asian and Pacific Island languages",
  "5" = "Other language"
)


lngi_labels <- c(
  "b" = "N/A (GQ/vacant)",
  "1" = "At least one HH member (14+) speaks English only / very well",
  "2" = "No HH member (14+) speaks English only / very well"
)

cit_labels <- c(
  "1" = "Born in the US",
  "2" = "Born in PR/Guam/USVI",
  "3" = "Born abroad of US citizen parent(s)",
  "4" = "US citizen by naturalization",
  "5" = "Not a US citizen"
)
esr_labels <- c(
  "b" = "N/A (less than 16 years old)",
  "1" = "Civilian employed, at work",
  "2" = "Civilian employed, with a job but not at work",
  "3" = "Unemployed",
  "4" = "Armed forces, at work",
  "5" = "Armed forces, with a job but not at work",
  "6" = "Not in labor force"
)
schl_labels <- c(
  "00" = "N/A (< 3 years old)",
  "01" = "No schooling completed",
  "02" = "Nursery school, preschool",
  "03" = "K-11",  # Kindergarten
  "04" = "K-11",  # Grade 1
  "05" = "K-11",  # Grade 2
  "06" = "K-11",  # Grade 3
  "07" = "K-11",  # Grade 4
  "08" = "K-11",  # Grade 5
  "09" = "K-11",  # Grade 6
  "10" = "K-11",  # Grade 7
  "11" = "K-11",  # Grade 8
  "12" = "K-11",  # Grade 9
  "13" = "K-11",  # Grade 10
  "14" = "K-11",  # Grade 11
  "15" = "12th grade - no diploma",
  "16" = "Regular high school diploma",
  "17" = "GED or alternative credential",
  "18" = "Some college, but less than 1 year",
  "19" = "1+ years of college credit, no degree",
  "20" = "Associate's degree",
  "21" = "Bachelor's degree",
  "22" = "Master's degree",
  "23" = "Professional degree (beyond bachelor's)",
  "24" = "Doctorate degree"
)

pobp_labels <- c(
  "001" = "Alabama (AL)",
  "002" = "Alaska (AK)",
  "004" = "Arizona (AZ)",
  "005" = "Arkansas (AR)",
  "006" = "California (CA)",
  "008" = "Colorado (CO)",
  "009" = "Connecticut (CT)",
  "010" = "Delaware (DE)",
  "011" = "District of Columbia (DC)",
  "012" = "Florida (FL)",
  "013" = "Georgia (GA)",
  "015" = "Hawaii (HI)",
  "016" = "Idaho (ID)",
  "017" = "Illinois (IL)",
  "018" = "Indiana (IN)",
  "019" = "Iowa (IA)",
  "020" = "Kansas (KS)",
  "021" = "Kentucky (KY)",
  "022" = "Louisiana (LA)",
  "023" = "Maine (ME)",
  "024" = "Maryland (MD)",
  "025" = "Massachusetts (MA)",
  "026" = "Michigan (MI)",
  "027" = "Minnesota (MN)",
  "028" = "Mississippi (MS)",
  "029" = "Missouri (MO)",
  "030" = "Montana (MT)",
  "031" = "Nebraska (NE)",
  "032" = "Nevada (NV)",
  "033" = "New Hampshire (NH)",
  "034" = "New Jersey (NJ)",
  "035" = "New Mexico (NM)",
  "036" = "New York (NY)",
  "037" = "North Carolina (NC)",
  "038" = "North Dakota (ND)",
  "039" = "Ohio (OH)",
  "040" = "Oklahoma (OK)",
  "041" = "Oregon (OR)",
  "042" = "Pennsylvania (PA)",
  "044" = "Rhode Island (RI)",
  "045" = "South Carolina (SC)",
  "046" = "South Dakota (SD)",
  "047" = "Tennessee (TN)",
  "048" = "Texas (TX)",
  "049" = "Utah (UT)",
  "050" = "Vermont (VT)",
  "051" = "Virginia (VA)",
  "053" = "Washington (WA)",
  "054" = "West Virginia (WV)",
  "055" = "Wisconsin (WI)",
  "056" = "Wyoming (WY)",
  "060" = "American Samoa",
  "066" = "Guam",
  "069" = "Northern Mariana Islands",
  "072" = "Puerto Rico",
  "078" = "US Virgin Islands",
  "100" = "Albania",
  "102" = "Austria",
  "103" = "Belgium",
  "104" = "Bulgaria",
  "105" = "Czechoslovakia",
  "106" = "Denmark",
  "108" = "Finland",
  "109" = "France",
  "110" = "Germany",
  "116" = "Greece",
  "117" = "Hungary",
  "118" = "Iceland",
  "119" = "Ireland",
  "120" = "Italy",
  "126" = "Netherlands",
  "127" = "Norway",
  "128" = "Poland",
  "129" = "Portugal",
  "130" = "Azores Islands",
  "132" = "Romania",
  "134" = "Spain",
  "136" = "Sweden",
  "137" = "Switzerland",
  "138" = "UK, Not Specified",
  "139" = "England",
  "140" = "Scotland",
  "142" = "Northern Ireland",
  "147" = "Yugoslavia",
  "148" = "Czech Republic",
  "149" = "Slovakia",
  "150" = "Bosnia and Herzegovina",
  "151" = "Croatia",
  "152" = "Macedonia",
  "154" = "Serbia",
  "156" = "Latvia",
  "157" = "Lithuania",
  "158" = "Armenia",
  "159" = "Azerbaijan",
  "160" = "Belarus",
  "161" = "Georgia",
  "162" = "Moldova",
  "163" = "Russia",
  "164" = "Ukraine",
  "165" = "USSR",
  "166" = "Europe (Not Specified)",
  "167" = "Kosovo",
  "168" = "Montenegro",
  "169" = "Other Europe, Not Specified",
  "200" = "Afghanistan",
  "202" = "Bangladesh",
  "203" = "Bhutan",
  "205" = "Myanmar",
  "206" = "Cambodia",
  "207" = "China",
  "209" = "Hong Kong",
  "210" = "India",
  "211" = "Indonesia",
  "212" = "Iran",
  "213" = "Iraq",
  "214" = "Israel",
  "215" = "Japan",
  "216" = "Jordan",
  "217" = "Korea",
  "218" = "Kazakhstan",
  "219" = "Kyrgyzstan",
  "222" = "Kuwait",
  "223" = "Laos",
  "224" = "Lebanon",
  "226" = "Malaysia",
  "228" = "Mongolia",
  "229" = "Nepal",
  "231" = "Pakistan",
  "233" = "Philippines",
  "235" = "Saudi Arabia",
  "236" = "Singapore",
  "238" = "Sri Lanka",
  "239" = "Syria",
  "240" = "Taiwan",
  "242" = "Thailand",
  "243" = "Turkey",
  "245" = "United Arab Emirates",
  "246" = "Uzbekistan",
  "247" = "Vietnam",
  "248" = "Yemen",
  "249" = "Asia, Not Specified",
  "253" = "South Central Asia, Not Specified",
  "254" = "Other Asia, Not Specified",
  "300" = "Bermuda",
  "301" = "Canada",
  "303" = "Mexico",
  "310" = "Belize",
  "311" = "Costa Rica",
  "312" = "El Salvador",
  "313" = "Guatemala",
  "314" = "Honduras",
  "315" = "Nicaragua",
  "316" = "Panama",
  "321" = "Antigua and Barbuda",
  "323" = "Bahamas",
  "324" = "Barbados",
  "327" = "Cuba",
  "328" = "Dominica",
  "329" = "Dominican Republic",
  "330" = "Grenada",
  "332" = "Haiti",
  "333" = "Jamaica",
  "338" = "St. Kitts-Nevis",
  "339" = "St. Lucia",
  "340" = "St. Vincent & the Grenadines",
  "341" = "Trinidad And Tobago",
  "343" = "West Indies",
  "344" = "Caribbean, Not Specified",
  "360" = "Argentina",
  "361" = "Bolivia",
  "362" = "Brazil",
  "363" = "Chile",
  "364" = "Colombia",
  "365" = "Ecuador",
  "368" = "Guyana",
  "369" = "Paraguay",
  "370" = "Peru",
  "372" = "Uruguay",
  "373" = "Venezuela",
  "374" = "South America (Not Specified)",
  "399" = "Americas, Not Specified",
  "400" = "Algeria",
  "407" = "Cameroon",
  "408" = "Cabo Verde",
  "412" = "Congo",
  "414" = "Egypt",
  "416" = "Ethiopia",
  "417" = "Eritrea",
  "420" = "Gambia",
  "421" = "Ghana",
  "423" = "Guinea",
  "425" = "Ivory Coast",
  "427" = "Kenya",
  "429" = "Liberia",
  "430" = "Libya",
  "436" = "Morocco",
  "440" = "Nigeria",
  "442" = "Rwanda",
  "444" = "Senegal",
  "447" = "Sierra Leone",
  "448" = "Somalia",
  "449" = "South Africa",
  "451" = "Sudan",
  "453" = "Tanzania",
  "454" = "Togo",
  "456" = "Tunisia",
  "457" = "Uganda",
  "459" = "Democratic Republic of Congo (Zaire)",
  "460" = "Zambia",
  "461" = "Zimbabwe",
  "462" = "Africa, Not Specified",
  "463" = "South Sudan",
  "464" = "Northern Africa, Not Specified",
  "467" = "Western Africa, Not Specified",
  "468" = "Other Africa, Not Specified",
  "469" = "Eastern Africa, Not Specified",
  "501" = "Australia",
  "508" = "Fiji",
  "511" = "Marshall Islands",
  "512" = "Micronesia",
  "515" = "New Zealand",
  "523" = "Tonga",
  "527" = "Samoa",
  "554" = "Other US Island Areas/Oceania, Not Specified/At Sea"
)

indp_labels <- c(
  "0000" = "N/A (<16 years old/NILF last worked >5 yrs or never worked)",
  "0170" = "AGR - Crop Production",
  "0180" = "AGR - Animal Production And Aquaculture",
  "0190" = "AGR - Forestry Except Logging",
  "0270" = "AGR - Logging",
  "0280" = "AGR - Fishing, Hunting And Trapping",
  "0290" = "AGR - Support Activities For Agriculture And Forestry",
  "0370" = "EXT - Oil And Gas Extraction",
  "0380" = "EXT - Coal Mining",
  "0390" = "EXT - Metal Ore Mining",
  "0470" = "EXT - Nonmetallic Mineral Mining And Quarrying",
  "0490" = "EXT - Support Activities For Mining",
  "0570" = "UTL - Electric Power Generation, Transmission, Distribution",
  "0580" = "UTL - Natural Gas Distribution",
  "0590" = "UTL - Electric And Gas, And Other Combinations",
  "0670" = "UTL - Water Supply, Irrigation, Steam & Air-Conditioning",
  "0680" = "UTL - Sewage Treatment Facilities",
  "0690" = "UTL - Not Specified Utilities",
  "0770" = "CON - Construction",
  "1070" = "MFG - Animal Food, Grain And Oilseed Milling",
  "1080" = "MFG - Sugar And Confectionery Products",
  "1090" = "MFG - Fruit And Vegetable Preserving And Specialty Food",
  "1170" = "MFG - Dairy Product",
  "1180" = "MFG - Animal Slaughtering And Processing",
  "1190" = "MFG - Retail Bakeries",
  "1270" = "MFG - Bakeries And Tortilla, Except Retail Bakeries",
  "1280" = "MFG - Seafood And Other Misc Foods, N.E.C.",
  "1290" = "MFG - Not Specified Food Industries",
  "1370" = "MFG - Beverage",
  "1390" = "MFG - Tobacco",
  "1470" = "MFG - Fiber, Yarn, And Thread Mills",
  "1480" = "MFG - Fabric Mills, Except Knit Fabric Mills",
  "1490" = "MFG - Textile And Fabric Finishing And Coating Mills",
  "1570" = "MFG - Carpet And Rug Mills",
  "1590" = "MFG - Textile Product Mills, Except Carpet And Rug",
  "1670" = "MFG - Knit Fabric Mills, And Apparel Knitting Mills",
  "1691" = "MFG - Cut And Sew, Apparel Accessories, Other Apparel",
  "1770" = "MFG - Footwear",
  "1790" = "MFG - Leather And Hide Tanning/Finishing, Other Leather",
  "1870" = "MFG - Pulp, Paper, And Paperboard Mills",
  "1880" = "MFG - Paperboard Container",
  "1890" = "MFG - Misc Paper And Pulp Products",
  "1990" = "MFG - Printing And Related Support Activities",
  "2070" = "MFG - Petroleum Refineries",
  "2090" = "MFG - Petroleum/Coal Products Mfg, Except Refineries",
  "2170" = "MFG - Resin, Synthetic Rubber, Artificial/Synthetic Fibers",
  "2180" = "MFG - Pesticide, Fertilizer, Other Agricultural Chemical",
  "2190" = "MFG - Pharmaceutical And Medicine",
  "2270" = "MFG - Paint, Coating, And Adhesive",
  "2280" = "MFG - Soap, Cleaning Compound, Toilet Preparation",
  "2290" = "MFG - Basic Chemical & Other Chemical Product, N.E.C.",
  "2370" = "MFG - Plastics Product",
  "2380" = "MFG - Tire",
  "2390" = "MFG - Rubber Products, Except Tires",
  "2470" = "MFG - Pottery, Ceramics, And Plumbing Fixture",
  "2480" = "MFG - Clay Building Material And Refractories",
  "2490" = "MFG - Glass And Glass Product",
  "2570" = "MFG - Cement, Concrete, Lime, Gypsum Product",
  "2590" = "MFG - Other Nonmetallic Mineral Product",
  "2670" = "MFG - Iron And Steel Mills And Steel Product",
  "2680" = "MFG - Alumina And Aluminum Production/Processing",
  "2690" = "MFG - Nonferrous Metal (Except Aluminum) Production",
  "2770" = "MFG - Foundries",
  "2780" = "MFG - Forgings And Stampings",
  "2790" = "MFG - Cutlery And Handtool",
  "2870" = "MFG - Architectural And Structural Metals, Boiler/Tank",
  "2880" = "MFG - Machine Shops; Turned Product; Screw, Nut, Bolt",
  "2890" = "MFG - Coating, Engraving, Heat Treating, Allied",
  "2970" = "MFG - Ordnance",
  "2980" = "MFG - Misc Fabricated Metal Product",
  "2990" = "MFG - Not Specified Metal Industries",
  "3070" = "MFG - Agricultural Implement",
  "3080" = "MFG - Construction, Mining, Oil/Gas Field Machinery",
  "3095" = "MFG - Commercial And Service Industry Machinery",
  "3170" = "MFG - Metalworking Machinery",
  "3180" = "MFG - Engine, Turbine, Power Transmission Equipment",
  "3291" = "MFG - Machinery, N.E.C. Or Not Specified",
  "3365" = "MFG - Computer And Peripheral Equipment",
  "3370" = "MFG - Communications, Audio, Video Equipment",
  "3380" = "MFG - Navigational, Measuring, Electromedical Instruments",
  "3390" = "MFG - Semiconductor, Magnetic/Optical Media, Other Electronic",
  "3470" = "MFG - Household Appliance",
  "3490" = "MFG - Electric Lighting/Equipment, Other Electrical, N.E.C.",
  "3570" = "MFG - Motor Vehicles And Motor Vehicle Equipment",
  "3580" = "MFG - Aircraft, Aircraft Engine, And Aircraft Parts",
  "3590" = "MFG - Guided Missile, Space Vehicle, And Parts",
  "3670" = "MFG - Railroad Rolling Stock",
  "3680" = "MFG - Ship And Boat Building",
  "3690" = "MFG - Other Transportation Equipment",
  "3770" = "MFG - Sawmills And Wood Preservation",
  "3780" = "MFG - Veneer, Plywood, Engineered Wood Product",
  "3790" = "MFG - Manufactured Home/Mobile Home, Prefab Wood Building",
  "3875" = "MFG - Miscellaneous Wood Product",
  "3895" = "MFG - Furniture And Related Product",
  "3960" = "MFG - Medical Equipment And Supplies",
  "3970" = "MFG - Sporting & Athletic Goods, Doll, Toy, Game",
  "3980" = "MFG - Misc Manufacturing, N.E.C.",
  "3990" = "MFG - Not Specified Manufacturing Industries",
  "4070" = "WHL - Motor Vehicle/Parts/Supplies Merchant Wholesalers",
  "4080" = "WHL - Furniture/Home Furnishing Merchant Wholesalers",
  "4090" = "WHL - Lumber/Other Construction Materials Wholesalers",
  "4170" = "WHL - Professional/Commercial Equipment Wholesalers",
  "4180" = "WHL - Metal & Mineral (Except Petroleum) Merchant Wholesalers",
  "4195" = "WHL - Household Appliances/Electronic Goods Wholesalers",
  "4265" = "WHL - Hardware/Plumbing/Heating Equipment Wholesalers",
  "4270" = "WHL - Machinery, Equipment, Supplies Merchant Wholesalers",
  "4280" = "WHL - Recyclable Material Merchant Wholesalers",
  "4290" = "WHL - Misc Durable Goods, Except Recyclable, Wholesalers",
  "4370" = "WHL - Paper/Paper Product Merchant Wholesalers",
  "4380" = "WHL - Drugs, Sundries, Chemical, Allied Product Wholesalers",
  "4390" = "WHL - Apparel, Piece Goods, Notions Merchant Wholesalers",
  "4470" = "WHL - Grocery & Related Product Merchant Wholesalers",
  "4480" = "WHL - Farm Product Raw Material Merchant Wholesalers",
  "4490" = "WHL - Petroleum & Petroleum Products Merchant Wholesalers",
  "4560" = "WHL - Beer, Wine, Distilled Alcoholic Beverage Wholesalers",
  "4570" = "WHL - Farm Supplies Merchant Wholesalers",
  "4580" = "WHL - Misc Nondurable Goods Merchant Wholesalers",
  "4585" = "WHL - Wholesale Trade Agents And Brokers",
  "4590" = "WHL - Not Specified Wholesale Trade",
  "4670" = "RET - Automobile Dealers",
  "4681" = "RET - Other Motor Vehicle Dealers",
  "4691" = "RET - Automotive Parts, Accessories, Tire Retailers",
  "4771" = "RET - Furniture And Home Furnishings Retailers",
  "4796" = "RET - Electronics And Appliance Retailers",
  "4871" = "RET - Building Material & Supplies Dealers, Except Hardware",
  "4881" = "RET - Hardware Retailers",
  "4891" = "RET - Lawn And Garden Equipment/Supplies Retailers",
  "4971" = "RET - Supermarkets And Other Grocery (No Convenience)",
  "4973" = "RET - Convenience Retailers And Vending Machine Operators",
  "4981" = "RET - Specialty Food Retailers",
  "4991" = "RET - Beer, Wine, And Liquor Retailers",
  "5071" = "RET - Pharmacies And Drug Retailers",
  "5081" = "RET - Health & Personal Care (Except Drug) Retailers",
  "5090" = "RET - Gasoline Stations",
  "5171" = "RET - Clothing And Clothing Accessories Retailers",
  "5181" = "RET - Shoe Retailers",
  "5191" = "RET - Jewelry, Luggage, Leather Goods Retailers",
  "5276" = "RET - Sporting Goods, Hobby, Toy, And Game Retailers",
  "5281" = "RET - Sewing, Needlework, Piece Goods Retailers",
  "5296" = "RET - Musical Instrument And Supplies Retailers",
  "5371" = "RET - Book Retailers And News Dealers",
  "5382" = "RET - Department Stores",
  "5392" = "RET - Warehouse Clubs, Supercenters, General Merch Retail",
  "5471" = "RET - Florists",
  "5481" = "RET - Office Supplies And Stationery Retailers",
  "5491" = "RET - Used Merchandise Retailers",
  "5571" = "RET - Gift, Novelty, Souvenir Retailers",
  "5581" = "RET - Other Miscellaneous Retailers",
  "5680" = "RET - Fuel Dealers",
  "5791" = "RET - Not Specified Retail Trade",
  "6070" = "TRN - Air Transportation",
  "6080" = "TRN - Rail Transportation",
  "6090" = "TRN - Water Transportation",
  "6170" = "TRN - Truck Transportation",
  "6180" = "TRN - Transit/Ground Passenger Transportation (except Taxi)",
  "6190" = "TRN - Taxi And Limousine Service",
  "6270" = "TRN - Pipeline Transportation",
  "6280" = "TRN - Scenic And Sightseeing Transportation",
  "6290" = "TRN - Support Activities For Transportation",
  "6370" = "TRN - Postal Service",
  "6380" = "TRN - Couriers And Messengers",
  "6390" = "TRN - Warehousing And Storage",
  "6471" = "INF - Newspaper Publishers",
  "6481" = "INF - Periodical, Book, Directory, Mailing List Publishers",
  "6490" = "INF - Software Publishers",
  "6570" = "INF - Motion Pictures And Video Industries",
  "6590" = "INF - Sound Recording Industries",
  "6671" = "INF - Broadcasting And Content Providers",
  "6680" = "INF - Wired Telecommunications Carriers",
  "6690" = "INF - Telecommunications, Except Wired Carriers",
  "6695" = "INF - Computing Infrastructure, Data Processing, Hosting",
  "6770" = "INF - Libraries And Archives",
  "6781" = "INF - Web Search Portals & All Other Info Services",
  "6871" = "FIN - Monetary Authorities/Central Bank, Commercial Banking",
  "6881" = "FIN - Credit Unions, Savings Institutions, Other Depository",
  "6890" = "FIN - Nondepository Credit & Related Activities",
  "6970" = "FIN - Securities, Commodities, Funds, Other Financial",
  "6991" = "FIN - Insurance Carriers",
  "6992" = "FIN - Agencies, Brokerages, Other Insurance Related",
  "7071" = "FIN - Lessors Of Real Estate, Offices Of Real Estate Agents",
  "7072" = "FIN - Activities Related To Real Estate",
  "7080" = "FIN - Automotive Equipment Rental And Leasing",
  "7181" = "FIN - Consumer Goods Rental, General Rental Centers",
  "7190" = "FIN - Commercial/Industrial Machinery, Nonfinancial Assets",
  "7270" = "PRF - Legal Services",
  "7280" = "PRF - Accounting, Tax Prep, Bookkeeping, Payroll Services",
  "7290" = "PRF - Architectural, Engineering, Related Services",
  "7370" = "PRF - Specialized Design Services",
  "7380" = "PRF - Computer Systems Design, Related Services",
  "7390" = "PRF - Management, Scientific, Technical Consulting",
  "7460" = "PRF - Scientific Research And Development Services",
  "7470" = "PRF - Advertising, Public Relations, Related Services",
  "7480" = "PRF - Veterinary Services",
  "7490" = "PRF - Other Professional/Scientific/Technical (Except Vet)",
  "7570" = "PRF - Management Of Companies And Enterprises",
  "7580" = "PRF - Employment Services",
  "7590" = "PRF - Business Support Services",
  "7670" = "PRF - Travel Arrangements And Reservation Services",
  "7680" = "PRF - Investigation And Security Services",
  "7690" = "PRF - Services To Buildings And Dwellings (Except Landscape)",
  "7770" = "PRF - Landscaping Services",
  "7780" = "PRF - Other Administrative And Other Support Services",
  "7790" = "PRF - Waste Management And Remediation Services",
  "7860" = "EDU - Elementary And Secondary Schools",
  "7870" = "EDU - Junior Colleges, Colleges, Universities, Prof Schools",
  "7880" = "EDU - Business, Trade, Tech Schools, Computer/Mgmt Training",
  "7890" = "EDU - Other Schools/Instruction, Educational Support",
  "7970" = "MED - Offices Of Physicians",
  "7980" = "MED - Offices Of Dentists",
  "7990" = "MED - Offices Of Chiropractors",
  "8070" = "MED - Offices Of Optometrists",
  "8080" = "MED - Offices Of Other Health Practitioners",
  "8090" = "MED - Outpatient Care Centers",
  "8170" = "MED - Home Health Care Services",
  "8180" = "MED - Other Health Care Services",
  "8191" = "MED - Hospitals (General/Specialty Except Psych/Substance)",
  "8192" = "MED - Psychiatric And Substance Abuse Hospitals",
  "8270" = "MED - Nursing Care Facilities (Skilled Nursing)",
  "8290" = "MED - Residential Care Facilities (Except Skilled Nursing)",
  "8370" = "SCA - Individual And Family Services",
  "8380" = "SCA - Community Food, Housing, Emergency/Other Relief",
  "8390" = "SCA - Vocational Rehabilitation Services",
  "8470" = "SCA - Child Care Services",
  "8561" = "ENT - Performing Arts Companies",
  "8562" = "ENT - Spectator Sports",
  "8563" = "ENT - Promoters of Performing Arts/Sports, Agents/Managers",
  "8564" = "ENT - Independent Artists, Writers, Performers",
  "8570" = "ENT - Museums, Historical Sites, Similar Institutions",
  "8580" = "ENT - Bowling Centers",
  "8590" = "ENT - Other Amusement, Gambling, Recreation Industries",
  "8660" = "ENT - Traveler Accommodation",
  "8670" = "ENT - RV Parks, Camps, Rooming/Boarding Houses",
  "8680" = "ENT - Food Services And Drinking Places (except Alcohol)",
  "8690" = "ENT - Drinking Places (Alcoholic Beverages)",
  "8770" = "SRV - Automotive Repair And Maintenance (Except Car Washes)",
  "8780" = "SRV - Car Washes",
  "8790" = "SRV - Electronic/Precision Equipment Repair And Maintenance",
  "8870" = "SRV - Commercial/Industrial Machinery Repair And Maintenance",
  "8891" = "SRV - Personal/Household Goods Repair And Maintenance",
  "8970" = "SRV - Barber Shops",
  "8980" = "SRV - Beauty Salons",
  "8990" = "SRV - Nail Salons And Other Personal Care Services",
  "9070" = "SRV - Drycleaning And Laundry Services",
  "9080" = "SRV - Death Care Services",
  "9090" = "SRV - Other Personal Services",
  "9160" = "SRV - Religious Organizations",
  "9170" = "SRV - Civic, Social, Advocacy Orgs, Grantmaking/Giving",
  "9180" = "SRV - Labor Unions And Similar Labor Organizations",
  "9190" = "SRV - Business, Professional, Political, Similar Orgs",
  "9290" = "SRV - Private Households",
  "9370" = "ADM - Executive Offices And Legislative Bodies",
  "9380" = "ADM - Public Finance Activities",
  "9390" = "ADM - Other General Government And Support",
  "9470" = "ADM - Justice, Public Order, Safety Activities",
  "9480" = "ADM - Administration Of Human Resource Programs",
  "9490" = "ADM - Administration Of Environmental Quality/Housing/Urban",
  "9570" = "ADM - Administration Of Economic Programs, Space Research",
  "9590" = "ADM - National Security And International Affairs",
  "9670" = "MIL - U.S. Army",
  "9680" = "MIL - U.S. Air Force",
  "9690" = "MIL - U.S. Navy",
  "9770" = "MIL - U.S. Marines",
  "9780" = "MIL - U.S. Coast Guard",
  "9790" = "MIL - Armed Forces, Branch Not Specified",
  "9870" = "MIL - Military Reserves Or National Guard",
  "9920" = "Unemployed, No Work Experience In Last 5+ Years Or Never Worked"
)
naicsp_labels <- c(
  # "N" for N/A
  "N" = "N/A",
  
  # Agriculture
  "111" = "AGR - Crop Production",
  "112" = "AGR - Animal Production And Aquaculture",
  "1133" = "AGR - Logging",
  "113M" = "AGR - Forestry Except Logging",
  "114" = "AGR - Fishing, Hunting And Trapping",
  "115" = "AGR - Support Activities For Agriculture And Forestry",
  
  # Extraction / Mining
  "211" = "EXT - Oil And Gas Extraction",
  "2121" = "EXT - Coal Mining",
  "2122" = "EXT - Metal Ore Mining",
  "2123" = "EXT - Nonmetallic Mineral Mining And Quarrying",
  "213"  = "EXT - Support Activities For Mining",
  
  # Utilities
  "2211P" = "UTL - Electric Power Generation, Transmission, Distribution",
  "2212P" = "UTL - Natural Gas Distribution",
  "22132" = "UTL - Sewage Treatment Facilities",
  "2213M" = "UTL - Water/Irrigation/Steam/Air-Conditioning Supply",
  "221MP" = "UTL - Electric And Gas, And Other Combinations",
  "22S"   = "UTL - Not Specified Utilities",
  
  # Construction
  "23"    = "CON - Construction",
  
  # Manufacturing (Food)
  "3113"  = "MFG - Sugar And Confectionery Products",
  "3114"  = "MFG - Fruit And Vegetable Preserving And Specialty Food",
  "3115"  = "MFG - Dairy Product",
  "3116"  = "MFG - Animal Slaughtering And Processing",
  "311811"= "MFG - Retail Bakeries",
  "3118Z" = "MFG - Bakeries And Tortilla, Except Retail Bakeries",
  "311M1" = "MFG - Animal Food, Grain And Oilseed Milling",
  "311M2" = "MFG - Seafood And Other Miscellaneous Foods, N.E.C.",
  "311S"  = "MFG - Not Specified Food Industries",
  "3121"  = "MFG - Beverage",
  "3122"  = "MFG - Tobacco",
  
  # Manufacturing (Textiles/Apparel)
  "3131"  = "MFG - Fiber, Yarn, And Thread Mills",
  "3132Z" = "MFG - Fabric Mills, Except Knit Fabric Mills",
  "3133"  = "MFG - Textile And Fabric Finishing And Coating Mills",
  "31411" = "MFG - Carpet And Rug Mills",
  "314Z"  = "MFG - Textile Product Mills, Except Carpet And Rug",
  "315M"  = "MFG - Cut And Sew, And Apparel Accessories And Other Apparel",
  "3162"  = "MFG - Footwear",
  "316M"  = "MFG - Leather/Hide Tanning & Finishing, Other Leather Product",
  "31M"   = "MFG - Knit Fabric Mills, And Apparel Knitting Mills",
  
  # Manufacturing (Wood)
  "3211"  = "MFG - Sawmills And Wood Preservation",
  "3212"  = "MFG - Veneer, Plywood, And Engineered Wood Product",
  "32199M"= "MFG - Manufactured Home (Mobile) And Prefab Wood Building",
  "3219ZM"= "MFG - Miscellaneous Wood Product",
  
  # Manufacturing (Paper/Printing)
  "3221"  = "MFG - Pulp, Paper, And Paperboard Mills",
  "32221" = "MFG - Paperboard Container",
  "3222M" = "MFG - Miscellaneous Paper And Pulp Products",
  "3231"  = "MFG - Printing And Related Support Activities",
  
  # Manufacturing (Petroleum/Chemical)
  "3241M" = "MFG - Petroleum And Coal Products Mfg, Except Refineries",
  "32411" = "MFG - Petroleum Refineries",
  "3252"  = "MFG - Resin, Synthetic Rubber, Artificial/Synthetic Fibers",
  "3253"  = "MFG - Pesticide, Fertilizer, Other Agricultural Chemical",
  "3254"  = "MFG - Pharmaceutical And Medicine",
  "3255"  = "MFG - Paint, Coating, And Adhesive",
  "3256"  = "MFG - Soap, Cleaning Compound, Toilet Preparation",
  "325M"  = "MFG - Basic Chemical, Other Chemical Product And Preparation",
  
  # Manufacturing (Plastics/Rubber)
  "3261"  = "MFG - Plastics Product",
  "32621" = "MFG - Tire",
  "3262M" = "MFG - Rubber Products, Except Tires",
  
  # Manufacturing (Nonmetallic Mineral)
  "32711" = "MFG - Pottery, Ceramics, And Plumbing Fixture",
  "32712" = "MFG - Clay Building Material And Refractories",
  "3272"  = "MFG - Glass And Glass Product",
  "3279"  = "MFG - Other Nonmetallic Mineral Product",
  "327M"  = "MFG - Cement, Concrete, Lime, Gypsum Product",
  
  # Manufacturing (Metals)
  "3313"  = "MFG - Alumina And Aluminum Production And Processing",
  "3314"  = "MFG - Nonferrous Metal (Except Aluminum) Production",
  "3315"  = "MFG - Foundries",
  "331M"  = "MFG - Iron And Steel Mills And Steel Product",
  "3321"  = "MFG - Forging And Stamping",
  "3322"  = "MFG - Cutlery And Handtool",
  "3327"  = "MFG - Machine Shops; Turned Product; Screw, Nut, Bolt",
  "3328"  = "MFG - Coating, Engraving, Heat Treating, Allied Activities",
  "33299M"= "MFG - Ordnance",
  "332M"  = "MFG - Architectural & Structural Metals, Boiler/Tank/Container",
  "332MZ" = "MFG - Misc Fabricated Metal Product",
  
  # Manufacturing (Machinery)
  "33311" = "MFG - Agricultural Implement",
  "3331M" = "MFG - Construction, Mining & Oil/Gas Field Machinery",
  "3333"  = "MFG - Commercial And Service Industry Machinery",
  "3335"  = "MFG - Metalworking Machinery",
  "3336"  = "MFG - Engine, Turbine, Power Transmission Equipment",
  "333MS" = "MFG - Machinery, N.E.C. Or Not Specified",
  
  # Manufacturing (Computers/Electronics)
  "3341"  = "MFG - Computer And Peripheral Equipment",
  "3345"  = "MFG - Navigational, Measuring, Electromedical Instruments",
  "334M1" = "MFG - Communications, Audio, Video Equipment",
  "334M2" = "MFG - Semiconductor, Magnetic/Optical Media, Other Electronic",
  
  # Manufacturing (Electrical/Transportation)
  "3352"  = "MFG - Household Appliance",
  "335M"  = "MFG - Electric Lighting And Electrical, Other Electrical N.E.C.",
  "33641M1"= "MFG - Aircraft, Aircraft Engine, And Aircraft Parts",
  "33641M2"= "MFG - Guided Missile And Space Vehicle, And Parts",
  "3365"  = "MFG - Railroad Rolling Stock",
  "3366"  = "MFG - Ship And Boat Building",
  "3369"  = "MFG - Other Transportation Equipment",
  "336M"  = "MFG - Motor Vehicles And Motor Vehicle Equipment",
  
  # Manufacturing (Other)
  "337"   = "MFG - Furniture And Related Product",
  "3391"  = "MFG - Medical Equipment And Supplies",
  "3399M" = "MFG - Sporting & Athletic Goods, Doll, Toy, Game",
  "3399ZM"= "MFG - Misc Manufacturing, N.E.C.",
  "33MS"  = "MFG - Not Specified Metal Industries",
  "3MS"   = "MFG - Not Specified Manufacturing Industries",
  
  # Wholesale Trade
  "4231"  = "WHL - Motor Vehicle & Parts Merchant Wholesalers",
  "4232"  = "WHL - Furniture & Home Furnishing Merchant Wholesalers",
  "4233"  = "WHL - Lumber & Construction Materials Merchant Wholesalers",
  "4234"  = "WHL - Professional & Commercial Equipment Wholesalers",
  "4235"  = "WHL - Metal & Mineral, Except Petroleum, Wholesalers",
  "4236"  = "WHL - Household Appliances & Electronic Goods Wholesalers",
  "4237"  = "WHL - Hardware, Plumbing, Heating Equipment Wholesalers",
  "4238"  = "WHL - Machinery, Equipment, Supplies Merchant Wholesalers",
  "42393" = "WHL - Recyclable Material Merchant Wholesalers",
  "4239Z" = "WHL - Misc Durable Goods, Except Recyclable, Wholesalers",
  "4241"  = "WHL - Paper & Paper Product Merchant Wholesalers",
  "4243"  = "WHL - Apparel, Piece Goods, Notions Merchant Wholesalers",
  "4244"  = "WHL - Grocery & Related Product Merchant Wholesalers",
  "4245"  = "WHL - Farm Product Raw Material Merchant Wholesalers",
  "4247"  = "WHL - Petroleum & Petroleum Products Merchant Wholesalers",
  "4248"  = "WHL - Beer, Wine, & Distilled Alcoholic Beverage Wholesalers",
  "42491" = "WHL - Farm Supplies Merchant Wholesalers",
  "4249Z" = "WHL - Misc Nondurable Goods Merchant Wholesalers",
  "424M"  = "WHL - Drugs, Druggists' Sundries, Chemical & Allied Wholesalers",
  "4251"  = "WHL - Wholesale Trade Agents And Brokers",
  "42S"   = "WHL - Not Specified Wholesale Trade",
  
  # Retail Trade
  "4411"  = "RET - Automobile Dealers",
  "4412"  = "RET - Other Motor Vehicle Dealers",
  "4413"  = "RET - Automotive Parts, Accessories, Tire Retailers",
  "44414" = "RET - Hardware Retailers",
  "4441Z" = "RET - Building Material & Supplies Dealers, Except Hardware",
  "4442"  = "RET - Lawn & Garden Equipment & Supplies Retailers",
  "44511" = "RET - Supermarkets & Other Grocery (No Convenience) Retailers",
  "44513" = "RET - Convenience Retailers & Vending Machine Operators",
  "4452"  = "RET - Specialty Food Retailers",
  "4453"  = "RET - Beer, Wine, And Liquor Retailers",
  "4491"  = "RET - Furniture & Home Furnishings Retailers",
  "4492"  = "RET - Electronics And Appliance Retailers",
  "4551"  = "RET - Department Stores",
  "4552"  = "RET - Warehouse Clubs, Supercenters, General Merch Retailers",
  "45611" = "RET - Pharmacies And Drug Retailers",
  "4561Z" = "RET - Health & Personal Care (Except Drug) Retailers",
  "4571"  = "RET - Gasoline Stations",
  "4572"  = "RET - Fuel Dealers",
  "4581"  = "RET - Clothing & Clothing Accessories Retailers",
  "4582"  = "RET - Shoe Retailers",
  "4583"  = "RET - Jewelry, Luggage, Leather Goods Retailers",
  "45913" = "RET - Sewing, Needlework, Piece Goods Retailers",
  "45914" = "RET - Musical Instrument & Supplies Retailers",
  "4591M" = "RET - Sporting Goods, Hobby, Toy, Game Retailers",
  "45921" = "RET - Book Retailers And News Dealers",
  "4593"  = "RET - Florists",
  "45941" = "RET - Office Supplies And Stationery Retailers",
  "45942" = "RET - Gift, Novelty, And Souvenir Retailers",
  "4595"  = "RET - Used Merchandise Retailers",
  "4599"  = "RET - Other Miscellaneous Retailers",
  "4MS"   = "RET - Not Specified Retail Trade",
  
  # Transportation
  "481"   = "TRN - Air Transportation",
  "482"   = "TRN - Rail Transportation",
  "483"   = "TRN - Water Transportation",
  "484"   = "TRN - Truck Transportation",
  "4853"  = "TRN - Taxi And Limousine Service",
  "485M"  = "TRN - Transit/Ground Passenger Transport (Except Taxi)",
  "486"   = "TRN - Pipeline Transportation",
  "487"   = "TRN - Scenic And Sightseeing Transportation",
  "488"   = "TRN - Support Activities For Transportation",
  "491"   = "TRN - Postal Service",
  "492"   = "TRN - Couriers And Messengers",
  "493"   = "TRN - Warehousing And Storage",
  
  # Information
  "5121"  = "INF - Motion Pictures And Video Industries",
  "5122"  = "INF - Sound Recording Industries",
  "51311" = "INF - Newspaper Publishers",
  "5131Z" = "INF - Periodical, Book, Directory/Mailing List, Other Publishers",
  "5132"  = "INF - Software Publishers",
  "516"   = "INF - Broadcasting And Content Providers",
  "517111"= "INF - Wired Telecommunications Carriers",
  "517Z"  = "INF - Telecommunications, Except Wired Carriers",
  "5182"  = "INF - Computing Infrastructure, Data Processing, Hosting",
  "51921" = "INF - Libraries And Archives",
  "51929" = "INF - Web Search Portals & All Other Information Services",
  
  # Finance / Insurance / Real Estate / Rental
  "522M"  = "FIN - Nondepository Credit Intermediation & Related",
  "5221M" = "FIN - Credit Unions, Savings Institutions, Other Depository",
  "5241"  = "FIN - Insurance Carriers",
  "5242"  = "FIN - Agencies, Brokerages, Other Insurance Related",
  "52M2"  = "FIN - Securities, Commodities, Funds, Trusts, Other Financial",
  "52M3"  = "FIN - Monetary Authorities-Central Bank, Commercial Banking",
  "531M"  = "FIN - Lessors Of Real Estate, Offices Of Real Estate Agents",
  "5313"  = "FIN - Activities Related To Real Estate",
  "5321"  = "FIN - Automotive Equipment Rental And Leasing",
  "532M2" = "FIN - Consumer Goods Rental, General Rental Centers",
  "53M"   = "FIN - Commercial & Industrial Machinery/Nonfinancial Assets Rental",
  
  # Professional / Scientific / Technical
  "5411"  = "PRF - Legal Services",
  "5412"  = "PRF - Accounting, Tax Prep, Bookkeeping, Payroll",
  "5413"  = "PRF - Architectural, Engineering, Related Services",
  "5414"  = "PRF - Specialized Design Services",
  "5415"  = "PRF - Computer Systems Design, Related Services",
  "5416"  = "PRF - Management, Scientific, Technical Consulting",
  "5417"  = "PRF - Scientific Research And Development Services",
  "5418"  = "PRF - Advertising, Public Relations, Related Services",
  "54194" = "PRF - Veterinary Services",
  "5419Z" = "PRF - Other Professional/Scientific/Technical (Except Vet)",
  "55"    = "PRF - Management Of Companies And Enterprises",
  "5613"  = "PRF - Employment Services",
  "5614"  = "PRF - Business Support Services",
  "5615"  = "PRF - Travel Arrangements And Reservation Services",
  "5616"  = "PRF - Investigation And Security Services",
  "56173" = "PRF - Landscaping Services",
  "5617Z" = "PRF - Services To Buildings & Dwellings (Except Landscape)",
  "561M"  = "PRF - Other Administrative & Support Services",
  "562"   = "PRF - Waste Management And Remediation Services",
  
  # Education
  "6111"  = "EDU - Elementary And Secondary Schools",
  "611M1" = "EDU - Junior Colleges, Colleges, Universities, Prof Schools",
  "611M2" = "EDU - Business, Trade & Tech Schools, Computer/Mgmt Training",
  "611M3" = "EDU - Other Schools/Instruction, Educational Support",
  
  # Health Care
  "6211"  = "MED - Offices Of Physicians",
  "6212"  = "MED - Offices Of Dentists",
  "62131" = "MED - Offices Of Chiropractors",
  "62132" = "MED - Offices Of Optometrists",
  "6213ZM"= "MED - Offices Of Other Health Practitioners",
  "6214"  = "MED - Outpatient Care Centers",
  "6216"  = "MED - Home Health Care Services",
  "621M"  = "MED - Other Health Care Services",
  "622M"  = "MED - Hospitals (General/Specialty Except Psych/Substance)",
  "6222"  = "MED - Psychiatric And Substance Abuse Hospitals",
  "6231"  = "MED - Nursing Care Facilities (Skilled Nursing)",
  "623M"  = "MED - Residential Care Facilities (Except Skilled Nursing)",
  
  # Social / Community Services
  "6241"  = "SCA - Individual And Family Services",
  "6242"  = "SCA - Community Food, Housing, Emergency/Other Relief",
  "6243"  = "SCA - Vocational Rehabilitation Services",
  "6244"  = "SCA - Child Care Services",
  
  # Entertainment / Recreation
  "7111"  = "ENT - Performing Arts Companies",
  "7112"  = "ENT - Spectator Sports",
  "711M"  = "ENT - Promoters Of Performing Arts/Sports, Agents/Managers",
  "7115"  = "ENT - Independent Artists, Writers, Performers",
  "712"   = "ENT - Museums, Historical Sites, Similar Institutions",
  "71395" = "ENT - Bowling Centers",
  "713Z"  = "ENT - Other Amusement, Gambling, Recreation Industries",
  "7211"  = "ENT - Traveler Accommodation",
  "721M"  = "ENT - RV Parks/Camps, Rooming/Boarding Houses, Dorms, Camps",
  "7224"  = "ENT - Drinking Places (Alcoholic Beverages)",
  "722Z"  = "ENT - Food Services And Drinking Places (Except Alcohol)",
  
  # Other Services (Repair, Personal, Etc.)
  "811192"= "SRV - Car Washes",
  "8111Z" = "SRV - Automotive Repair And Maintenance",
  "8112"  = "SRV - Electronic/Precision Equipment Repair And Maintenance",
  "8113"  = "SRV - Commercial/Industrial Machinery Repair And Maintenance",
  "8114"  = "SRV - Personal And Household Goods Repair And Maintenance",
  "812111"= "SRV - Barber Shops",
  "812112"= "SRV - Beauty Salons",
  "8121M" = "SRV - Nail Salons And Other Personal Care Services",
  "8122"  = "SRV - Death Care Services",
  "8123"  = "SRV - Drycleaning And Laundry Services",
  "8129"  = "SRV - Other Personal Services",
  "8131"  = "SRV - Religious Organizations",
  "81393" = "SRV - Labor Unions And Similar Labor Organizations",
  "8139Z" = "SRV - Business, Professional, Political, Similar Orgs",
  "813M"  = "SRV - Civic, Social, Advocacy Orgs, Grantmaking/Giving",
  "814"   = "SRV - Private Households",
  
  # Public Administration / Government
  "92113" = "ADM - Public Finance Activities",
  "92119" = "ADM - Other General Government And Support",
  "9211MP"= "ADM - Executive Offices And Legislative Bodies",
  "92M1"  = "ADM - Administration of Environmental Quality, Housing, Urban",
  "92M2"  = "ADM - Administration of Economic Programs, Space Research/Tech",
  "92MP"  = "ADM - Justice, Public Order, Safety Activities",
  "923"   = "ADM - Administration Of Human Resource Programs",
  "9281P" = "ADM - National Security And International Affairs",
  
  # Military
  "92811P1"= "MIL - U.S. Army",
  "92811P2"= "MIL - U.S. Air Force",
  "92811P3"= "MIL - U.S. Navy",
  "92811P4"= "MIL - U.S. Marines",
  "92811P5"= "MIL - U.S. Coast Guard",
  "92811P6"= "MIL - Armed Forces, Branch Not Specified",
  "92811P7"= "MIL - Military Reserves Or National Guard",
  
  # Unemployed
  "999920" = "Unemployed, No Work Experience In Last 5+ Years Or Never Worked"
)

philly_pums_data <- philly_pums_data %>%
  mutate(
    COW_label = recode(COW,!!!cow_labels),
    NATIVITY_label = recode(NATIVITY, !!!nativity_labels),
    CIT_label  = as.factor(recode(CIT, !!!cit_labels)),
    ESR_label  = as.factor(recode(ESR, !!!esr_labels)),
    SCHL_label = as.factor(recode(SCHL, !!!schl_labels)),
    INDP_label = as.factor(recode(INDP, !!!indp_labels)),
    NAICSP_label = as.factor(recode(NAICSP, !!!naicsp_labels)),
    POBP_label = as.factor(recode(POBP, !!!pobp_labels)),
    ENG_label = as.factor(recode(ENG, !!!eng_labels)),
    HHL_label = as.factor(recode(HHL, !!!hhl_labels)),
    WAOB_label = recode(as.character(WAOB), !!!waob_labels),
    HHLDRRAC1P_label = recode(as.character(HHLDRRAC1P), !!!hhl_drac_labels)
  )

sector_2digit <- function(naicsp_code_vector) {
  
  # 1. Extract first two characters from each NAICSP code
  #    (some codes are strings with letters, so use suppressWarnings on numeric conversion).
  first_two <- str_sub(naicsp_code_vector, 1, 2)
  num_first_two <- suppressWarnings(as.numeric(first_two))
  
  # 2. Use case_when for a fully vectorized approach
  dplyr::case_when(
    naicsp_code_vector %in% c("bbbbbbbb", "999920") ~ "N/A or Unemployed",
    num_first_two == 11 ~ "Agriculture, Forestry, Fishing, Hunting",
    num_first_two == 21 ~ "Mining, Quarrying, and Oil/Gas Extraction",
    num_first_two == 22 ~ "Utilities",
    num_first_two == 23 ~ "Construction",
    num_first_two %in% 31:33 ~ "Manufacturing",
    num_first_two == 42 ~ "Wholesale Trade",
    num_first_two %in% c(44, 45) ~ "Retail Trade",
    num_first_two %in% c(48, 49) ~ "Transportation and Warehousing",
    num_first_two == 51 ~ "Information",
    num_first_two == 52 ~ "Finance and Insurance",
    num_first_two == 53 ~ "Real Estate and Rental/Leasing",
    num_first_two == 54 ~ "Professional, Scientific, Technical Services",
    num_first_two == 55 ~ "Management of Companies and Enterprises",
    num_first_two == 56 ~ "Admin & Support, Waste Mgmt/Remediation Services",
    num_first_two == 61 ~ "Educational Services",
    num_first_two == 62 ~ "Health Care and Social Assistance",
    num_first_two == 71 ~ "Arts, Entertainment, and Recreation",
    num_first_two == 72 ~ "Accommodation and Food Services",
    num_first_two == 81 ~ "Other Services (except Public Admin)",
    num_first_two == 92 ~ "Public Administration",
    TRUE ~ "Other/Unknown NAICS"  # fallback if none match
  )
}

philly_pums_data <- philly_pums_data %>%
  mutate(
    industry_2digits = sector_2digit(NAICSP)
  )

philly_pums_data <- philly_pums_data %>%
  mutate(
    SCHL_label = case_when(
      SCHL_label %in% c("Some college, but less than 1 year", 
                        "Regular high school diploma", 
                        "Bachelor's degree", 
                        "GED or alternative credential", 
                        "Master's degree", 
                        "1+ years of college credit, no degree", 
                        "12th grade - no diploma", 
                        "Associate's degree", 
                        "Professional degree (beyond bachelor's)", 
                        "Doctorate degree", 
                        "K-11") ~ SCHL_label,
      SCHL_label %in% c("0") ~ "N/A (< 3 years old)",
      SCHL_label %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") ~ "K-11",
      TRUE ~ SCHL_label
    )
  )

philly_pums_data <- philly_pums_data %>%
  mutate(
    WAOB_label = case_when(
      # Latin America
      POBP %in% c(sprintf("%03d", 303), as.character(310:399)) ~
        "Latin America",
      
      # Africa
      POBP %in% as.character(400:499) ~
        "Africa",
      
      # Europe
      POBP %in% c(as.character(100:157), "160", as.character(162:199)) ~
        "Europe",
      
      # Asia
      POBP %in% c("158","159","161", as.character(200:299)) ~
        "Asia",
      
      # Northern America
      POBP %in% c(as.character(300:302), as.character(304:309)) ~
        "Northern America",
      
      # Oceania and at Sea
      POBP %in% c("060", as.character(500:554)) ~
        "Oceania and at Sea",
      
      # PR, US Island Areas, or US states
      POBP %in% as.character(1:59) ~
        "US state",
      
      POBP %in% as.character(61:99) ~
        "PR and US Island Areas",
      
      # Fallback
      TRUE ~ "Other/Unknown"
    )
  )
foreign_born <- philly_pums_data %>%
  filter(NATIVITY == 2)
glimpse(foreign_born) #5984
phillynative <-philly_pums_data %>%
  filter(NATIVITY_label == "Native")
phillyforeign <-philly_pums_data %>%
  filter(NATIVITY_label == "Foreign born")

##########################################################

# 1. Filter for labor force (employed) among natives and immigrants
labforce_natives <- phillynative %>% 
  filter(ESR_label %in% c("Civilian employed, at work",
                          "Civilian employed, with a job but not at work")) %>%
  mutate(age = as.numeric(AGEP))

labforce_foreign <- phillyforeign %>%
  filter(ESR_label %in% c("Civilian employed, at work",
                          "Civilian employed, with a job but not at work")) %>%
  mutate(age = as.numeric(AGEP))

# 2. Define age groups
age_breaks <- c(16, 25, 35, 45, 55, 65, Inf)
age_labels <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")

labforce_natives <- labforce_natives %>%
  mutate(age_group = cut(age, breaks = age_breaks, right = FALSE, labels = age_labels),
         group = "Philadelphia Workers")

labforce_foreign <- labforce_foreign %>%
  mutate(age_group = cut(age, breaks = age_breaks, right = FALSE, labels = age_labels),
         group = "Immigrant Workers")

# 3. Combine, then calculate % within each group
labforce_age <- bind_rows(labforce_natives, labforce_foreign)

labforce_age_summary <- labforce_age %>%
  group_by(group, age_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# 4. Pivot wider so each age group has separate columns for each group
labforce_age_wide <- labforce_age_summary %>%
  select(group, age_group, percent) %>%
  tidyr::pivot_wider(names_from = group, values_from = percent)

# 5. Make immigrant workers negative for left side of the chart
labforce_age_wide <- labforce_age_wide %>%
  mutate(`Immigrant Workers` = -`Immigrant Workers`)

# 6. Plot: a single chart with two bars (one negative, one positive), plus % labels
ggplot(labforce_age_wide, aes(y = fct_rev(age_group))) +
  # Immigrant bar (left)
  geom_bar(aes(x = `Immigrant Workers`, fill = "Immigrant Workers"),
           width = 0.5,
           stat = "identity") +
  # Native bar (right)
  geom_bar(aes(x = `Philadelphia Workers`, fill = "Philadelphia Workers"),
           width = 0.5,
           stat = "identity") +
  
  # --- ADDING THE PERCENTAGE LABELS ---
  # Immigrant label (in the middle of the negative bar)
  geom_text(
    aes(
      x = (`Immigrant Workers`)/2, 
      label = paste0(round(abs(`Immigrant Workers`), 1), "%")
    ),
    color = "white",
    size = 3,
    fontface="bold"
  ) +
  # Native label (in the middle of the positive bar)
  geom_text(
    aes(
      x = (`Philadelphia Workers`)/2, 
      label = paste0(round(`Philadelphia Workers`, 1), "%")
    ),
    color = "white",
    size = 3,
    fontface="bold"
  ) +
  # -------------------------------------

# Make x-axis show absolute values, but keep negative to the left
scale_x_continuous(
  labels = function(x) abs(x),
  expand = expansion(mult = c(0.05, 0.05))
) +
  
  scale_fill_manual(
    name = "Group Population",
    values = c("Immigrant Workers" = "#b5de2b",
               "Philadelphia Workers" = "#007abd")
  ) +
  
  labs(
    title = "Age Distribution of the Labor Force",
    subtitle = "Immigrant Workers vs. Philadelphia Workers",
    x = "Percentage (%)",
    y = "Age Group",
    caption = "Data Source: 2023 ACS 5-year PUMS",
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  mytheme2

ggsave("0_population_age_distribution.png",height = 4, width = 10, units = "in")
############################################################
#====================== world map showing birthplace
###########################################################

# birthplace_counts <- foreign_born %>%
#   group_by(POBP_label) %>%
#   summarise(count = n())
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# world_birthplace <- left_join(world, birthplace_counts, by = c("name" = "POBP_label")) %>%
#   select(name, count )
# mapview(world_birthplace, zcol = "count")
# st_write(world_birthplace, "world_birthplace.shp", driver = "ESRI Shapefile")
# 
# ggplot(data = world_birthplace) +
#   geom_sf(aes(fill = count), color = "NA", size = 0.3) +
#   scale_fill_gradientn(
#     colors = brewer.pal(9, "Blues"),
#     na.value = "#f2f2f2"
#   ) +
#   labs(title = "Foreign Born Immigrant Birthplaces",
#        subtitle = "A Snapshot of Immigrant in Philadelphia",
#        caption = "Data Source: 2023 ACS 5-year PUMS",
#        fill = "Immigrant Count") +
#   theme_minimal() +
#   mytheme2 +
#   theme(plot.background = element_rect(fill = "NA", color = NA),
#         legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
#         legend.position = c(0.05, 0.15),         # Legend inside at left bottom
#         legend.justification = c("left", "bottom"), # Anchors to the bottom-left
#         legend.direction = "vertical",
#         legend.key.size = unit(1, "line"))
# 
# ggsave("1_birthplace_map.png",height = 5, width = 10, units = "in")

#===========================================

waob_counts <- foreign_born %>%
  group_by(WAOB_label) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(waob_counts, aes(x = reorder(WAOB_label, count), y = percentage, fill = WAOB_label)) +
  geom_bar(stat = "identity", fill = "#73a8d6", width = 0.5) +
  coord_flip() +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Proportion of Immigrants by World Area of Birth",
    caption = "Data Source: 2023 ACS 5-year PUMS",
    x = NULL,
    y = "Percentage",
    fill = "World Area of Birth"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  mytheme2
ggsave("1_birth_area.png",height = 3, width = 10, units = "in")

################################
# Percentage Tree map ##########
df_treemap <- foreign_born %>%
  group_by(WAOB_label, POBP_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = 100 * count / sum(count))

ggplot(df_treemap, 
       aes(area = count,
           fill = WAOB_label,
           label = paste0(POBP_label, "\n", round(percentage, 1), "%"),
           subgroup = WAOB_label)) +
  geom_treemap(color = "white", size = 1) +
  geom_treemap_subgroup_border(color = "white", size = 3) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.7, 
                             color = "grey40", fontface = "bold", min.size = 0) +
  geom_treemap_text(colour = "white", place = "centre", fontface = "bold", reflow = TRUE, min.size = 0) +
  scale_fill_manual(values = c("#fde725", "#b5de2b", "#6ece58", "#35b779", "#26828e", "#007abd")) +
  labs(
    title = "Foreign-Born Population by World Area and Birthplace in Philadelphia",
    caption = "Data Source: 2023 ACS 5-year PUMS",
    fill = "World Area of Birth"
  ) +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "top")
ggsave("2_birthplace_treemap.png",height = 8, width = 10, units = "in")

###########################################################
#================Foreign born distribution ================
################# census analysis #########################

variables <- c(
  # 1. CORE IMMIGRANT CHARACTERISTICS 
  total_pop = "B05002_001E",
  total_foreign_born = "B05002_013E", 
  naturalized_citizens = "B05002_014E",
  non_citizens = "B05002_021E",
  
  # Year of Entry (B06007)
  entry_2010_or_later = "B06007_002E",
  entry_2000_2009 = "B06007_003E",
  entry_before_2000 = "B06007_004E",
  
  # 2. ECONOMIC INTEGRATION
  # Employment Status (B23001)
  labor_force_total = "B23001_001E",
  employed_total    = "B23001_007E",
  unemployed_total  = "B23001_008E",
  
  # Income (B19013, B19001)
  median_household_income = "B19013_001E",
  hh_income_less_10k = "B19001_002E",
  hh_income_10k_15k  = "B19001_003E",
  hh_income_15k_25k  = "B19001_004E",
  
  # 3. SOCIAL INTEGRATION
  # Language (B16001)
  speak_only_english    = "B16001_002E",
  speak_spanish         = "B16001_003E",
  speak_spanish_lep     = "B16001_004E",
  
  # Transportation (B08006)
  total_workers  = "B08006_001E",
  drove_alone    = "B08006_002E",
  public_transit = "B08006_008E",
  walked         = "B08006_015E",
  other_transport= "B08006_016E",
  work_at_home   = "B08006_017E",
  
  # Housing (B25003, B25014)
  housing_total    = "B25003_001E",
  owner_occupied   = "B25003_002E",
  renter_occupied  = "B25003_003E",
  housing_overcrowded = "B25014_007E",
  
  # 4. GEOGRAPHIC MOBILITY (B07001)
  same_house         = "B07001_017E",
  moved_within_county= "B07001_033E",
  moved_within_state = "B07001_049E",
  moved_from_state   = "B07001_065E", 
  moved_from_abroad  = "B07001_081E",
  
  # 5. Ethnicity
  race_white  = "B02001_002E",
  race_black  = "B02001_003E",
  race_ai_an  = "B02001_004E",
  race_asian  = "B02001_005E",
  race_nhpi   = "B02001_006E",
  race_other  = "B02001_007E"
)

philly_data <- get_acs(
  geography = "tract",
  variables = variables,
  state = "PA",
  county = "Philadelphia",
  year = 2023,        # or 2022 if 2023 is not yet available
  geometry = TRUE,
  output = "wide"# get SF polygons for tracts
)

philly_data <- philly_data %>%
  mutate(
    # Immigrant status
    pct_foreign_born = (total_foreign_born / total_pop) * 100,
    pct_naturalized = (naturalized_citizens / total_foreign_born) * 100,
    pct_noncitizen  = (non_citizens / total_foreign_born) * 100,
    
    # Labor force
    labor_force_participation = (labor_force_total / total_pop) * 100,
    unemployment_rate = (unemployed_total / labor_force_total) * 100,
    
    # Housing
    homeownership_rate = (owner_occupied / housing_total) * 100,
    
    # Transportation
    pct_public_transit = (public_transit / total_workers) * 100,
    
    # Mobility
    pct_moved_abroad = (moved_from_abroad / total_pop) * 100,
    pct_same_house   = (same_house / total_pop) * 100
  )
sf <- pumas(state = "PA", year = 2023) %>% 
  filter(PUMACE20 %in% c(
    "03216", "03221", "03222", "03223", "03224", 
    "03225", "03227", "03228", "03229", "03230", "03231"
  ))

philly_pumas_sf <- pumas(state = "PA", year = 2023) %>% 
  filter(PUMACE20 %in% c(
    "03216", "03221", "03222", "03223", "03224", 
    "03225", "03227", "03228", "03229", "03230", "03231"
  ))

puma_summary <- foreign_born %>%
  group_by(PUMA) %>%
  summarize(
    immigrant_density = sum(PWGTP),
    total_weight = sum(PWGTP, na.rm = TRUE),   # Weighted count
    avg_wage     = mean(WAGP, na.rm = TRUE),    # Average wage
    total_fb    = sum(PWGTP, na.rm = TRUE),  # Total foreign-born count
    labforce_fb = sum(PWGTP * if_else(ESR_label %in% "Not in labor force", 1, 0), na.rm = TRUE),
    unemp_fb    = sum(PWGTP * if_else(ESR_label == "Unemployed", 1, 0), na.rm = TRUE)
  ) %>%
  mutate(
    immigrant_lfpr       = if_else(total_fb > 0, labforce_fb / total_fb * 100, NA_real_),
    immigrant_unemp_rate = if_else(labforce_fb > 0, unemp_fb / labforce_fb * 100, NA_real_)
  ) %>%
  ungroup()

philly_pumas_joined <- philly_pumas_sf %>%
  left_join(puma_summary, by = c("PUMACE20" = "PUMA")) 

tract_puma_df <- st_join(
  x = philly_data,          # the tract-level sf with geometry
  y = philly_pumas_sf,      # the PUMA polygons
  join = st_intersects,
  largest = TRUE
)

ggplot() +
  geom_sf(
    data = philly_data,
    aes(fill = pct_foreign_born),
    color = "white",  # each tract has a thin white boundary
    size = 0.1
  ) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(
    title = "Map1: Foreign Born Individuals Distribution in Philadelphia",
    subtitle = "Percentage of Immigrant Population by Census Tract",
    caption = "Data Source: 2023 ACS 5-year Census",
    fill = "% Foreign Born"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(size = 8)) +
  mytheme2

ggsave("3_foreignborn_percentage.png",height = 8, width = 10, units = "in")

st_write(philly_data, "philly_foreignborn_data.shp", driver = "ESRI Shapefile")
mapview(philly_data, zcol = "pct_foreign_born")

##############################################
#====== Living Condition: Wages and Poverty
##############################################
foreign_born_mod <- foreign_born %>%
  mutate(
    NP_num   = as.numeric(NP),
    wage_num = as.numeric(WAGP),
    # 2025 HHS poverty guidelines for 48 states (including DC)
    poverty_line = case_when(
      NP_num == 1 ~ 15650,
      NP_num == 2 ~ 21150,
      NP_num == 3 ~ 26650,
      NP_num == 4 ~ 32150,
      NP_num == 5 ~ 37650,
      NP_num == 6 ~ 43150,
      NP_num == 7 ~ 48650,
      NP_num == 8 ~ 54150,
      NP_num == 9 ~ 59650,
      NP_num == 10 ~ 65150,
      NP_num > 10  ~ 65150 + (NP_num - 9) * 5500,
      TRUE ~ NA_real_
    ),
    ratio       = wage_num / poverty_line,
    poverty_cat = if_else(ratio < 1, "Below Poverty", "At/Above Poverty")
  ) %>%
  filter(!is.na(poverty_line) & wage_num >= 0)

poverty_summary <- foreign_born_mod %>%
  group_by(NP_num, poverty_cat) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(NP_num) %>%
  mutate(percent = total_weight / sum(total_weight) * 100) %>%
  ungroup()

ggplot(poverty_summary, aes(x = factor(NP_num), y = total_weight, fill = poverty_cat)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  # Matched colors to your previous chart image (Blue = Above, Green = Below)
  scale_fill_manual(
    values = c("At/Above Poverty" = "#007abd", 
               "Below Poverty" = "#b5de2b"),
    guide = guide_legend(reverse = TRUE) # Ensures legend order matches stack order
  ) +
  labs(
    title = "Poverty Status of Foreign-Born Residents by Household Size",
    subtitle = "Based on U.S. Federal Poverty Guidelines to Determine Financial Eligibility",
    x = "Number of Persons in Household",
    y = "Proportion", # Changed from "Percentage (%)"
    fill = "Poverty Status",
    caption = "Data Source: 2023 ACS 5-year PUMS"
  ) +
  theme_minimal() +
  mytheme2 + # Commented out as the definition is not provided
  coord_flip() +
  theme(
    legend.position = "top",
    axis.title.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", angle = 90, size = 10, face = "bold")
  )

ggsave("1_foreignborn_by_householdsize_poverty.png", 
       height = 4, width = 8, units = "in")

###############################
# education attainment
foreign_born_labor <- philly_pums_data %>%
  filter(NATIVITY_label == "Foreign born",
         ESR_label %in% c("Civilian employed, at work", 
                          "Civilian employed, with a job but not at work"))

# Summarize by education level
education_prop <- foreign_born_labor %>%
  group_by(SCHL_label) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  mutate(proportion = total_weight / sum(total_weight) * 100)

# Reorder SCHL_label based on proportion in descending order
education_prop <- education_prop %>%
  mutate(SCHL_label = fct_reorder(SCHL_label, proportion, .desc = TRUE))


ggplot(education_prop, aes(area = total_weight, fill = SCHL_label,
                           label = paste0(SCHL_label, "\n", round(proportion, 1), "%"))) +
  geom_treemap() +
  geom_treemap_text(color = "white", fontface = "bold", size = 10, place = "centre", grow = TRUE) +
  labs(
    title = "Educational Attainment Proportion",
    subtitle = "Among Foreign-Born Labor Force",
    caption = "Data Source: 2023 ACS 5-year PUMS"
  ) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "none")

ggsave("1_foreignborn_laborforce_education_treemap_sorted.png", 
       height = 4, 
       width = 8, units = "in")
######################################################
#========= Industry Distribution by Percentage
######################################################

labforce <- philly_pums_data %>% 
  filter(ESR_label %in% c("Civilian employed, at work", 
                          "Civilian employed, with a job but not at work"))

# Summarize weighted counts by industry and nativity
industry_labforce <- labforce %>%
  group_by(industry_2digits, NATIVITY) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(industry_2digits) %>%
  mutate(percent = total_weight / sum(total_weight) * 100) %>%
  ungroup() %>%
  mutate(nativity_label = if_else(NATIVITY == 1, "Native", "Foreign Born"))

# Sort industries by the percentage share of foreign-born workers (descending)
industry_order <- industry_labforce %>% 
  filter(nativity_label == "Foreign Born") %>% 
  arrange(desc(percent)) %>% 
  pull(industry_2digits)

industry_labforce <- industry_labforce %>%
  mutate(industry_2digits = factor(industry_2digits, levels = industry_order))

# Define your color palette (selecting two colors for nativity groups)
color_palette <- c("Foreign Born" = "#007abd", "Native" = "#b5de2b")
ggplot(industry_labforce, aes(x = industry_2digits, y = percent, fill = nativity_label)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold",
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Immigrant Proportions by Industry among the Labor Force",
    x = "Industry Sector",
    y = "Percentage (%)",
    caption = "Data Source: 2023 ACS 5-year PUMS",
    fill= "Nativity"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  mytheme2 +
  labs(color=NULL)

ggsave("2.1_Immigrant Proportions by Industry.png",height = 8, width = 10, units = "in")



cow_labforce <- labforce %>%
  group_by(COW_label, NATIVITY) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(COW_label) %>%
  mutate(percent = total_weight / sum(total_weight) * 100) %>%
  ungroup() %>%
  mutate(nativity_label = if_else(NATIVITY == 1, "Native", "Foreign Born"))

# 3. Sort COW categories by the percentage share of Foreign Born (descending)
cow_order <- cow_labforce %>%
  filter(nativity_label == "Foreign Born") %>%
  arrange(desc(percent)) %>%
  pull(COW_label)

cow_labforce <- cow_labforce %>%
  mutate(COW_label = factor(COW_label, levels = cow_order))

ggplot(cow_labforce, aes(x = COW_label, y = percent, fill = nativity_label)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold",
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Labor Force Composition Across Worker Categories",
    x = "Class of Worker (COW)",
    y = "Percentage (%)",
    caption = "Data Source: 2023 ACS 5-year PUMS",
    fill = "Nativity"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  mytheme2 
ggsave("2.2_Labor Force Composition Across Worker Categories.png",height = 4, width = 10, units = "in")


####
# 1. Create Immigrant Count Data (Absolute Numbers)
df_count <- labforce %>%
  filter(NATIVITY == 2) %>%  # foreign-born only
  group_by(industry_2digits) %>%
  summarize(immigrant_count = sum(PWGTP, na.rm = TRUE), .groups = "drop")

# 2. Create Proportion Data (Stacked: Native vs. Foreign)
df_prop <- labforce %>%
  group_by(industry_2digits, NATIVITY) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(industry_2digits) %>%
  mutate(percent = total_weight / sum(total_weight) * 100) %>%
  ungroup() %>%
  mutate(nativity_label = if_else(NATIVITY == 1, "Native", "Foreign Born"))

# 3. Define Industry Order (using descending immigrant count)
industry_order <- df_count %>%
  arrange(desc(immigrant_count)) %>%
  pull(industry_2digits)

# Ensure both data frames use the same factor ordering
df_count <- df_count %>%
  mutate(industry_2digits = factor(industry_2digits, levels = industry_order))

df_prop <- df_prop %>%
  mutate(industry_2digits = factor(industry_2digits, levels = industry_order))
foreign_order <- df_prop %>%
  filter(nativity_label == "Foreign Born") %>%
  arrange(desc(percent)) %>%
  pull(industry_2digits)
df_prop <- df_prop %>%
  mutate(industry_2digits = factor(industry_2digits, levels = foreign_order))
# 4. Define your color palette for nativity (for proportion chart)
color_palette <- c("Foreign Born" = "#007abd", "Native" = "#b5de2b")

# --- Plot 1: Absolute Immigrant Count by Industry ---
p1 <- ggplot(df_count, aes(x = immigrant_count, y = industry_2digits)) +
  geom_col(fill = "#007abd", width = 0.6) +
  geom_text(aes(label = comma(immigrant_count)),
            hjust = -0.1, size = 3, fontface = "bold") +
  # 1) Expand the x-axis scale so there's room for labels:
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.2)) # 0.2 = 20% extra space on the right
  ) +
  # 2) Turn off clipping so text can extend into the margin:
  coord_cartesian(clip = "off") +
  labs(
    x = "Number of Immigrant Workers",
    y = "Industry",
    caption = "Data Source: 2023 ACS 5-year PUMS"
  ) +
  theme_minimal() +
  mytheme2 +
  theme(
    # 3) Add margin on the right to accommodate text
    plot.margin = margin(r = 60),
    axis.text.y = element_blank()
  )

# --- Plot 2: Proportion Stacked Bar (Native vs. Foreign) ---
p2 <- ggplot(df_prop, aes(x = percent, y = industry_2digits, fill = nativity_label)) +
  geom_bar(stat = "identity", width = 0.6, position = "stack") +
  geom_text(aes(label = paste0(round(percent,1),"%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Proportion of Immigrant Workers and Employment by Industry",
    x     = "Percentage (%)",
    y     = NULL,
    fill  = "Nativity"
  ) +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "top")

# --- Combine the Two Plots Side by Side ---
combined_plot <- p2 + plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "top")

combined_plot
ggsave("2.3_Combined_Industry_Employment_and_Proportion.png", combined_plot,
       height = 8, width = 10, units = "in")

###############################################################################
#Relationship Between Birthplace and English Proficiency
#=================================

top_50_df <- foreign_born %>%
  count(POBP_label, wt = PWGTP, name = "weighted_n") %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 30)

top_50_pob <- top_50_df %>% pull(POBP_label)

# Summarize by ENG_label and compute percentages for these top 25 places
df_eng_by_pob <- foreign_born %>%
  filter(POBP_label %in% top_50_pob) %>%
  group_by(POBP_label, ENG_label) %>%
  summarize(total_weight = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(POBP_label) %>%
  mutate(percentage = total_weight / sum(total_weight)) %>%
  ungroup() %>%
  # Reorder the birthplace factor using the order from top_25_pob (descending)
  mutate(POBP_label = factor(POBP_label, levels = top_50_pob))

# # Define custom color palette
# color_palette_eng <- c(
#   "#f2b950",  # gold-ish
#   "#73a8d6",  # light blue
#   "#c95926",  # brown-ish
#   "#007abd",  # bold blue
#   "#bfdef5",  # pale blue
#   "#13456e",  # deep navy
#   "#c0772a"   # earthy orange
# )
viridis_colors <- c(#"#fde725", 
  "#b5de2b", 
  "#6ece58",
  "#35b779", 
  #"#1f9e89", 
  "#26828e", 
  "#31688e",
  "#3e4989"
  )
# Facet-wrapped bar chart with percentage on y-axis
ggplot(df_eng_by_pob, aes(x = ENG_label, y = percentage, fill = ENG_label)) +
  geom_col() +
  facet_wrap(~ POBP_label, scales = "fixed", ncol = 5) +
  labs(
    x = "English Ability",
    y = "Percentage",
    title = "English Proficiency Across Top 30 Immigrant Birthplaces in Philadelphia",
    caption = "Data Source: 2023 ACS 5-year PUMS",
    fill = "English Proficiency"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  mytheme2 +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8)
  ) +
  scale_fill_manual(values = viridis_colors) +
  coord_flip()

ggsave("4_english_proficiency.png",
       height=8, width = 10, units = "in")

#employment status and english proficiency chart
foreign_born_employed <- foreign_born %>%
  filter(ESR_label %in% c("Civilian employed, at work", 
                          "Civilian employed, with a job but not at work"))

# Calculate average wage by English proficiency
wage_by_eng <- foreign_born_employed %>%
  group_by(ENG_label) %>%
  summarize(
    mean_wage = mean(WAGP, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
wage_diff <- wage_by_eng %>%
  filter(ENG_label %in% c("Very well", "Not well")) %>%
  select(ENG_label, mean_wage) %>%
  spread(key = ENG_label, value = mean_wage)
diff_value <- wage_diff$`Very well` - wage_diff$`Not well`
#36075.51

ggplot(wage_by_eng, aes(x = fct_reorder(ENG_label, mean_wage), y = mean_wage, fill = ENG_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = dollar(mean_wage)), vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = viridis_colors) +
  labs(
    title = "Average Wage by English Proficiency for Foreign-Born Workers",
    subtitle = paste("Those speaking 'Not well' earn about", dollar(round(diff_value, 0)),
                     "less per year on average than those speaking 'Very well'."),
    x = "English Proficiency",
    y = "Average Wage (USD)",
    caption = "Data Source: 2023 ACS 5-year PUMS"
  ) +
  theme_minimal() +
  mytheme2 +
  coord_flip()
# # A tibble: 5 × 3
# ENG_label          mean_wage count
# <fct>                  <dbl> <int>
# 2 Not at all            28802.   204
# 3 Not well              30712.   599
# 4 Very well             66787.  1198
# 5 Well                  37983.   767
# Calculate total weighted count of immigrants
total_weight <- foreign_born %>%
  summarize(total = sum(as.numeric(PWGTP), na.rm = TRUE)) %>%
  pull(total)

# Calculate weighted count for those who "don't speak English well"
dont_speak_well_weight <- foreign_born %>%
  filter(ENG_label %in% c("Not well", "Not at all")) %>%
  summarize(total = sum(as.numeric(PWGTP), na.rm = TRUE)) %>%
  pull(total)

# Calculate weighted count for those who "cannot speak English at all"
cannot_speak_weight <- foreign_born %>%
  filter(ENG_label == "Not at all") %>%
  summarize(total = sum(as.numeric(PWGTP), na.rm = TRUE)) %>%
  pull(total)

# Compute proportions in percentage
prop_dont_speak_well <- (dont_speak_well_weight / total_weight) * 100
prop_cannot_speak <- (cannot_speak_weight / total_weight) * 100

prop_dont_speak_well #30.7224
prop_cannot_speak #10.66546

df_status_eng <- foreign_born %>%
  filter(!is.na(ENG_label), 
         !is.na(ESR_label),
         ENG_label != "N/A (<5 years old)") %>%  # remove N/A (<5 years old)
  group_by(ENG_label, ESR_label) %>%
  summarize(weight = sum(as.numeric(PWGTP), na.rm = TRUE), .groups = "drop") %>%
  group_by(ENG_label) %>%
  mutate(total_for_ENG = sum(weight),
         percent = weight / total_for_ENG * 100) %>%
  ungroup() %>%
  mutate(ENG_label = fct_reorder(ENG_label, total_for_ENG, .desc = TRUE))

ggplot(df_status_eng, aes(x = ENG_label, y = percent, fill = ESR_label)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        round(percent, 1) < 3,      # If segment < 3%, skip label
        "",
        paste0(round(percent, 1), "%")
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 2.8                      # Slightly smaller text
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(values = viridis_colors) +
  labs(
    title = "Employment Status by English Proficiency",
    x = "English Proficiency",
    y = "Percentage (%)",
    fill = "Employment Status",
    caption = "Data Source: 2023 ACS 5-year PUMS"
  ) +
  theme_minimal() +
  mytheme2 +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  coord_flip()

ggsave("4_english_proficiency_employment.png",
       height=3, width = 10, units = "in")
#================================ESL Program locations
# Accessibility analysis
######################################################
esl <- st_read("C:/Users/hliu/Desktop/Data@Request/202411_immigrants/ESL (English as a Second Language) Class Locations/esl_class_locations.shp")
philly_tracts <- tracts(state = "PA", county = "Philadelphia", year = 2024)
philly_boundary <- st_union(philly_tracts)
philly_centroids <- st_centroid(philly_tracts)
esl <- st_transform(esl, st_crs(philly_centroids))

ggplot() +
  geom_sf(data = philly_tracts, fill = "gray90", color = "white", size = 0.2) +
  geom_sf(data = esl, aes(color = "ESL Class Location"), size = 2, shape = 21, fill = "#007abd") +
  scale_color_manual(values = "#007abd") +
  labs(
    title = "ESL Program Locations in Philadelphia",
    subtitle = "Locations of ESL Classes",
    caption = "Data Source: Opendataphilly",
    color = "Location Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 8)
  ) + mytheme2

ggsave("4_esl_locations.png", height = 6, width = 10, units = "in")

distance_matrix <- st_distance(philly_centroids, esl)
philly_centroids$min_distance <- apply(distance_matrix, 1, min)
philly_tracts$min_distance <- philly_centroids$min_distance
philly_tracts <- philly_tracts %>%
  left_join(st_drop_geometry(philly_data) %>% select(GEOID, pct_foreign_born), by = "GEOID")
cor(philly_tracts$min_distance, philly_tracts$pct_foreign_born, use = "complete.obs")

#-0.13

# Remove geometry for plotting
philly_df <- st_drop_geometry(philly_tracts)

ggplot(philly_df, aes(x = pct_foreign_born, y = min_distance)) +
  geom_point(alpha = 0.8, color = "#b5de2b") +
  geom_smooth(method = "lm", se = TRUE, color = "#73a8d6") +
  labs(title = "Relationship between Foreign-born % and ESL Accessibility",
       subtitle = "Census Tracts in Philadelphia",
       x = "Foreign-born Percentage",
       y = "Distance to Nearest ESL (m)",
       caption = "Data Source: 2023 ACS 5-year PUMS & Opendataphilly") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  mytheme2
ggsave("5_english_proficiency.png",
       height=6, width = 10, units = "in")

#==================examine the autocorrelation
library(corrplot)
num_df <- foreign_born %>%
  select(
    Age                       = AGEP,
    `Annual Wages & Salaries` = WAGP,
    `Income-to-Poverty Ratio` = POVPIP,
    `Household Size`          = NP,
    `Year of Entry to US`     = YOEP
  ) %>%
  # IMPORTANT: The POVPIP variable uses -1 for N/A. We must convert it.
  mutate(`Income-to-Poverty Ratio` = na_if(`Income-to-Poverty Ratio`, -1))

schl_dummies <- foreign_born %>%
  mutate(Education = fct_recode(SCHL,
                                "Edu: < HS Diploma" = "01", "Edu: < HS Diploma" = "02", "Edu: < HS Diploma" = "03",
                                "Edu: < HS Diploma" = "04", "Edu: < HS Diploma" = "05", "Edu: < HS Diploma" = "06",
                                "Edu: < HS Diploma" = "07", "Edu: < HS Diploma" = "08", "Edu: < HS Diploma" = "09",
                                "Edu: < HS Diploma" = "10", "Edu: < HS Diploma" = "11", "Edu: < HS Diploma" = "12",
                                "Edu: < HS Diploma" = "13", "Edu: < HS Diploma" = "14", "Edu: < HS Diploma" = "15",
                                "Edu: HS Diploma/GED" = "16", "Edu: HS Diploma/GED" = "17",
                                "Edu: Some College/Assoc." = "18", "Edu: Some College/Assoc." = "19", "Edu: Some College/Assoc." = "20",
                                "Edu: Bachelor's or Higher" = "21", "Edu: Bachelor's or Higher" = "22",
                                "Edu: Bachelor's or Higher" = "23", "Edu: Bachelor's or Higher" = "24"
  )) %>%
  select(Education) %>%
  model.matrix(~ Education - 1, data = .) %>%
  as.data.frame()

eng_dummies <- model.matrix(~ ENG_label - 1, data = foreign_born) %>%
  as.data.frame() %>%
  select(
    `ENG: Very Well` = `ENG_labelVery well`,
    `ENG: Well` = `ENG_labelWell`,
    `ENG: Not Well` = `ENG_labelNot well`,
    `ENG: Not at All` = `ENG_labelNot at all`
  )

esr_dummies <- foreign_born %>%
  mutate(Employment_Status = case_when(
    ESR_label %in% c("Civilian employed, at work", "Civilian employed, with a job but not at work", "Armed forces, at work") ~ "ESR: Employed",
    ESR_label == "Unemployed" ~ "ESR: Unemployed",
    ESR_label == "Not in labor force" ~ "ESR: Not in Labor Force",
    TRUE ~ NA_character_ # This will handle "N/A (less than 16 years old)" and any other cases
  )) %>%
  filter(!is.na(Employment_Status)) %>%
  select(Employment_Status) %>%
  model.matrix(~ Employment_Status - 1, data = .) %>%
  as.data.frame()

valid_rows <- which(
  !foreign_born$ESR_label %in% c("N/A (less than 16 years old)") &
    !is.na(foreign_born$ESR_label)
)

num_df_aligned <- num_df[valid_rows, ]
schl_dummies_aligned <- schl_dummies[valid_rows, ]
eng_dummies_aligned <- eng_dummies[valid_rows, ]
esr_dummies_aligned <- esr_dummies # This was already filtered, so it's aligned now

combined_df <- bind_cols(num_df_aligned, schl_dummies_aligned, eng_dummies_aligned, esr_dummies_aligned)
corr_full <- cor(combined_df, use = "pairwise.complete.obs")

# Isolate the relationships we care about: numeric vs. categorical
vars_num    <- names(num_df)
vars_binary <- c(names(schl_dummies), names(eng_dummies), names(esr_dummies))
corr_sub    <- corr_full[vars_num, vars_binary]

corr_df_plot <- as.data.frame(corr_sub) %>%
  mutate(Numeric_Indicator = rownames(.)) %>%
  pivot_longer(-Numeric_Indicator, names_to = "Categorical_Factor", values_to = "Correlation") %>%
  filter(!is.na(Correlation)) %>%
  mutate(
    Numeric_Indicator = factor(Numeric_Indicator, levels = rev(vars_num)),
    Categorical_Factor = factor(Categorical_Factor, levels = vars_binary)
  )

# Plot with your color ramp and mytheme2
ggplot(corr_df_plot, aes(x = Numeric_Indicator, y = Categorical_Factor, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.5, color = "black") +
  scale_fill_gradient2(
    low      = "#6ece58", # Orange for negative
    mid      = "white",
    high     = "#0072B2", # Blue for positive
    midpoint = 0,
    name     = "Correlation (ρ)"
  ) +
  labs(
    title    = "Individual-Level Drivers of Immigrant Economic Outcomes",
    subtitle = "Correlations between personal attributes and economic indicators in Philadelphia",
    x        = "Personal Characteristics (Education, English Proficiency, Employment Status)",
    y        = "Economic Indicators",
    caption  = "Source: 2023 ACS 5-year PUMS, individual-level data for population 16+"
  ) +
  theme_minimal(base_family = "sans") +
  mytheme2 +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 1)) 



ggplot(corr_df, aes(x = var2, y = var1, fill = corr)) +
  geom_tile(color = "white") +
  geom_text(aes(label = corr), size = 3) +
  scale_fill_gradient2(
    low    = "#6ece58", 
    mid    = "white", 
    high   = "#31688e", 
    midpoint = 0,
    name   = "Pearson"
  ) +
  labs(
    title    = "Correlation Matrix: Socioeconomic Indicators",
    subtitle = "Foreign‐born Population in Philadelphia County (ACS 2023)",
    x        = NULL,
    y        = NULL,
    caption  = "Source: 2023 ACS 5-year PUMS"
  ) +
  mytheme2 +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("5_correlation.png",
       height=8, width = 10, units = "in")

#+====================
# 1. Prep data: simplify ESR, drop raws with missing
fb2 <- foreign_born %>%
  mutate(
    ESR_simple = case_when(
      ESR_label %in% c("Civilian employed, at work",
                       "Civilian employed, with a job but not at work",
                       "Armed forces, at work") ~ "Employed",
      ESR_label == "Unemployed"       ~ "Unemployed",
      TRUE                            ~ "Not in LF"
    ) %>% fct_relevel("Not in LF","Employed","Unemployed")
  ) %>%
  filter(!is.na(ESR_simple))

# 2. Auto-detect predictors (drop IDs & original ESR)
omit <- c("SERIALNO","ESR","ESR_label","PWGTP")  
predictors <- setdiff(names(fb2), c(omit, "ESR_simple"))
zero_var <- sapply(fb2[predictors], function(x) length(unique(x)) < 2)
print(predictors[zero_var])
predictors2 <- predictors[!zero_var]
# 3. Ensure chars → factors
fml2 <- reformulate(predictors2, response = "ESR_simple")
multi_all2 <- multinom(
  fml2,
  data    = fb2,
  weights = fb2$PWGTP,
  MaxNWts = 10000,    # raise from the default ~1000
  trace   = FALSE
)
summary(multi_all2)


#============================
poverty_foreign_born <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = c(
    fb_poverty_universe = "B05010_009E", # Universe: Foreign-born for whom poverty is determined
    fb_below_poverty = "B05010_010E"     # Count: Foreign-born with income below poverty level
  ),
  year = 2023,
  output = "wide"
) %>%
  mutate(
    # Calculate the poverty rate specifically for the foreign-born population
    pct_poverty_foreign_born = ifelse(fb_poverty_universe > 0, fb_below_poverty / fb_poverty_universe, 0)
  ) %>%
  select(GEOID, pct_poverty_foreign_born)

lang_isolation_data <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = c(
    total_hh = "C16002_001E",
    iso_spanish = "C16002_004E",
    iso_indo_euro = "C16002_007E",
    iso_api = "C16002_010E",
    iso_other = "C16002_013E"
  ),
  year = 2023,
  output = "wide"
) %>%
  mutate(
    total_isolated_hh = iso_spanish + iso_indo_euro + iso_api + iso_other,
    pct_lang_isolated = ifelse(total_hh > 0, total_isolated_hh / total_hh, 0)
  ) %>%
  select(GEOID, pct_lang_isolated)

philly_data_enhanced <- philly_data %>%
  left_join(poverty_foreign_born, by = "GEOID") %>%
  left_join(lang_isolation_data, by = "GEOID")

ggplot(data = philly_data_enhanced) +
  geom_sf(aes(fill = pct_poverty_foreign_born), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "viridis", 
    name = "Poverty Rate\n(Foreign-Born)",
    labels = scales::percent_format(accuracy = 1),
    direction = -1 # Reverse to make high poverty darker
  ) +
  labs(
    title = "The Geography of Immigrant Poverty in Philadelphia",
    subtitle = "Poverty rate for the foreign-born population by census tract",
    caption = "Source: 2019-2023 ACS 5-Year Estimates. Darker areas indicate higher poverty."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0.5),
    legend.position = "right"
  )

ggplot(data = philly_data_enhanced) +
  geom_sf(aes(fill = pct_lang_isolated), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "viridis", 
    direction = -1,
    name = "% Linguistically\nIsolated Households",
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Map4: The Geography of Language Isolation in Philadelphia",
    subtitle = "Percentage of households where no one over 14 speaks English 'very well'",
    caption = "Data Source: 2019-2023 ACS 5-Year Estimates. Darker areas indicate higher isolation."
  ) +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "top")

ggsave("6_Language_Isolation.png",height = 8, width = 10, units = "in")



foreign_born_earnings <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  variables = c(median_earnings_fb = "B20017_003E"), # THIS IS THE CORRECTED VARIABLE
  year = 2023, 
  output = "wide"
) %>%
  select(GEOID, median_earnings_fb)

philly_data_enhanced <- left_join(philly_data_enhanced, foreign_born_earnings, by = "GEOID")

ggplot(data = philly_data_enhanced) +
  # Draw the census tracts, filling them based on median earnings
  geom_sf(aes(fill = median_earnings_fb.y), color = "white", linewidth = 0.1) +
  
  # Use an intuitive color palette for income. 'Magma' works well.
  # Yellow/light colors represent higher earnings, dark colors represent lower.
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Median Personal\nEarnings (Foreign-Born)",
    labels = scales::dollar_format(prefix = "$", scale = 1/1000, suffix = "k"), # Format as $30k, $50k etc.
    na.value = "grey80" # Use a neutral grey for tracts with no data
  ) +
  
  # Add titles and captions
  labs(
    title = "Map3: The Geography of Immigrant Earnings in Philadelphia",
    subtitle = "Median personal earnings for the foreign-born population by census tract",
    caption = "Data Source: 2019-2023 ACS 5-Year Estimates. Grey areas indicate no value."
  ) +
  theme_minimal() +
  mytheme2 +
  theme(legend.position = "top",
        legend.key.height= unit(0.4, 'cm'),
        legend.key.width= unit(1, 'cm')) 
ggsave("7_median_earnings.png",height = 8, width = 10, units = "in")


#======================save shp

# At the end of your script, add these lines to save your spatial dataframes as shapefiles:

# 1. Save the main Philadelphia data with foreign-born percentages and other indicators
st_write(philly_data, "shapefiles/philly_foreignborn_data.shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE)

# 2. Save the enhanced Philadelphia data with poverty and language isolation
st_write(philly_data_enhanced, "shapefiles/philly_enhanced_data.shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE)

# 3. Save the Philadelphia PUMAs with immigrant density data
st_write(philly_pumas_joined, "shapefiles/philly_pumas_immigrants.shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE)

# 4. Save the ESL locations
st_write(esl, "shapefiles/esl_locations.shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE)

# 5. Save the Philadelphia tracts with ESL accessibility (min_distance)
st_write(philly_tracts, "shapefiles/philly_tracts_esl_access.shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE)

# Optional: Create the directory first if it doesn't exist
if (!dir.exists("shapefiles")) {
  dir.create("shapefiles")
}

# Print summary of what was saved
cat("\n=== Shapefiles saved successfully ===\n")
cat("1. philly_foreignborn_data.shp - Basic foreign-born population statistics\n")
cat("2. philly_enhanced_data.shp - Enhanced data with poverty and language isolation\n")
cat("3. philly_pumas_immigrants.shp - PUMA-level immigrant density\n")
cat("4. esl_locations.shp - ESL program locations\n")
cat("5. philly_tracts_esl_access.shp - Tracts with ESL accessibility metrics\n")
