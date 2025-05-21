### Responding to some of Maria's comments, for the purposes of FSN
## Note: This file did not end up being used. 
if(!require("pacman")){install.packages("pacman");library("pacman")}
pacman::p_load(data.table, ggplot2, tidyverse, knitr, zoo, lubridate, sf, basemaps, ggmap)

cbi_colours = c("#7C477E", "#0B5471", "#0083A0", "#5EC5C2", "#D2E288", "#007DC5", "#F47D20")
options(scipen = 4000)
theme_set(theme_bw())
theme_update(legend.position = "bottom", 
             legend.text = element_text(size = rel(0.8)), 
             legend.box.spacing = unit(0, "pt"))


valuations <- readRDS("data/fsn/valuations_processed.rds")
cork_imputed <- readRDS("data/fsn/cork_summary_processed.rds")

# Some Cleanliness -----------
## Labelling -------
valuations[segment == "All", segment := "Other"]
valuations[Subsegment == "All", Subsegment := "Other"]
valuations[, dub := fifelse(County == "DUBLIN", "Dublin", "Ex-Dublin")]
# valuations[segment == "Other", segment := fifelse(grepl("hotel", Uses, ignore.case = TRUE), "Hotel", "Other")]



## Location ---------
valuations[segment == "Office", office_location_simple := fcase(
  RoutingKey %chin% c("D01", "D02", "D04") & County == "DUBLIN", "Office - Dublin CBD",
  County == "DUBLIN", "Office - Rest of Dublin",
  LocalAuthority %chin% c("LIMERICK CITY AND COUNTY COUNCIL", "GALWAY CITY COUNCIL", "WATERFORD CITY AND COUNTY COUNCIL"), "Secondary Cities",
  default = "Ex-Dublin"
)]

valuations[segment == "Retail", retail_location := fcase(
  LocalAuthority == "DUBLIN CITY COUNCIL", "Dublin City",
  County == "DUBLIN", "Rest of Dublin",
  LocalAuthority %chin% c("LIMERICK CITY AND COUNTY COUNCIL", "GALWAY CITY COUNCIL", "WATERFORD CITY AND COUNTY COUNCIL"), "Secondary Cities",
  default = "Ex-Cities"
)]


valuations[segment == "Retail", retail_sub := fifelse(grepl("Grafton Street|Henry|Other City Centre|Rest of Dublin", Subsegment), "Retail", Subsegment)]

valuations[, nuts3_region := fcase(
  County %chin% c("CARLOW", "KILKENNY", "WEXFORD", "WATERFORD"), "South-East",
  County %chin% c("LAOIS", "LONGFORD", "OFFALY", "WESTMEATH"), "Midlands", 
  County %chin% c("KILDARE", "MEATH", "WICKLOW", "LOUTH"), "Mid-East",
  County %chin% c("CORK", "KERRY"), "South-West",
  County %chin% c("MAYO", "GALWAY", "ROSCOMMON"), "West",
  County %chin% c("CAVAN", "DONEGAL", "LEITRIM", "MONAGHAN", "SLIGO"), "Border",
  County %chin% c("CLARE", "TIPPERARY", "LIMERICK"), "Mid-West",
  County %chin% c("DUBLIN"), "Dublin",
  default = NA_character_
)]


valuations[segment == "Industrial", industrial_use := fcase(
  grepl("data centre", Uses, ignore.case = TRUE), "Data Centres",
  grepl("pharmaceutical|medical|chemical", Uses, ignore.case = TRUE), "Pharm.",
  grepl("computer", Uses, ignore.case = TRUE), "Technology",
  grepl("warehouse|distribution|workshop", Uses, ignore.case = TRUE), "Core I&L",
  grepl("factory", Uses, ignore.case = TRUE), "Factories",
  default = "All Other"
)]

## CV per sq. metres ---------
valuations[NetFloorArea > 5 & segment != "Other", cv_pm := capital_value/NetFloorArea]



## 3rd vs 4th Gen
offices <- valuations[segment == "Office"]

offices[, office_esg := fcase(
  grepl("1st gen", Uses, ignore.case = TRUE), "1st Gen",
  grepl("2nd gen", Uses, ignore.case = TRUE), "2nd Gen",
  grepl("3rd gen", Uses, ignore.case = TRUE), "3rd Gen",
  grepl("4th gen", Uses, ignore.case = TRUE), "4th Gen",
  default = "Heritage / Over-the-shop / Other"
)]


## Industrial
valuations[segment == "Industrial", .(N = .N,
                                      floorspace = sum(NetFloorArea),
                                      med_rent = median(Valuation),
                                      med_cvpm = median(cv_pm, na.rm = T),
                                      avg_yld = median(yield)),
           keyby = .(dub, industrial_use == "Core I&L")]
valuations[industrial_use == "Core I&L",  .(N = .N,
                                            floorspace = sum(NetFloorArea),
                                            med_cvpm = median(cv_pm, na.rm = T),
                                            avg_yld = median(yield)), keyby = .(dub)]
