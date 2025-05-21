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


cork_imputed[segment == "All", segment := "Other"]

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


## CV per sq. metres ---------
valuations[NetFloorArea > 5 & segment != "Other", cv_pm := capital_value/NetFloorArea]

# AGGREGATE MARKET ----------------
market_splits_ct <- valuations[, .(capital_value = sum(capital_value)), keyby = .(County, segment)]


cork_imputed <- cork_imputed[, .(County, segment, capital_value),][order(segment)]
market_splits_ct <- rbind(market_splits_ct, cork_imputed[, .(County, segment, capital_value)])

market_splits <- market_splits_ct[, .(capital_value = sum(capital_value)), .(segment)]

market_splits_dub = copy(market_splits_ct)
market_splits_dub[, dub := fifelse(County == "DUBLIN", "Dublin", "Ex-Dublin")]
#
market_splits_dub <- market_splits_dub[, .(capital_value = sum(capital_value)), keyby = .(dub, segment)]


market_splits[, prop := capital_value/sum(capital_value)]

# fwrite(market_splits, "docs/out/market_split.csv")

ggplot(data = market_splits[, ], aes(x = segment, y = prop)) + geom_bar(stat = "identity", colour = "black") + scale_fill_manual(values = cbi_colours)  # + scale_y_continuous(labels)


# W/O Cork - Same
ggplot(data = valuations[, .(cv = sum(capital_value)), keyby = segment][, prop := cv/sum(cv)], aes(x = segment, y = prop)) +  geom_bar(stat = "identity", colour = "black")

## Granular market -------------------
asset_segment <- valuations[, .(capital_value = sum(capital_value)), .(Subsegment, dub)]

# [, prop := 100*capital_value/sum(capital_value)][,]

asset_segment_nat <- rbind(asset_segment[], cork_imputed[, .(dub = County, Subsegment = segment, capital_value)])

asset_segment_nat[dub == "CORK", dub := "Ex-Dublin"]
asset_segment_nat <- asset_segment_nat[, .(capital_value = sum(capital_value)), .(Subsegment, dub)][, prop_total := 100*capital_value/sum(capital_value)][order(dub),]

fwrite(asset_segment_nat, "docs/out/more_granular_breakdown.csv")




# Industrial ---------------------
valuations[segment == "Industrial", industrial_use := fcase(
  grepl("data centre", Uses, ignore.case = TRUE), "Data Centres",
  grepl("pharmaceutical|medical|chemical", Uses, ignore.case = TRUE), "Pharm.",
  grepl("computer", Uses, ignore.case = TRUE), "Technology",
  grepl("warehouse|distribution|workshop", Uses, ignore.case = TRUE), "Core I&L",
  grepl("factory", Uses, ignore.case = TRUE), "Factories",
  default = "All Other"
)]

industrial_trees <- valuations[segment == "Industrial", .(cv = sum(capital_value),
                            floor_area = sum(NetFloorArea)), keyby = industrial_use][, `:=` (prop_cv = 100*cv/sum(cv),
                                                                                            prop_fa = 100*floor_area/sum(floor_area))]

industrial_trees 



valuations[segment == "Industrial", .(cv = sum(capital_value), floor_area = sum(NetFloorArea)), keyby = .(dub, industrial_use)][, `:=` (prop_cv = 100*cv/sum(cv), prop_fa = 100*floor_area/sum(floor_area)), by = dub][,]


industrial_vals_dub <- valuations[segment == "Industrial", .(cv = sum(capital_value), floor_area = sum(NetFloorArea)), keyby = .(dub, industrial_use)][, `:=` (prop_cv_grp = 100*cv/sum(cv), prop_fa_grp = 100*floor_area/sum(floor_area))][,]

industrial_tabs <- valuations[segment == "Industrial", ]

fwrite(industrial_trees, "docs/out/ind_split.csv")
fwrite(industrial_vals_dub, "docs/out/ind_split_dub.csv")

segments <- valuations[segment == "Industrial", .(cv = sum(capital_value), floor_area = sum(NetFloorArea)), keyby = .(industrial_use, nuts3_region)][, `:=` (prop_cv_grp = 100*cv/sum(cv), prop_fa_grp = 100*floor_area/sum(floor_area)), by = industrial_use][,]


# treemap(dtf = industrial_trees, index = "industrial_use", vSize = "prop_cv", type = "index")

# sum_all <- valuations[, .(cv = sum(capital_value)), keyby = segment][, `:=` (prop_cv = 100*cv/sum(cv))]
# treemap(dtf = sum_all, index = "segment", vSize = "prop_cv", type = "index", palette = cbi_colours[4:7], title = "")


# OFFICE -------------
## Distribution of Capital Values, Cumulative --------
offices <- valuations[segment == "Office"]

offices[, quantile_office := cut(capital_value, 
                                 quantile(capital_value, probs = seq(0, 1, by = .01)), 
                                 include.lowest = TRUE, labels = 1:100)]

office_decile = offices[, .(cv = sum(capital_value), fa = sum(NetFloorArea)), keyby = .(quantile_office)][,  `:=` (cumulative_cv = cumsum(cv), cumulative_fa = cumsum(fa))][, `:=` (c_prop = cumulative_cv/sum(cv), fa_prop = cumulative_fa/sum(fa))]

ggplot(data = office_decile, aes(x = quantile_office, y = c_prop)) + geom_histogram(stat = "identity")

melted_deciles <-  melt(office_decile, id.vars = "quantile_office", measure.vars = c("c_prop", "fa_prop"))
melted_decile_values <- office_decile[office_decile_area] |> melt(id.vars = "quantile_office", measure.vars = c("cumulative", "cumulative_fa"))



# fwrite(melted_decile_values, "docs/out/office_decile_cumulative.csv")



ggplot(data = melted_deciles, aes(x = quantile_office, y = value, fill = variable)) + geom_histogram(stat = "identity", position = "dodge") + scale_fill_manual(values = cbi_colours[4:5]) + xlab("Quantile of office CV")
## CORRECT ----------------------
## Here: Anything with NA cvpm, set to 1
offices[NetFloorArea > 5, cv_pm := capital_value/NetFloorArea]

offices[, quantile_office_cvm := cut(cv_pm, 
                                 quantile(cv_pm, probs = seq(0, 1, by = .01), na.rm = TRUE), 
                                 include.lowest = TRUE, labels = 1:100)]

offices[, decile_office_cvm := cut(cv_pm, 
                                     quantile(cv_pm, probs = seq(0, 1, by = .1), na.rm = TRUE), 
                                     include.lowest = TRUE, labels = 1:10)]


offices[is.na(quantile_office_cvm), quantile_office_cvm := 1]
offices[is.na(decile_office_cvm), decile_office_cvm := 1]

office_decile_cvm = offices[, .(cv = sum(capital_value), fa = sum(NetFloorArea)), keyby = .(quantile_office_cvm)][,  `:=` (cumulative_cv = cumsum(cv), cumulative_fa = cumsum(fa))][, `:=` (c_prop = cumulative_cv/sum(cv), fa_prop = cumulative_fa/sum(fa))]

melted_deciles_cvm <-  melt(office_decile_cvm, id.vars = "quantile_office_cvm", measure.vars = c("c_prop", "fa_prop"))

ggplot(data = melted_deciles_cvm, aes(x = quantile_office_cvm, y = value, group = variable)) + geom_line() + scale_fill_manual(values = cbi_colours[4:5]) + xlab("Quantile of office CV/m")

## Distribution by Postal Codes -----------
offices[, .(cv = sum(capital_value)/1e9, fa =  sum(NetFloorArea)), .(RoutingKey, County)][order(-cv)] |> print(topn = 10)

# Retail --------------------
## Categorisation into Shopping centres, town centres, etc. -------------------
## Table 1:
valuations[segment == "Retail", .(Units = .N, 
                                  NFA = sum(NetFloorArea, na.rm = TRUE), 
                                  CapitalValue = sum(capital_value), 
                                  Yield = round(mean(yield), 2)), 
           keyby = .(Subsegment, retail_location)][, cv_m := CapitalValue/NFA][,]


valuations[segment == "Retail" & grepl("department store", Uses, ignore.case = TRUE), .(N = .N, cv = sum(capital_value), fa = sum(NetFloorArea)), .(Uses)][order(-cv)] |> print(topn = 50)

valuations[segment == "Retail" & grepl("\\bdepartment\\b \\bstore\\b", Uses, ignore.case = TRUE), .(Address, County, yield)] # Some are shopping / town / shop centres, others are not. 

retail <- valuations[segment == "Retail"]
retail[NetFloorArea > 5, cv_pm := capital_value/NetFloorArea]
retail[, retail_sets := fcase(
  grepl("\\bsupermarket\\b", Uses, ignore.case = TRUE), "Supermarkets",
  grepl("(\\bshop\\b|\\bshopping\\b|\\btown\\b) (\\bcentre\\b|\\bcenter\\b)", Address, ignore.case = TRUE, perl = TRUE) & Category == "RETAIL (SHOPS)", "Shopping Centres",
  Category == "RETAIL (WAREHOUSE)", "Retail Warehouse",
  default = "Retail Units"
)]


retail[, .(Units = .N,NFA = sum(NetFloorArea, na.rm = TRUE), 
                                  CapitalValue = sum(capital_value), 
                                  Yield = round(mean(yield), 2)), 
           keyby = .(retail_sets, dub)][, cv_prop := CapitalValue/sum(CapitalValue)][,]

## Cumulative Distribution ------------

retail[, quantile_retail := cut(cv_pm, 
                                 quantile(cv_pm, probs = seq(0, 1, by = .01), na.rm = TRUE), 
                                 include.lowest = TRUE, labels = 1:100)]

retail[is.na(quantile_retail), quantile_retail := 1]

retail_quantiles = retail[, .(cv = sum(capital_value), fa = sum(NetFloorArea)), keyby = .(quantile_retail)][,  `:=` (cumulative_cv = cumsum(cv), cumulative_fa = cumsum(fa))][, `:=` (c_prop = cumulative_cv/sum(cv), fa_prop = cumulative_fa/sum(fa))]

retail[, .(cv = sum(capital_value)), keyby = .(quantile_retail)][,  `:=` (cumulative_cv = cumsum(cv))][]
retail[, .(fa = sum(NetFloorArea)), keyby = .(quantile_retail)][,  `:=` (cumulative_fa = cumsum(fa))][]

## Other Tables -----------

retail[retail_sets == "Retail Units", prime_st := fcase(
  LocalAuthority == "LIMERICK CITY AND COUNTY COUNCIL" & grepl("connell\\b \\bst", Address, ignore.case = TRUE), "Limerick - O'Connell St", 
  LocalAuthority == "LIMERICK CITY AND COUNTY COUNCIL" & RoutingKey == "V94", "Limerick City",
  LocalAuthority == "DUBLIN CITY COUNCIL" & grepl("\\bgrafton\\b \\bst", Address, ignore.case = TRUE), "Dublin - Grafton St",
  LocalAuthority == "DUBLIN CITY COUNCIL" & grepl("\\bhenry \\bst|\\bmary\\b \\bst", Address, ignore.case = TRUE), "Dublin - Henry & Mary St",
  LocalAuthority == "DUBLIN CITY COUNCIL" & grepl("D01|D02", RoutingKey), "Dublin City",
  LocalAuthority == "GALWAY CITY COUNCIL" & grepl("\\bshop\\b (\\bstreet\\b|\\bst)", Address, ignore.case = TRUE), "Galway - Shop Street",
  LocalAuthority == "GALWAY CITY COUNCIL" & grepl(""), "Galway City",
  default = NA_character_
)]

retail[retail_sets == "Shopping Centres", .(cv = sum(capital_value)), nuts3_region][, cv_prop := cv/sum(cv)][,]

retail[, .(Units = .N, 
                                  NFA = sum(NetFloorArea, na.rm = TRUE), 
                                  CapitalValue = sum(capital_value), 
                                  Yield = round(mean(yield), 2),
                                  cv_pm = mean(cv_pm, na.rm = TRUE)),
           keyby = .(prime_st)][, .(Location = prime_st, 
                                          Units,
                                          `Net Floor Area (sqm)` = NFA,
                                          `% of Floor Area` = scales::label_percent(accuracy = 0.1)(NFA/sum(NFA)),
                                          `Yield (%)` = Yield,
                                          `Capital Value (€bn)` = sprintf("€%.3fbn", CapitalValue/1e9),
                                          `Avg. CV per sqm` = sprintf("€%s", format(CapitalValue/NFA, big.mark = ",", digits = 2)),
                                          `Avg. CV per sqm - 2` = sprintf("€%s", format(cv_pm, big.mark = ",", digits = 2)),
                                          `% of Retail Capital Value` = scales::label_percent(accuracy = 0.1)(CapitalValue/sum(CapitalValue)) )] |> data.table::transpose(make.names =  "Location", keep.names = "Statistic") |> kable(caption = "Retail Segments", align = "lcccccccc")

retail[retail_sets == "Retail Units", .(Units = .N, 
                                        NFA = sum(NetFloorArea, na.rm = TRUE), 
                                        CapitalValue = sum(capital_value), 
                                        Yield = round(mean(yield), 2),
                                        cv_pm = mean(cv_pm, na.rm = TRUE)),
       keyby = .(prime_st)]
# Industrial ---------------
## Cumulative Distributions
ind <- valuations[segment == "Industrial"]

ind[, quantile_ind := cut(cv_pm, 
                                quantile(cv_pm, probs = seq(0, 1, by = .01), na.rm = TRUE), 
                                include.lowest = TRUE, labels = 1:100)]


ind[is.na(quantile_ind), quantile_ind := 1]

ind_quantiles = ind[, .(cv = sum(capital_value), fa = sum(NetFloorArea)), keyby = .(quantile_ind)][,  `:=` (cumulative_cv = cumsum(cv), cumulative_fa = cumsum(fa))][, `:=` (c_prop = cumulative_cv/sum(cv), fa_prop = cumulative_fa/sum(fa))]

ind[, .(cv = sum(capital_value)), keyby = .(quantile_ind)][,  `:=` (cumulative_cv = cumsum(cv))][]

data_c <- ind[industrial_use == "Data Centres"]

ind[, sum(capital_value)/1e9, nuts3_region]


# Spatial Maps ------------
shape_ie <- sf::read_sf("data/spatial/Ireland_shapefile/ie_10km.shp")
shape_ie <- sf::st_transform(shape_ie, crs = 2157)
county <- sf::read_sf("data/spatial/county_shapefile/Administrative_Areas_-_Ungen_2019.shp")

eds <- sf::read_sf("data/spatial/Electoral_Divisions.geojson")
dublin <- county |> filter(ENGLISH %in% c("DUBLIN CITY COUNCIL", "DUN LAOGHAIRE-RATHDOWN COUNTY COUNCIL"))
dublin <- sf::st_transform(dublin, crs = 3857)


ggplot(county |> filter(PROVINCE == "LEINSTER")) + geom_sf(fill = "white", colour = "black") + theme_void()



data_c 
data_c_sf <- sf::st_as_sf(data_c[!is.na(Xitm) & County != "CLARE" & PropertyNumber != 5021061,], coords = c("Xitm", "Yitm"), crs = 2157)
data_c_sf <- sf::st_transform(data_c_sf, crs = 4326)
data_c_boundaries = sf::st_bbox(data_c_sf)

data_c_boundaries = as.vector(data_c_boundaries)
# dub_map <- get_openstreetmap(bbox = data_c_boundaries, zoom = 5, maptype = "stamen_toner")
eds_subset <- sf::st_crop(eds, data_c_boundaries)

ggplot(eds_subset) + geom_sf(fill = "white") + geom_sf(data = data_c_sf)

ggplot(data_c_sf) + geom_sf() + geom_sf(data)

x <- opq(bbox = data_c_boundaries)
osmdata::bbox_to_string(data_c_boundaries)

osmdata_sf(x)

a <- draw_ext()
a <- sf::st_transform(a, 3857)
base_dub <- basemap_ggplot(a, map_service = "carto", map_type = "light")

base_dub + geom_sf(data = data_c_sf2)
data_c_sf2 <- sf::st_transform(data_c_sf, 3857)

basemap_ggplot(a, map_service = "osm", map_type = "streets") + geom_sf(data = data_c_sf2, aes(color = "COUNTY"))
base_dub + geom_sf(data = data_c_sf2, colour = "red", size = 2)

cbd <- offices[office_location_simple == "Office - Dublin CBD" & !(PropertyNumber %in% c(10028881, 5022896, 386725))]

cbd_spatial <- st_as_sf(cbd[!is.na(Xitm)], coords = c("Xitm", "Yitm"), crs = 2157)

cbd_spatial <- st_transform(cbd_spatial, crs = 3857)



base <- basemap_ggplot(ext = cbd_bbox, map_service = "carto", map_type = "dark", 0.8)

base + geom_sf(data = cbd_spatial, colour = "white")

county_shape_bbox <- sf::st_bbox(dublin)

dublin_cbd <- basemap_ggplot(ext = cbd_bbox, map_service = "carto", map_type = "dark", 0.99)

dublin_cbd + geom_sf(data = cbd_spatial, aes(colour = office_gen))




ggplot() + basemap_gglayer(ext = county_shape_bbox, map_service = "carto", map_type = "dark", 0.95) + scale_fill_identity() + coord_sf()



## ggnmap --------------------
cbd_coords <- sf::st_coordinates(cbd_spatial)


bbox <- make_bbox(lon = X, lat = Y, data = cbd_coords, f = c(0.15, 0.3))
cbd_bbox <- sf::st_bbox(cbd_spatial)

for (i in seq_along(1:4)) {
  cbd_bbox[i] <- bbox[i]
}

dublin_cbd <- basemap_ggplot(ext = cbd_bbox, map_service = "carto", map_type = "dark", 0.95)

dublin_cbd + geom_sf(data = cbd_spatial, aes(colour = office_gen)) + scale_colour_manual(values = cbi_colours[5:6])
## Offices ---------


dublin_offices <- offices[County == "DUBLIN" & !(PropertyNumber %in% c(10028881, 5022896, 386725)) & !is.na(Xitm)]
dublin_offices[NetFloorArea > 10, size_cuts := cut(NetFloorArea,
                                                   quantile(NetFloorArea, probs = seq(0, 1, by = .25), na.rm = TRUE),
                                                   include.lowest = TRUE, labels = c(1, 2, 3, 4))]

dublin_offices[NetFloorArea > 10, quantile_office_cvpm := cut(cv_pm,
                                                   quantile(cv_pm, probs = seq(0, 1, by = .25), na.rm = TRUE),
                                                   include.lowest = TRUE, labels = 1:4)]

dub_off_st <- st_as_sf(dublin_offices, coords = c("Xitm", "Yitm"), crs = 2157)
dub_off_st <- st_transform(dub_off_st, crs = 3857)

dub_off_coords <- sf::st_coordinates(dub_off_st)
dub_bbox <- make_bbox(lon = X, lat = Y, data = dub_off_coords, f = c(0.1, 0.05))
dub_st_bbox <- st_bbox(dub_off_st)

for (i in seq_along(1:4)) {
  dub_st_bbox[i] <- dub_bbox[i]
}                     

dublin_base <- basemap_ggplot(ext = dub_st_bbox, map_service = "carto", map_type = "light", 0.95)
dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm), shape = 18, size = 1.7) + scale_colour_manual(values = cbi_colours[4:8]) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank())

# dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm))) + geom_density2d(aes(colour = quantile_office_cvpm))

spatial_map <- dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm), shape = 18, size = 1.7) + scale_colour_manual(values = cbi_colours[4:8]) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm), shape = 18, size = 1.8) + scale_colour_manual(values = cbi_colours) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Subset?
# dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm) & office_gen == "Prime"), aes( colour = NetFloorArea, shape = 18)) + scale_shape_binned() + coord_sf(xlim = c(-710000, -680000), ylim = c(7070000, 7020000))

tiff("map.tiff", units = "cm", width = 12.75, height = 15.75, res = 350)
spatial_map
dev.off()
# dublin_base + geom_sf(data = dub_off_st) + geom_point(aes(geometry = geometry, colour = cv_pm, size = NetFloorArea, stat = "sf_coordinates"))

### CBD Map --------

dublin_cbd <- offices[office_location_simple == "Office - Dublin CBD" & !(PropertyNumber %in% c(10028881, 5022896, 386725)) & !is.na(Xitm)]


dublin_cbd[NetFloorArea > 10, size_cuts := cut(NetFloorArea,
                                                   quantile(NetFloorArea, probs = seq(0, 1, by = .25), na.rm = TRUE),
                                                   include.lowest = TRUE, labels = c(1, 2, 3, 4))]

dublin_cbd[NetFloorArea > 10, quantile_office_cvpm := cut(cv_pm,
                                                              quantile(cv_pm, probs = seq(0, 1, by = .25), na.rm = TRUE),
                                                              include.lowest = TRUE, labels = 1:4)]

dub_off_st <- st_as_sf(dublin_cbd, coords = c("Xitm", "Yitm"), crs = 2157)
dub_off_st <- st_transform(dub_off_st, crs = 3857)

dub_off_coords <- sf::st_coordinates(dub_off_st)
dub_bbox <- ggmap::make_bbox(lon = X, lat = Y, data = dub_off_coords, f = c(0.15, 0.05))
dub_st_bbox <- st_bbox(dub_off_st)

for (i in seq_along(1:4)) {
  dub_st_bbox[i] <- dub_bbox[i]
}                     
dub_st_bbox[1] <- -705000
dub_st_bbox[2] <- 7040303.4

dub_2 <- sf::st_crop(dub_off_st, dub_st_bbox)

cbd_map <- basemap_ggplot(ext = dub_2, map_service = "carto", map_type = "dark", 0.95)
cbd_map + geom_sf(data = dub_2 |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm, size = NetFloorArea), shape = 18) + scale_colour_manual(values = cbi_colours[4:8]) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank())

# dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm))) + geom_density2d(aes(colour = quantile_office_cvpm))

# spatial_map <- dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm), shape = 18, size = 1.8) + scale_colour_manual(values = cbi_colours) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank())

# dublin_base + geom_sf(data = dub_off_st |> filter(!is.na(quantile_office_cvpm)), aes(colour = quantile_office_cvpm), shape = 18, size = 1.8) + scale_colour_manual(values = cbi_colours) + facet_wrap(~office_gen) + labs(x = NULL, y = NULL, colour = "Quartile of €psm.") + theme(axis.text=element_blank(), axis.ticks = element_blank())


### Data Centre Map--------------
data_c_sf2
data_co <- sf::st_coordinates(data_c_sf2)
data_c_box <- make_bbox(lon = X, lat = Y, data = data_co, f = c(0.2, 0.1))
data_st_bbox <- st_bbox(data_c_sf2)

for (i in seq_along(1:4)) {
  data_st_bbox[i] <- data_c_box[i]
}                     
data_counties <- county |> filter(COUNTY %in% c("DUBLIN", "MEATH"))
data_counties <- st_transform(data_counties, crs = 3857)

data_centre_base <- basemap_ggplot(ext = st_bbox(data_counties), map_service = "carto", map_type = "dark", map_res = 0.99)
data_centre_base + geom_sf()

basemap_ggplot(ext = st_bbox(data_counties), map_service = "carto", map_type = "dark", map_res = 0.99)



data_map <- data_centre_base + geom_sf(data = data_counties, fill = NA, colour = "white") + geom_sf(data = data_c_sf2, shape = 18, size = 2, colour = cbi_colours[4]) + labs(x = NULL, y = NULL) + theme(axis.text=element_blank(), axis.ticks = element_blank())

tiff("data_centres.tiff", units = "cm", width = 8, height = 9, res = 350)
data_map
dev.off()


### Pharmaceuticals map ------------------
pharm <- ind[industrial_use == "Pharmaceutics & Chemical"]
pharm <- sf::st_as_sf(pharm, coords = c("Xitm", "Yitm"), crs = 2157)
# pharm <- sf::st_transform(pharm, crs = 3857)
ggplot() + geom_sf(data = county |> filter(COUNTY != "CORK")) + geom_sf(data = pharm, shape = 18, size = 2, colour = cbi_colours[4])


## Retail Maps ----------------
### Shopping centre dist. 

