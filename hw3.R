# Install packages once in the Console if needed:
install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset

#PROBLEM 1

#PART A
ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6)+ geom_smooth(method = "lm", se = FALSE)  + facet_wrap(~drv) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)") +
  theme_bw()
  
#There are three lines because facet_wrap splits the data into a separate panel for each drive type so geom_smooth fits a line in each panel. 

#PART B
p_positions <- ggplot(vehicles, aes(x = drv, fill = factor(cyl)))
p_stack <- p_positions + geom_bar(position = "stack") + theme_bw()
p_stack


p_fill <- p_positions + geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()
p_fill

vehicles |> count(drv)

#The bars are the same height because position = "fill" makes every bar add up to 1 but the sample sizes are different since rear-wheel drive has 25 cars and the others have over 100

#PART C
smaller <- gems |> filter(carat < 3)
nrow(smaller)               
nrow(gems) - nrow(smaller) 
#retained 53900
#excluded 40

p_fine <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Bin width = 0.01", x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Bin width = 0.5", x = "Carat", y = "Count") +
  theme_bw()

p_fine
p_coarse

gems |>
  filter(carat %in% c(0.99, 1.00)) |>
  count(carat)

#The small bins show big spikes at round numbers like 1.00 carat. The big bins hide these spikes and specific variations.

#PROBLEM 2
libs_raw <- read_csv("chicago_libraries.csv")
nrow(libs_raw)    

libs <- libs_raw |>
  mutate(LOCATION = gsub("[() ]", "", LOCATION)) |>
  separate_wider_delim(LOCATION, delim = ",", names = c("lat", "lon")) |>
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

sum(is.na(libs$lat) | is.na(libs$lon))     
libs |> count(BRANCH) |> filter(n > 1)        

libs_clean <- libs |>
  filter(!is.na(lat), !is.na(lon)) |>
  distinct(BRANCH, .keep_all = TRUE) |>
  mutate(site_id = row_number(), site_name = BRANCH) |>
  select(site_id, site_name, lat, lon)

nrow(libs_clean)  
#the raw data and cleaned has same number of rows because there were no problems with missing coordinates or duplicates

libs_sf <- st_as_sf(libs_clean, coords = c("lon", "lat"), crs = 4326)
sites_m <- st_transform(libs_sf, 26916)

areas_raw <- st_read("chicago_community_areas.geojson")
city <- areas_raw |> st_make_valid() |> st_transform(26916) |> st_union()

# buffers
buf500 <- st_union(st_buffer(sites_m, 500))
buf1000 <- st_union(st_buffer(sites_m, 1000))

p_sites <- ggplot() +
  geom_sf(data = city, fill = "grey95") +
  geom_sf(data = sites_m, aes(color = "Library")) +
  labs(title = "Chicago libraries", color = NULL,
       caption = "Source: City of Chicago Data Portal, 2026-11-07") +
  theme_bw()
p_sites

p_buffers <- ggplot() +
  geom_sf(data = city, fill = "grey95") +
  geom_sf(data = buf500, aes(fill = "500 m buffer"), alpha = 0.4) +
  geom_sf(data = sites_m, aes(color = "Library")) +
  labs(title = "Area within 500 m of a library", fill = NULL, color = NULL,
       caption = "Source: City of Chicago Data Portal, 2026-11-07") +
  theme_bw()
p_buffers

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buf500)) / 1e6,
                     as.numeric(st_area(buf1000)) / 1e6)
)
buffer_summary
buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1] 

data_sources <- tibble(
  dataset = c("Chicago Libraries", "Chicago Community Areas"),
  source = c("City of Chicago Data Portal", "City of Chicago Data Portal"),
  url = c("https://data.cityofchicago.org/d/x8fc-8rcq",
          "https://data.cityofchicago.org/d/igwz-8jzy"),
  downloaded = c("2026-11-07", "2026-11-07")
)
write_csv(data_sources, "data_sources.csv")

#PROBLEM 3

names(areas_raw)
nrow(areas_raw) #77
sum(is.na(areas_raw$area_numbe))    #0    
sum(duplicated(areas_raw$area_numbe))  #0  
sum(st_is_empty(areas_raw)) #0       
sum(!st_is_valid(areas_raw))     #0

districts <- areas_raw |>
  st_make_valid() |>
  st_transform(26916) |>
  mutate(district_id = area_numbe, district_name = community) |>
  select(district_id, district_name)

sum(!st_is_valid(districts))  

p_overlay <- ggplot() +
  geom_sf(data = districts, fill = "white") +
  geom_sf(data = sites_m, color = "darkblue") +
  labs(title = "Libraries over Chicago community areas",
       caption = "Source: City of Chicago Data Portal, 2026-11-07") +
  theme_bw()
p_overlay

matches <- st_intersects(sites_m, districts)
site_match_audit <- tibble(site_id = sites_m$site_id, n_matches = lengths(matches))
write_csv(site_match_audit, "site_match_audit.csv")

site_match_audit |> count(n_matches)       
site_match_audit |> filter(n_matches != 1)

one <- lengths(matches) == 1
assigned <- sites_m[one, ] |>
  mutate(district_id = districts$district_id[unlist(matches[one])])
n_unassigned <- nrow(sites_m) - nrow(assigned)
n_unassigned

site_counts <- assigned |>
  st_drop_geometry() |>
  count(district_id, name = "n_sites")


district_counts <- districts |>
  mutate(area_km2 = as.numeric(st_area(districts)) / 1e6) |>
  left_join(site_counts, by = "district_id") |>
  mutate(n_sites = replace_na(n_sites, 0),
         sites_per_km2 = n_sites / area_km2) |>
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

cap <- paste0("Source: City of Chicago Data Portal, 2026-11-07. ",
              n_unassigned, " sites unassigned. Grey = missing.")

p_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites)) +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey60",
                      name = "Libraries") +
  labs(title = "Libraries per community area", caption = cap) +
  theme_bw()
p_count

p_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2)) +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey60",
                      name = "Libraries per km²") +
  labs(title = "Library density by community area", caption = cap) +
  theme_bw()
p_density

district_counts |> st_drop_geometry() |> slice_max(n_sites, n = 1, with_ties = TRUE)
district_counts |> st_drop_geometry() |> slice_max(sites_per_km2, n = 1, with_ties = TRUE)

#Five areas tie for the most libraries with 3 each, Portage Park, Humboldt Park, Austin, Near West Side and New City.
#Douglas has the highest density at about 0.47 libraries per km²
#A zero means no library is inside that area, but people there could still live close to one across the border.

