
# Plot hospitals and their ZCTA catchments on map

# TO DO
# plot all cities catchment on a TX map with the Texas outline

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### SOURCE FUNCTIONS ####
#/////////////////////////
# List of file paths to source
files_to_source <- c(
  "get_packages_used.R",
  "estimation_fns.R",
  # need to add function tests once I have them
  "../private_input_data/api_keys.R"
)
# Source each file
purrr::walk(files_to_source, source)

# Make dir for figs if it doesn't exist
fig_dir="../figures/"
if(!dir.exists(fig_dir)){
  dir.create(fig_dir)
} # end if fig dir not made

#///////////////////
#### OPEN FILES ####
#///////////////////

# Spatial elements
zcta_city_crosswalk = read_csv("../big_input_data/US_ZCTA-CITY-COUNTY_pop_2018-2022_acs.csv")
us_zcta_city_pop = get_zcta_acs_pop(state="US") %>%
  separate(CITY_NAME, into=c("CITY", "STATE"), sep=", ", extra="merge")
tx_zcta_city_pop = us_zcta_city_pop %>%
  filter(STATE=="TX")

# Hospital data 
disease = "FLU" # hyphen separated alphabetical string
date_range = "2022Q3-2023Q2" # hyphen separated start-end date string e.g. 2018Q3-2019Q2
calc_catch = read_csv(paste0("../private_results/HOSP_CATCHMENTS/HOSP-CATCH-CALC_", 
                                  disease, "_", date_range, ".csv")) %>%
  separate(CITY_NAME, into=c("ZCTA_CITY", "ZCTA_STATE"), sep=", ") %>%
  mutate(ZCTA = as.character(ZCTA))
hosp_catchments = read_csv(paste0("../private_results/HOSP_CATCHMENTS/CITY-HOSP-POP-CATCH_", 
                                  disease, "_", date_range, ".csv")) %>%
  rename_with(~ paste0("HOSP_", .), .cols = c(CITY, STATE, STREET_ADDRESS_clean:COUNTY_FIPS))


#/////////////////////////////////
#### Example Catchments Plots ####
#/////////////////////////////////
houston_hosp = hosp_catchments %>%
  filter(HOSP_CITY=="HOUSTON")
calc_catch_houston = calc_catch %>%
  filter(HOSPITAL %in% houston_hosp$HOSPITAL)
  #filter(ZCTA_CITY == "Houston")
houston_catch_zcta_geo = tx_zcta_city_pop %>%
  filter(ZCTA %in% calc_catch_houston$ZCTA) %>%
  dplyr::select(ZCTA, geometry) %>%
  left_join(calc_catch_houston, by="ZCTA") %>%
  group_by(ZCTA) %>%
  summarise(
    PAT_COUNT = sum(PAT_COUNT),
    POPULATION = first(POPULATION),
    TOTAL_PAT_PER_ZCTA_COUNT = first(TOTAL_PAT_PER_ZCTA_COUNT),
    PAT_COUNT_PROPORTION = sum(PAT_COUNT_PROPORTION),
    ZCTA_CONTRIBUTION = sum(ZCTA_CONTRIBUTION)
  ) %>%
  ungroup()

min_pat = min(houston_catch_zcta_geo$PAT_COUNT)
max_pat = max(houston_catch_zcta_geo$PAT_COUNT)

pat_count_plt = ggplot()+
  geom_sf(data=houston_catch_zcta_geo, mapping=aes(geometry=geometry, fill=PAT_COUNT),
          size = 0.05, color="black")+ # , show.legend = FALSE , fill="cornsilk"
  scale_fill_gradient(low="cornsilk", high="orange2", name = "Patient Count",
                      limits = c(min_pat, max_pat),
                      breaks = floor(seq(min_pat, max_pat, length.out = 5))
                      ) +
  guides(fill = guide_colourbar(reverse = FALSE)) +
  theme_minimal()+ # base_size = 10
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.text.align = 1,
    plot.margin=unit(c(0, 0.1, 0, 0),"cm")
  )
ggsave(paste0(fig_dir, "houston-all-hosp_",disease, "_", date_range, ".png"),
       pat_count_plt, dpi=1200,
       width=11, height=9, units="in", bg="white"
)








#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### MAKE PLOTS ####
#///////////////////
# Plot of all TX ZCTA boundaries
tx_zcta_plt = ggplot()+
  geom_sf(data=tx_zcta_city_pop, mapping=aes(geometry=geometry, fill=estimate),
          size = 0.05, color="black")+ # , show.legend = FALSE , fill="cornsilk"
  scale_fill_gradient(low="cornsilk", high="orange4", name = "2022 Pop")+
  guides(fill = guide_colourbar(reverse = FALSE)) +
  theme_void()+ # base_size = 10
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.text.align = 1,
    #legend.key.size = unit(0.5, "lines"),
    plot.margin=unit(c(0, 0.1, 0, 0),"cm")
  )
ggsave(paste0(fig_dir, "tx_zcta_2022pop.png"),
       tx_zcta_plt, dpi=1200,
       width=11, height=9, units="in", bg="white"
       )











