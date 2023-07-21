# This code creates a map for motorised travel time to a permanent cataract surgery facility in Malawi at the national level. 
# The bar chart legend displays the percentage of the Malawi population aged 50 and over by access.

# Created by: Jinfeng Zhao, Jacqueline Ramke and Ian McCormick
# Last modified: 21 July 2023

#### packages ####
library(tidyverse)
library(here)
library(sf)
library(raster)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(classInt)
library(exactextractr)

#### 1. DATA #### 

#### 1.1 importing Malawi adm0, adm1 and adm2 boundaries #### 
# Malawi adm0 boundary
adm0 <- st_read(here("data","malawi","mwi_adm_nso_20181016_shp.nosync","mwi_admbnda_adm0_nso_20181016.shp"))
adm0 <- adm0[, c("ADM0_EN", "ADM0_PCODE")]

# Malawi adm1 boundary
adm1 <- st_read(here("data","malawi","mwi_adm_nso_20181016_shp.nosync","mwi_admbnda_adm1_nso_20181016.shp"))
adm1 <- adm1[, c("ADM1_EN", "ADM1_PCODE")]

# Malawi adm2 boundary
adm2 <- st_read(here("data","malawi","mwi_adm_nso_20181016_shp.nosync","mwi_admbnda_adm2_nso_20181016.shp"))
adm2 <- adm2[, c("ADM2_EN", "ADM2_PCODE", "ADM1_EN", "ADM1_PCODE")]

#### 1.2 load location of public eye facilities data #### 
eye_fac <- read.csv(here("data", "malawi", "malawi-gps-cat-facilities_230510.csv"))

eye_fac_sf <- eye_fac %>% 
  st_as_sf(coords = c("Long", "Lat"), crs=4326) %>%
  filter(., Cataract.surgical.services == "Permanent" &
           Owner != "Private")

#### 1.3 opening the population tiff file (popsum) created already, and aggregate ####
# for the code to create popsum refers to https://github.com/JNesemann/eye_facilities
popsum <- raster(here("data/malawi", "rasters", "popsum_constrained.tif"))

#### 1.4 reclassifying the motorised travel time to permanent surgeries raster file (access.motor.surgperm) already created ####
# for the code to create access.motor.surgperm refers to https://github.com/JNesemann/eye_facilities
access.motor.surgperm <- raster(here("data","malawi","rasters","traveltimes", "motor travel time",
                                     "access_motor_surgperm.tif"))

# creating a reclassification matrix
reclass <- c(0, 60, 1, # within 60 minutes
             60, 120, 2, # within 120 minutes
             120, 180, 3, # within 180 minutes
             180, Inf, 4) # greater than 150 minutes

reclass.mat <- matrix(reclass,
                      ncol = 3,
                      byrow = TRUE)

# reclassifying raster access.motor.surgperm
motor.surgperm.cat <- reclassify(access.motor.surgperm,
                                 reclass.mat,
                                 include.lowest = TRUE) 

# convert raster to polygon and dissolve
motor.surgperm.cat.poly.diss <- rasterToPolygons(motor.surgperm.cat, dissolve=TRUE)

motor.surgperm.cat.poly.diss <- st_as_sf(motor.surgperm.cat.poly.diss, crs = 4326)

colnames(motor.surgperm.cat.poly.diss)[colnames(motor.surgperm.cat.poly.diss) == "layer"] ="access"

#### 1.5 speeding up st_intersection using a function & zonal statistics ####
# This function was copied from this thread: https://github.com/r-spatial/sf/issues/801

st_intersection_faster <- function(x,y){
  #faster replacement for st_intersection(x, y,...)

  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}

  st_intersection(x, y_subset)
}

motor.surgperm.cat.poly.diss.adm2 <- st_intersection_faster(motor.surgperm.cat.poly.diss, adm2)

motor.surgperm.cat.poly.diss.adm2 <- motor.surgperm.cat.poly.diss.adm2 %>%
  mutate(pop =exact_extract(popsum, motor.surgperm.cat.poly.diss.adm2,
                            'sum'))

#### 1.6 summarise motorised travel time to permanent surgeries at the national and sub-national levels #### 
#### 1.6.1 at the adm0 level #### 
pop_pct_by_access_at_adm0 <- as.data.frame(motor.surgperm.cat.poly.diss.adm2[, c("access", "pop")]) %>%
  group_by(access) %>%
  summarise_at(c("pop"), sum,
               na.rm = TRUE) %>%
  ungroup() %>%
  mutate(travel_minutes = case_when(
    access == 1 ~ "≤60",
    access == 2 ~ "61-120",
    access == 3 ~ "121-180",
    access == 4 ~ ">180")) %>%
  mutate(pop = round(pop, digits = 0),
         pop_pct0 = round((pop / sum(pop) * 100), digits = 2),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) %>%
  mutate(transport = "motor",
         service = "surgperm",
         country = "Malawi")

pop_pct_by_access_at_adm0 <- pop_pct_by_access_at_adm0[, c("country", "transport", "service", "travel_minutes", "access", "pop",
                                                           "pop_pct0","pop_pct")]


#### 1.6.2 at the adm2 level ####
pop_pct_by_access_2hr_at_adm2 <- as.data.frame(motor.surgperm.cat.poly.diss.adm2[, c("access", "pop", "ADM2_EN")]) %>%
  mutate(travel_minutes = case_when(
    access <= 2 ~ "<=120",
    access > 2 ~ ">120")) %>%
  group_by(ADM2_EN, travel_minutes) %>%
  summarise_at(c("pop"), sum,
               na.rm = TRUE) %>%
  ungroup() %>%
  group_by(ADM2_EN) %>%
  mutate(pop = round(pop, digits = 0),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) %>%
  filter(travel_minutes == "<=120") %>%
  mutate(transport = "motor",
         service = "surgperm",
         country = "Malawi")

pop_pct_by_access_2hr_at_adm2 <- pop_pct_by_access_2hr_at_adm2[, c("country","transport", "service", "ADM2_EN", 
                                                                   "travel_minutes", "pop", "pop_pct")]

write.csv(pop_pct_by_access_2hr_at_adm2,
          here("data/malawi","table","Malawi_pop_pct_by_adm2_for_travel_minutes_le_120.csv"))


#### 2. PLOT ####
#### 2.1 access map #### 
# raster to data frame
access.motor.surgperm.df <- raster::as.data.frame(access.motor.surgperm, xy=TRUE) 
access.motor.surgperm.df <- na.omit(access.motor.surgperm.df)
summary(access.motor.surgperm.df$layer)

# create fixed interval
fix.brks <- classIntervals(access.motor.surgperm.df$layer, n = 4, style="fixed",
                           fixedBreaks=c(-0.1, 60, 120, 180, Inf),
                           intervalClosure="right")

access.motor.surgperm.df <- access.motor.surgperm.df %>%
  mutate(pop_cat = cut(layer, fix.brks$brks))

# plotting
map = ggplot() + 
  geom_sf(data = adm0, fill = "#d9d9d9", alpha = 1,
          show.legend = NA, color = NA) +
  geom_raster(data = access.motor.surgperm.df, 
              aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",
    values = c("#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    na.value = "#969696") +
  # add adm1 and adm0 outlines
  geom_sf(data = adm1, fill = NA, lwd = 2, colour="#737373", show.legend = NA) + # as adm1 polygon outline
  geom_sf(data = adm0, fill = NA, lwd = 3, colour="#737373", show.legend = NA) + # as adm1 polygon outline
  # add north arrow and scale
  annotation_scale(location = "bl", width_hint = 0.4,
                   pad_x = unit(0.12, "in"), pad_y = unit(0.4, "in"),
                   height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.26, "in"), pad_y = unit(0.6, "in"),
                         height = unit(1.0, "cm"), width = unit(0.9, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10)) +
  # add facility as point
  geom_sf(data = eye_fac_sf, fill = NA, aes(color = "Country"),
          size = 3, shape = "+") +
  scale_color_manual(name="",values = c("Country" = "red"),
                     labels = c("Permanent cataract\nsurgery facility"),
                     guide = guide_legend()) +
  theme(legend.position = c(1.1, 0.53),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.text = element_text(size=10.0), #change legend text font size
        legend.spacing.y = unit(0, "pt")) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 20,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0),
        panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), # optional - remove gridlines
        panel.grid.minor = element_blank(), # optional - remove gridlines
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', color=NA),
        legend.key=element_rect(fill="transparent"), 
        legend.margin =  margin(0,0,0,0,unit="pt"))
        

#### 2.2 barchart legend using ggplot ####
#### 2.2.1 process data ####
hist_factor = 2
legend_w = 14
vline_w = 0.2

bar_adm0 <- pop_pct_by_access_at_adm0 %>%
  mutate(pop_pct1 = pop_pct0 + legend_w,
         alpha = 1,
         label=(paste0(sprintf("%1.1f", pop_pct), "%")))

#### 2.2.2 barchart legend  ####

barchart_plot = ggplot(bar_adm0, aes(x = reorder(travel_minutes,-access), #pop_pct1),
                                y = pop_pct1/hist_factor,
                                fill = as.factor(access), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),
                     guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8,  
           color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label, 
                y = 9),
            size = 2.8, 
            position=position_dodge(width=0.9), vjust= 0.5,
            hjust= 0) + 
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Travel minutes to nearest facility\nand corresponding % of Malawi\npopulation aged 50+",
    x = "", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, colour="#000000", hjust = 0, vjust = 0),
    axis.text.y = element_text(size=8.5, colour="#000000"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 5,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 10)) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        aspect.ratio = 1/1.2) 


#### 2.3 combined plots ####
combined <- here("data/malawi","maps/original_resolution",
               "Malawi_Map1_Combined.png")

combined_plots = ggdraw() +
  draw_plot(map, height = 1, width = 0.8) +
  draw_plot(barchart_plot, x = 0.36, y = 0.5, hjust = 0, vjust = 0,
            width = 0.41, height = 0.44) +
  theme(plot.margin = margin(b=0.1, l=0.2, t=0.0, r=-0.2, "cm"),
        plot.background = element_rect(fill='transparent', color=NA))


ggsave(plot = combined_plots, file = combined,
       type = "cairo-png",
       bg = "white",
       width = 16, height = 16, units = "cm", dpi = 600)

