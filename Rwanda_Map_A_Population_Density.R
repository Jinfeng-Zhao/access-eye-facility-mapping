# This code creates a population density map for Rwanda at the national level.
# The bar chart legend displays the percentage of the Rwanda population aged 50 and over by population density.

# Created by: Jinfeng Zhao, Jacqueline Ramke and Ian McCormick
# Last modified: 21 July 2023


#### packages ####
library(dplyr)
library(here)
library(sf)
library(raster)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(classInt)


#### 1. DATA #### 
#### 1.1 importing Rwanda adm0 and adm1 boundaries #### 
# Rwanda adm0 boundary
adm0 <- st_read(here("data","rwanda","rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync","rwa_adm0_2006_NISR_WGS1984_20181002.shp"))
adm0 <- adm0[, c("ADM0_EN", "ADM0_PCODE")]

# Rwanda adm1 boundary
adm1 <- st_read(here("data","rwanda","rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync","rwa_adm1_2006_NISR_WGS1984_20181002.shp"))
adm1 <- adm1[, c("ADM1_EN", "ADM1_PCODE")]

#### 1.2 opening the population tiff file (popsum) created already, and aggregate ####
# for code to create popsum refers to https://github.com/JNesemann/eye_facilities
popsum <- raster(here("data/rwanda", "rasters", "popsum_constrained.tif"))

popsum.crop <- crop(popsum, adm0)
popsum.crop <- mask(popsum.crop, adm0)

# aggregating popsum
popsum.agg <- raster::aggregate(popsum.crop, 
                                fact = 10, 
                                fun = sum, 
                                expand = TRUE)

popsum.agg.df <- raster::as.data.frame(popsum.agg, xy=TRUE) 
popsum.agg.df <- na.omit(popsum.agg.df) 
popsum.agg.df <- popsum.agg.df %>%
  filter(popsum_constrained != 0) 

#### 2. PLOT ####
#### 2.1 access geog map #### 
# create fixed interval
fix.brks <- classIntervals(popsum.agg.df$popsum_constrained, n = 7, style="fixed",
               fixedBreaks=c(0, 10, 25, 50, 100, 250, 500, Inf),
               intervalClosure="right")

popsum.agg.df <- popsum.agg.df %>%
  mutate(pop_cat = cut(popsum_constrained, fix.brks$brks))

# plot
map = ggplot() + 
  geom_sf(data = adm0, fill = "#d9d9d9", alpha = 1,
          show.legend = NA, color = NA) + 
  geom_raster(data = popsum.agg.df, 
              aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",
    values = c("#c7e9c0", "#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b"),
    na.value = "#d9d9d9") +  
  # add adm1 outlines
  geom_sf(data = adm1, fill = NA, lwd = 2, colour="#737373", show.legend = NA) + # as adm1 polygon outline
  geom_sf(data = adm0, fill = NA, lwd = 3, colour="#737373", show.legend = NA) + # as adm1 polygon outline
  # add north arrow and scale
  annotation_scale(location = "br", pad_x = unit(0.5, "in"),
                   pad_y = unit(0.3, "in"),
                   width_hint = 0.15, height = unit(0.15, "cm",)) +
  annotation_north_arrow(location = "br", 
                         which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 8.0)) +
  
  scale_color_manual(name="",values = c("Country" = "red"), 
                     labels = "Permanent\ncataract\nsurgery facility") +
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
        plot.background = element_rect(fill='transparent', color=NA))
  
#### 2.2 barchart legend using ggplot ####
# process data
pop_pct_by_density_at_adm0 <- as.data.frame(popsum.agg.df) %>%
  group_by(pop_cat) %>%
  summarise_at(c("popsum_constrained"), sum,
               na.rm = TRUE) %>%
  mutate(row_id = row_number(),
         class = case_when(
           row_id == 0 ~ "0",
           row_id == 1 ~ "1 - 10",
           row_id == 2 ~ "11 - 25",
           row_id == 3 ~ "26 - 50",
           row_id == 4 ~ "51 - 100",
           row_id == 5 ~ "101 - 250",
           row_id == 6 ~ "251 - 500",
           row_id == 7 ~ "> 500")) %>%
  ungroup() %>%
  mutate(lab_col = ifelse(row_id == 7, "#de77ae", "#000000"),
         pop = round(popsum_constrained, digits = 0),
         pop_pct = round((popsum_constrained / sum(popsum_constrained) * 100), digits = 1)) %>%
  mutate(country = "Rwanda") 

hist_factor = 2
legend_w = 4
vline_w = 0.2

bar_adm0 <- pop_pct_by_density_at_adm0 %>%
  mutate(pop_pct1 = pop_pct + legend_w + vline_w,
         alpha = 1,
         row_id = row_number(),
         label=(paste0(sprintf("%1.1f", pop_pct), "%"))) %>% 
  add_row(class = "0", alpha = 1, row_id = 0,
          pop_pct1 = legend_w)

# plot
barchart = ggplot(bar_adm0, aes(x = reorder(class, -row_id), 
                                y = pop_pct1/hist_factor,
                                fill = as.factor(pop_cat), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#c7e9c0", "#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b"),
     na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),
                     guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8,
           color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label, 
                color = lab_col,
                y = 3),
            size = 2.8, 
            position=position_dodge(width=0.9), vjust= 0.5,
            hjust= 0) + # align left
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Density of Rwanda population\naged 50+ (persons per km\u00B2) and\n% of population by density",
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
# set map path and name
combined <- here("data/rwanda","maps/original_resolution",
               "Rwanda_MapA_Combined.png")

# plot
combined_plots = ggdraw() +
  draw_plot(map, height = 1, width = 1) +
  expand_limits(x = -0.4, y = -0.4) +
  draw_plot(barchart, x = 0, y = 0.62, hjust = 0, vjust = 0,
            width = 0.32, height = 0.38) +
  theme(plot.margin = margin(t=0.4, l= -0.1, b=0.1, r=0, "cm"),
        plot.background = element_rect(fill='transparent', color=NA))

# ggsave(plot = combined_plots, file = combined, 
#        type = "cairo-png",
#        bg = "white",
#        width = 21, height = 16, units = "cm", dpi = 600)
