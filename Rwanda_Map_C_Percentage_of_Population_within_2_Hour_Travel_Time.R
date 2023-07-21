# This code creates a map showing the percentage of Rwanda's county population aged 50 and over
# within 2 hours of motorised travel time to a permanent cataract surgery facility.
# The bar chart legend displays the percentage of Rwanda population aged 50 and over by county.

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

#### 1. DATA #### 

# Rwanda adm2 boundary
adm2 <- st_read(here("data","rwanda","rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync","rwa_adm2_2006_NISR_WGS1984_20181002.shp"))
adm2 <- adm2[, c("ADM0_EN", "ADM1_EN", "ADM2_EN")]

# import table derived from Rwanda_Map_B_Travel_Time.R
pop_pct_by_access_2hr_at_adm2 <- read.csv(here("data/rwanda","raster2polygon",
                                               "Rwanda_pop_pct_by_adm2_for_travel_minutes_le_120.csv"))

adm2.df <- merge(adm2, pop_pct_by_access_2hr_at_adm2,
                 by.x="ADM2_EN", by.y = "ADM2_EN", all.x = T) 

brks <- c("-0.01", "10", "20", "30", "40", "50",
          "60", "70", "80", "90", "100")

adm2.df <- adm2.df %>%
  mutate(pct_cat = cut(pop_pct, brks)) 

#### 2. PLOT ####
#### 2.1 % of population map #### 
map = ggplot() + 
  geom_sf(data = adm2.df, aes(fill = factor(pct_cat)),
                            lwd = 0, colour=NA,
                            show.legend = NA) + 
  scale_fill_manual(values = c("#b8e186","#7fbc41", "#276419"),
                    na.value = "#d9d9d9") +
  geom_sf(data = adm2, fill = NA, lwd = 1.5, colour="#737373", show.legend = NA) + # as adm2 polygon outline
    # add north arrow and scale
  annotation_scale(location = "br", pad_x = unit(0.5, "in"),
                   pad_y = unit(0.3, "in"),
                   width_hint = 0.15, height = unit(0.15, "cm",)) +
  annotation_north_arrow(location = "br", 
                         which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 8.0)) +
    theme(plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 20),
        panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), # optional - remove gridlines
        panel.grid.minor = element_blank(), # optional - remove gridlines
        plot.background = element_rect(fill='transparent', color=NA),
        legend.position = "none") 

#### 2.2 barchart legend using ggplot ####

hist_factor = 3
legend_w = 10
vline_w = 0.55
nzrate = 97.2 

adm2.df.2 <- adm2.df %>%
  mutate(pop_pct2 = pop_pct + legend_w) %>%
  mutate(lab_col = ifelse(pop_pct >= 90, "#de77ae", "#000000"))

barchart = ggplot(adm2.df.2, aes(x =reorder(ADM2_EN,pop_pct2),
                             y = pop_pct2/hist_factor,
                             fill = as.factor(pct_cat))) +
  coord_flip() +
  scale_fill_manual(values = c("#b8e186","#7fbc41","#276419"),
                    na.value = "#d9d9d9") +
    geom_bar(stat = 'identity',  width = 1.0, 
           color="#969696", linewidth = 0.05, na.rm = TRUE) + 
  geom_vline(xintercept = 2.5, color = "#525252", linewidth = 0.25) +
  geom_hline(aes(yintercept=100/hist_factor+legend_w/hist_factor),
             col = "#525252", linewidth = 0.25) +
  geom_hline(aes(yintercept=legend_w/hist_factor),col = "white",
             linewidth = vline_w) +
  geom_text(aes(label=paste0(sprintf("%1.1f", pop_pct), "%"),
                color = lab_col,
                y = 8.7),
            size = 2.6, colour="#252525",
            position=position_dodge(width=0.9), vjust= 0.5,
            hjust= 0.5) +
  geom_text(aes(label="100.0 %", x=0.725,y= 34.5), 
            size = 2.8, colour="#252525", hjust= 0.5) +
  geom_text(aes(label="97.2 % \nNational \nrate ", x=2.0,y= 49),
            size = 2.8, colour="#252525", hjust= 1) + # hjust= 1 - right aligned
  theme_bw() +
  labs(title="% of the district population aged 50+\nwithin 2 hours of travel time to\npermanent cataract surgery facility", 
       x="", y = "", colour="#000000") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, hjust = 0, vjust = 0),
        axis.text.y = element_text(size=8.2, colour="#252525"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 0.5,  # Top margin
                             r = 1,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0,
                             unit = "cm")) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        aspect.ratio = 2/1) 


#### 2.3 combined plots ####
combined <- here("data/rwanda","maps/original_resolution",
               "Rwanda_Map2_Combined.png")

combined_plots = ggdraw() +
  draw_plot(map, height = 1, width = 1.2) +
  expand_limits(x = -0.9, y = 0) +
  draw_plot(barchart, x = -0.1, y = 0.01, hjust = 0, 
            vjust = 0, scale = 1,
            width = 0.6, height = 0.98) + # width control bar length
  theme(plot.margin = margin(t=0.1, l=1.2, b=0.1, r=0.0, "cm"),
    plot.background = element_rect(fill='transparent', color=NA))
  

ggsave(plot = combined_plots, file = combined,
       type = "cairo-png",
       bg = "white",
       width = 27, height = 15, units = "cm", dpi = 600)
