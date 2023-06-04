## load packages
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library(gridExtra)

# import data
## article data
art_data <- read_excel("Spatial_distribution_data.xlsx") %>%
  select(c(1,2)) %>% rename(Article = 1)
## world boundary data
world <- read_sf("./world_shp/world.shp")
# check uniformity in name range and correct non-uniform name
common_name <- world$COUNTRY[which(world$COUNTRY %in% art_data$`Region covered`)]
regi_cov <- art_data$`Region covered`
diff_name <- regi_cov[! regi_cov %in% common_name]

# add new Name column to world data correcting country name
world <- world %>%
  rename(`Region covered` = 1) %>%
  mutate(`Region covered` = case_when(`Region covered` == "Gabon" ~ "Guyana/ Gabon",
                                      `Region covered` == "Saudi Arabia" ~ "Saudi arabia",
                                      `Region covered` == "United States" ~ "USA",
                                      `Region covered` == "India" ~ "Indian",
                                      TRUE ~ `Region covered`
                                      )
         )
## us state
us <- read_sf("./world_shp/us.shp") %>%
  select(NAME) %>% rename(`Region covered` = 1) %>%
  filter(`Region covered` %in% c("California", "Florida")) %>%
  st_transform(., crs = st_crs(world))
world <- world %>% rbind(us) %>%
  mutate(`Region covered` = case_when(`Region covered` == "Florida" ~ "Florid",
                                      TRUE ~ `Region covered`))

ready_df <- world %>%
  left_join(x = ., y = art_data, by = "Region covered")

### abreviation table
abr_df <- st_centroid(ready_df) %>%
  filter(!is.na(Article)) %>%
  mutate(ABR = substr(`Region covered`, 1,2))
## seperate abr_df
sep_abr_df <- cbind(
  data.frame(Label = paste(abr_df$ABR, ":"), Meaning = abr_df$`Region covered`)[1:10,],
  data.frame(Label = paste(abr_df$ABR, ":"), Meaning = abr_df$`Region covered`)[11:20,]
)
## set font
library(showtext)
font_add("mts_medi", "./fonts/Montserrat-Medium.ttf")
font_add("mts_medi", "./fonts/Montserrat-Medium.ttf")
showtext_opts(dpi = 300)
showtext_auto()


ggplot()+
  geom_sf(data = ready_df,
          fill = ifelse(is.na(ready_df$Article), "#dcdcdc", "#470071"),
          color = "#dcdcdc")+
  geom_sf(data = st_centroid(ready_df), aes(size = Article, color = 1),
          color = ifelse(is.na(ready_df$Article), NA, "#f68f00"))+
  scale_size(range = c(1,8))+
  geom_sf_label(data = abr_df, aes(label = ABR), nudge_x = 1.8, nudge_y = 4,
                label.r = unit(0.3, "lines"), label.size = 0.3,
                color = "#470071", family = "mts_medi", size = 3)+
  annotation_custom(gridExtra::tableGrob(sep_abr_df, rows = NULL,
                                         theme = ttheme_default(base_size = 7, base_family = "mts_medi")),
                    xmin = -170, ymin = -40, xmax = -110, ymax = -40)+
  theme_void()+
  guides(size = guide_legend(
    title = "Article number:",
    direction = "horizontal",
    #title.position = "top",
    label.position = "top",
    title.theme = element_text(size = 10, hjust = 0.5, family = "mts_medi"),
    label.theme = element_text(size = 8, family = "mts_medi"),
    override.aes = list(color = "#f68f00")
  ))+
  theme(
    legend.position = c(0.14, 0.48)
  )

ggsave("Pub_dist.jpeg", width = 30, height = 16, units = "cm", dpi = 300)
ggsave("Pub_dist.pdf", width = 30, height = 16, units = "cm", dpi = 300)

