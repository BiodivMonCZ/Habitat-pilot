#Packages
library(terra)
library(dplyr)
library(ggplot2)

r <- rast("Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20220101_20221231_GTYH_CLASS.tif")
vmbALL <- vect("Milovice_Mlada/MM_VMB_ALL.gpkg")
evl <- vect("data/MM_buff1000.gpkg")

crs(r) == crs(evl) && crs(evl) == crs(vmbALL)

r <- crop(r, evl)
r <- mask(r, evl)

vmbALL$BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", vmbALL$BIOTOP_SEZ))

r_bc <- rasterize(
  vmbALL,
  r,                   
  field = "BIOTOP_CODES",
  background = NA 
)

ext(r) == ext(r_bc)
res(r) == res(r_bc)

# merge
r_both_bc <- c(r, r_bc)
names(r_both_bc) <- c("predicted_label", "BIOTOP_CODES")

df_bc <- as.data.frame(r_both_bc, na.rm = FALSE)

# drop unmapped pixels
df_bc <- df_bc[!is.na(df_bc$BIOTOP_CODES), ]

# keep classes 21:27
df_bc <- df_bc %>%
  mutate(
    pred_cat = ifelse(is.na(predicted_label),
                      "NA",
                      as.character(predicted_label))
  )

# for HP, EUNIS translation
df_bc <- df_bc %>%
  mutate(
    ref_group = case_when(
      BIOTOP_CODES %in% c("T1.1", "T1.5", "T3.4D", "T5.1", "T5.2", "T5.3") ~ "Grassland",
      BIOTOP_CODES %in% c("K3", "T8.1B") ~ "Heathland",
      BIOTOP_CODES %in% c("L1", "L3.1", "L5.4", "L7.1", "L7.2", "L7.4") ~ "Forest",
      BIOTOP_CODES %in% c("M1.3", "M1.7", "V1G") ~ "Wetlands",
      BIOTOP_CODES %in% c("LP", "X1", "X12A", "X12B", "X13", "X2", "X5", "X6", "X7A", "X7B", "X8", "X9A", "X9B") ~ "Man-made",
      TRUE ~ NA_character_
    )
  )

# for safe
df_bc <- df_bc[!is.na(df_bc$ref_group), ]


# pořadí kategorií na ose X
df_bc$ref_group <- factor(
  df_bc$ref_group,
  levels = c("Grassland", "Heathland", "Forest", "Wetlands", "Man-made")
)

# VERZE PRO VMB

unique(df_bc$BIOTOP_CODES)

# fsb
df_bc <- df_bc %>%
  mutate(
    ref_group = case_when(
      BIOTOP_CODES %in% c("T1.1", "T1.5", "T3.4D", "T5.1", "T5.2", "T5.3", "T8.1B") ~ "T",
      BIOTOP_CODES %in% c("K3") ~ "K",
      BIOTOP_CODES %in% c("L1", "L3.1", "L5.4", "L7.1", "L7.2", "L7.4") ~ "L",
      BIOTOP_CODES %in% c("M1.3", "M1.7", "V1G") ~ "M",
      BIOTOP_CODES %in% c("LP", "X1", "X12A", "X12B", "X13", "X2", "X5", "X6", "X7A", "X7B", "X8", "X9A", "X9B") ~ "X",
      TRUE ~ NA_character_
    )
  )

# biotopy

df_bc <- df_bc %>%
  mutate(
    ref_group = BIOTOP_CODES
  )

unique(df_bc$ref_group)

tab_bc <- df_bc %>%
  group_by(ref_group, pred_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ref_group) %>%
  mutate(prop = n / sum(n))

# # drop small biotopes
# tab_bc <- tab_bc %>%
#   group_by(ref_group) %>%
#   filter(sum(n*0.01) >= 1) %>%
#   ungroup()

# pixel counts
count_obs_bc <- tab_bc %>%
  group_by(ref_group) %>%
  summarise(n = sum(n), .groups = "drop")

labels_map <- c(
  "21" = "Dry grassland",
  "22" = "Mesic grassland",
  "23" = "Wet and seasonally wet grassland",
  "24" = "Alpine and sub-alpine grassland",
  "25" = "Forest clearings",
  "26" = "Inland salt steppes",
  "27" = "Sparsely wooded grassland",
  "NA" = "Unclassified"
)

ggplot(tab_bc, aes(x = ref_group, y = prop, fill = pred_cat)) +
  geom_col() +
  geom_text(
    data = count_obs_bc,
    aes(x = ref_group, y = 1.02, label = paste0(n*0.01, " ha")),
    inherit.aes = FALSE,
    size = 2.5,
    angle = 45,
    hjust = 0,   
    vjust = 0.5
  ) +
  labs(
    x = "Reference",
    y = "Proportion of model predictions",
    fill = "Predicted grassland type",
    title = "Mapped habitat groups ~ EUGW predicted grassland"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c(
      "21" = "#f5ca7a",
      "22" = "#a5f57a",
      "23" = "#7ab6f5",
      "24" = "#ca7af5",
      "25" = "#5c8944",
      "26" = "#f57a7a",
      "27" = "#895a44",
      "NA" = "#cccccc"
    ),
    labels = labels_map
  )

