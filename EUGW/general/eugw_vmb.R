# VMB - EUGW comparision

library(terra)
library(sf)
library(dplyr)
library(caret)
library(ggplot2)

vmbALL <- st_read("Milovice_Mlada/MM_VMB_ALL.gpkg")


r <- rast("Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20220101_20221231_GTYH_CLASS.tif")
plot(r)

evl <- st_read("data/MM_buff1000.gpkg")
plot(evl, add = T)

crs(r) == crs(evl) && crs(evl) == crs(vmb) && crs(vmb) == crs(vmbX) && crs(vmbX) == crs(vmbALL)

r <- crop(r, evl)
r <- mask(r, evl)

vmb <- vect(vmb)
vmbX <- vect(vmbX)
vmbALL <- vect(vmbALL)

vmbALL$BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", vmbALL$BIOTOP_SEZ))

r_bc <- rasterize(
  vmbALL,
  r,                   
  field = "BIOTOP_CODES",
  background = NA 
)
r_fsb <- rasterize(
  vmbALL,
  r,
  field      = "FSB",
  background = NA
)
r_vmb <- c(r_bc, r_fsb)
names(r_vmb) <- c("BIOTOP_CODES", "FSB")

plot(r_vmb$FSB == "L")
x <- unique(r_vmb$BIOTOP_CODES)

r_gt <- r_vmb$FSB
r_model <- r

r_both <- c(r_gt, r_model)   # 1. vrstva = FSB, 2. vrstva = predicted_label

df <- as.data.frame(r_both, na.rm = FALSE, xy = FALSE)
str(df)
head(df)
unique(df$predicted_label)

# vyhodíme pixely bez GT (FSB = NA)
df2 <- df[!is.na(df$FSB), ]

# modelové predikce: 21–27 necháme, NA zůstane, převedeme na znak
df2$pred_cat <- ifelse(is.na(df2$predicted_label),
                       "NA",
                       as.character(df2$predicted_label))

# pro kontrolu
table(df2$FSB, useNA = "ifany")
table(df2$pred_cat, useNA = "ifany")

# tab <- df2 %>%
#   group_by(FSB, pred_cat) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   group_by(FSB) %>%
#   mutate(prop = n / sum(n))
# tab
# print(tab, n = 30)
# 
# count_obs <- tab %>%
#   group_by(FSB) %>%
#   summarise(n = sum(n))
# 
# labels_map <- c(
#   "21" = "Dry grassland",
#   "22" = "Mesic grassland",
#   "23" = "Wet and seasonally wet grassland",
#   "24" = "Alpine and sub-alpine grassland",
#   "25" = "Forest clearings",
#   "26" = "Inland salt steppes",
#   "27" = "Sparsely wooded grassland",
#   "NA" = "Unclassified"
# )
# 
# ggplot(tab, aes(x = FSB, y = prop, fill = pred_cat)) +
#   geom_col() +
#   geom_text(
#     data = count_obs,
#     aes(x = factor(FSB), y = 1.02, label = paste0("n = ", n)),   # y = 1.02 → kousek nad 100 %
#     inherit.aes = FALSE,
#     size = 3#,
#     #angle = 45
#   ) +
#   labs(
#     x = "Reference",
#     y = "Proportion of model prediction",
#     fill = "Predicted Grassland type",
#     title = "Mapped habitat types ~ EUGW predicted grassland"
#   ) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   scale_x_discrete(
#     labels = c(
#       K = "Scrub",
#       L = "Forests",
#       M = "Wetlands,\nriverine vegetation",
#       T = "Secondary grasslands,\nheathlands",
#       V = "Streams,\nwater bodies",
#       X = "Habitats strongly\ninfluenced or\ncreated by man"
#     )
#   ) +      
#   scale_fill_manual(
#     values = c(
#       "21" = "#f5ca7a",
#       "22" = "#a5f57a",
#       "23" = "#7ab6f5",
#       "24" = "#ca7af5",
#       "25" = "#5c8944",
#       "26" = "#f57a7a",
#       "27" = "#895a44",
#       "NA" = "#cccccc"
#     ),
#     labels = labels_map
#   )


# mapping
codes_21 <- c("T6.1", "T6.2", "T3.4", "T3.3", "T5.4","T2.2", "T2.3", "T5.1", "T5.3", "T5.5", "T5.2", "T3.4D")

codes_22 <- c("T1.3", "T1.1", "T1.2")

codes_23 <- c("T1.4", "T1.7", "T1.5", "T1.10", "T1.9")

codes_24 <- c("A3", "T2.1", "A1.2", "A1.1", "A5")

codes_25 <- c("T4.1", "T4.2", "T1.6", "M5", "M7", "T1.8", "A4.2", "A4.3", "A4.1")

codes_26 <- c("M2.4")

# pro jistotu kontrola shody
stopifnot(
  all.equal(ext(r_model), ext(r_bc)),
  all.equal(res(r_model), res(r_bc)),
  crs(r_model) == crs(r_bc)
)

# sloučit rastry a udělat tabulku
r_both <- c(r_model, r_bc)
names(r_both) <- c("predicted_label", "BIOTOP_CODES")

df <- as.data.frame(r_both, na.rm = FALSE)

# vyhodíme pixely bez referenční informace
df <- df[!is.na(df$BIOTOP_CODES), ]

df <- df %>%
  mutate(
    model_class = ifelse(is.na(predicted_label),
                         "NA",
                         as.character(predicted_label)),
    ref_class = case_when(
      BIOTOP_CODES %in% codes_21 ~ "21",
      BIOTOP_CODES %in% codes_22 ~ "22",
      BIOTOP_CODES %in% codes_23 ~ "23",
      BIOTOP_CODES %in% codes_24 ~ "24",
      BIOTOP_CODES %in% codes_25 ~ "25",
      BIOTOP_CODES %in% codes_26 ~ "26",
      TRUE                        ~ "Non-grassland habitats"   # vše ostatní (K3, L*, X*, …)
    ),
    # pro jistotu pořadí modelových tříd v ose X
    model_class = factor(model_class,
                         levels = c("NA", "21", "22", "23", "24", "25", "26", "27")
    )
  )
df

tab <- df %>%
  group_by(model_class, ref_class) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model_class) %>%
  mutate(prop = n / sum(n))
tab

count_obs <- tab %>%
  group_by(model_class) %>%
  summarise(n = sum(n), .groups = "drop")
count_obs

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

labels_map2 <- c(
  "21" = "Dry grassland",
  "22" = "Mesic grassland",
  "23" = "Wet and seasonally wet grassland",
  "24" = "Alpine and sub-alpine grassland",
  "25" = "Forest clearings",
  "26" = "Inland salt steppes",
  "27" = "Sparsely wooded grassland",
  "Non-grassland habitats" = "Non-grassland habitats"
)

labels_map3 <- c(
  "21" = "Dry grassland",
  "22" = "Mesic grassland",
  "23" = "Wet and seasonally wet grassland",
  "24" = "Alpine and sub-alpine grassland",
  "25" = "Forest clearings",
  "26" = "Inland salt steppes",
  "27" = "Sparsely wooded grassland",
  "non-Dry grassland" = "non-Dry grassland"
)

# volitelné: pořadí tříd na ose X
tab$model_class <- factor(tab$model_class,
                          levels = c("21", "22", "23", "24", "25", "26", "27", "NA")
)
count_obs$model_class <- factor(count_obs$model_class,
                                levels = c("21", "22", "23", "24", "25", "26", "27", "NA")
)

ggplot(tab, aes(x = model_class, y = prop, fill = ref_class)) +
  geom_col() +
  geom_text(
    data = count_obs,
    aes(x = model_class, y = 1.02, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    x = "Predicted grassland type (EUGW)",
    y = "Proportion of reference classes",
    fill = "Reference class (field mapped units)",
    title = "EUGW predicted grassland ~ mapped habitat types"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  scale_x_discrete(labels = labels_map) +
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
      "Non-grassland habitats" = "darkgrey"
    ),
    labels = labels_map2
  )

### merged layer

tab_collapsed <- tab %>%
  mutate(
    ref_class = if_else(ref_class == "21", "21", "non-Dry grassland")
  ) %>%
  group_by(model_class, ref_class) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(model_class) %>%
  mutate(prop = n / sum(n))
tab_collapsed

ggplot(tab_collapsed, aes(x = model_class, y = prop, fill = ref_class)) +
  geom_col() +
  geom_text(
    data = count_obs,
    aes(x = model_class, y = 1.02, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    x = "Predicted grassland type (EUGW)",
    y = "Proportion of reference class 21",
    fill = "Reference class Dry grassland",
    title = "EUGW predicted grassland ~ mapped Dry grassland habitats"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  scale_x_discrete(labels = labels_map) +
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
      "non-Dry grassland" = "darkgrey"
    ),
    labels = labels_map3
  )





# percentual reference layer
ref_totals <- tab %>%
  group_by(ref_class) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(
    prop = n / sum(n)
  )
ref_totals

ref_totals_collapsed <- ref_totals %>%
  mutate(ref_group = if_else(ref_class == "21", "21", "non-21")) %>%
  group_by(ref_group) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(prop = n / sum(n))
ref_totals_collapsed
