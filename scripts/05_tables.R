###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS from previous GAMBAROTA scripts (https://github.com/shared-research/face-mask-gew.git) 
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_pilot_AMIM1
#
#  Update:      23/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(dplyr)
library(flextable)
library(forcats)
library(officer)
library(ftExtra)
library(tidybayes)
library(tidyr)
library(here)
library(purrr)

# Functions ---------------------------------------------------------------

devtools::load_all()

save_table <- function(table, path){
  save_as_docx(table, path = path)
}

flextable_with_param <- function(data){
  data %>% 
    flextable() %>% 
    autofit() %>% 
    theme_vanilla() %>% 
    colformat_md(part = "all") %>% 
    fontsize(part = "all", size = 9)
}

get_post_summary <- function(data, group, sign = FALSE, null = 0){
  group <- rlang::enexpr(group)
  out <- data %>% 
    group_by(!!group) %>% 
    median_hdci(value) %>% 
    select(emotion, value, .lower, .upper)
  if(sign){
    out %>% 
      mutate(across(where(is.numeric), round, 3),
             value_chr = sprintf("**%s** [%s, %s]", value, .lower, .upper),
             value_chr = ifelse(.lower <= null & null <= .upper,
                                value_chr,
                                paste(value_chr, "*")))
  }else{
    out
  }
}

set_emotion_order <- function(data, col, levels, dpar = TRUE){
  col <- rlang::enexpr(col)
  data %>% 
    mutate(!!col := factor(!!col, levels = levels)) %>% {
      if(dpar){
        arrange(., main_param, !!col)
      }else{
        arrange(., !!col)
      }
    }
}

# Loading Data ------------------------------------------------------------

datasetname <- "dataset"

dat <- readRDS(file.path("data", paste0(datasetname,"_valid.rds")))
dat <- dat%>% 
  na.omit()%>%
  mutate(intensity = Video.intensity)

emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))

emo_order = c("Surprise", "Sadness", "Happiness", "Fear", "Disgust", "Anger")

# EDA Table ---------------------------------------------------------------

# the mean is the angular mean in radians. the computation is the same as
# using:
# ang <- circular::circular(dat$angle, units = "degrees", modulo = "2pi")
# circular::mean.circular(dat$angle)
# with less computation
# test: rad_to_deg(CircStats::circ.mean(dat$theta)) %% 360

tab_eda <- dat %>% 
  mutate(intensity = Video.intensity)%>%
  group_by(emotion,  intensity) %>%
  summarise(m_angle = rad_to_deg(CircStats::circ.mean(theta)) %% 360,
            var_angle = 1 - CircStats::circ.disp(theta)$var,
            m_int = mean(int),
            sd_int = sd(int)) %>% 
  left_join(., emo_coords %>% dplyr::select(emotion, angle_emo), by = "emotion") %>% 
  dplyr::select(emotion, angle_emo, everything()) %>% 
  clean_emotion_names(emotion) %>% 
  mutate(emotion = factor(emotion)) %>% 
  arrange(emotion) %>% 
  flextable_with_param() %>% 
  colformat_double(digits = 2) %>% 
  colformat_double(j = 2, digits = 0) %>% 
  theme_vanilla() %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    angle_emo = "Wheel Angle°",
    intensity = "Intensity",
    m_angle = "Mean°",
    var_angle = "Var",
    m_int = "Mean",
    sd_int = "SD"
  )) %>% 
  add_header_row(values = c( "", "", "",
                            rep(c("Angle", "Perceived Intensity"), each = 2))) %>% 
  merge_v(j = c(1:3)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all")

# Angle/Theta Full vs Subtle ----------------------------------------------

tab_kappa_angle_intensity_effect <- circular_objects$tidy_post$post_fit_ri_diff_int %>% 
  group_by(emotion, .draw) %>% 
  summarise(angle_full = mean(angle_full, na.rm = TRUE),
            angle_subtle = mean(angle_subtle, na.rm = TRUE),
            angle_diff = mean(angle_diff, na.rm = TRUE),
            kappa_inv_full = mean(kappa_inv_full, na.rm = TRUE),
            kappa_inv_subtle = mean(kappa_inv_subtle, na.rm = TRUE),
            kappa_inv_ratio = mean(kappa_inv_ratio, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(emotion, angle_full, angle_subtle, angle_diff, kappa_inv_full, kappa_inv_subtle, kappa_inv_ratio, .draw) %>%
  na.omit()%>%
  pivot_longer(2:(ncol(.)-1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = ifelse(startsWith(param, "kappa"), 1, 0)) %>% 
  mutate(summary = map2(data, null, function(x, y) get_post_summary(x, emotion, sign = TRUE, null = y))) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(main_param = ifelse(startsWith(param, "kappa"), "Uncertainty", "Bias"),
         contr_param = case_when(grepl("full", param) ~ "Intensity~full~",
                                 grepl("subtle", param) ~ "Intensity~subtle~",
                                 TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  select(-param) %>%
  pivot_wider(names_from = contr_param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    main_param = "Parameter"))

# Int full vs subtle ------------------------------------------------------

tab_int_intensity_effect <- intensity_objects$tidy_post$post_fit_ri_int %>% 
  group_by(emotion, intensity, .draw) %>% 
  summarise(int = mean(int)) %>% 
  pivot_wider(names_from = intensity, values_from = int) %>% 
  mutate(int_diff = full - subtle) %>% 
  select(emotion, full, subtle, int_diff, .draw) %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = 0,
         summary = map(data, get_post_summary, emotion, TRUE)) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(param = case_when(grepl("full", param) ~ "Intensity~full~",
                           grepl("subtle", param) ~ "Intensity~subtle~",
                           TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>%
  set_emotion_order(emotion, emo_order, dpar = FALSE) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion"
  ))

# Accuracy GEW ------------------------------------------------------------

tab_acc_gew <- dat %>% 
    filter(emotion != "neutrality") %>% 
    mutate(acc = ifelse(emotion == resp_emotion_label, 1, 0)) %>%
    group_by(emotion, intensity) %>% 
    summarise(acc = mean(acc)) %>% 
    clean_emotion_names(emotion) %>% 
    set_emotion_order(emotion, emo_order, FALSE) %>% 
    pivot_wider(names_from = emotion, values_from = acc) %>% 
    flextable() %>% 
    colformat_double(digits = 2) %>% 
    autofit() %>% 
    merge_v(j = 1:2) %>% 
    theme_vanilla() %>% 
    align(align = "center") %>% 
    set_header_labels(values = list(
      "intensity" = "Intensity"
    ))
  
# Saving ------------------------------------------------------------------

tab_list <- make_named_list(tab_eda, tab_kappa_angle_intensity_effect, tab_int_intensity_effect,
                            tab_acc_gew)

tab_files <- paste0(names(tab_list), ".docx")

saveRDS(tab_list, file = here("objects", "paper_tables.rds"))
purrr::walk2(tab_list, file.path("tables", tab_files), save_table)

#################################################
# 
# END
#
#################################################