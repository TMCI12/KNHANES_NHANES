knh <- read.csv("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/knh.csv", header = TRUE, stringsAsFactors = FALSE)
nh <- read.csv("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/nh.csv", header = TRUE, stringsAsFactors = FALSE)

knh$Depression <- ifelse(knh$PHQ_g %in% c(3, 4), 1, 0)
nh$Depression <- ifelse(nh$PHQ_g %in% c(3, 4), 1, 0)
knh$PHQ_g <- as.character(knh$PHQ_g)
nh$PHQ_g  <- as.character(nh$PHQ_g)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(pufa_pct, 3) + age_g + sex + alcohol + educ_g + incm_g + smk, data = knh)
pred_knh <- Predict(fit_knh, pufa_pct = seq(0, 30, by = 0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val <- pred_knh$yhat[pred_knh$pufa_pct == 0]
pred_knh_df <- pred_knh %>%
  mutate(
    OR = exp(yhat - ref_val),
    lower_OR = exp(lower - ref_val),
    upper_OR = exp(upper - ref_val),
    Country = "KNHANES"
  )

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(pufa_pct, 3) + age_g + RIAGENDR +drink_g + educ_g + incm_g + smk , data = nh)
pred_nh <- Predict(fit_nh, pufa_pct = seq(0, 30, by = 0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[pred_nh$pufa_pct == 0]
pred_nh_df <- pred_nh %>%
  mutate(
    OR = exp(yhat - ref_val_nh),
    lower_OR = exp(lower - ref_val_nh),
    upper_OR = exp(upper - ref_val_nh),
    Country = "NHANES"
  )

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

breaks <- c(0, 5, 10, 15, 20, 25, 30)
labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30")

knh$PUFA_group <- cut(knh$pufa_pct,
                      breaks = breaks,
                      include.lowest = TRUE,
                      right = FALSE,
                      labels = labels)

nh$PUFA_group <- cut(nh$pufa_pct,
                     breaks = breaks,
                     include.lowest = TRUE,
                     right = FALSE,
                     labels = labels)

pufa_count_knh <- knh %>% group_by(PUFA_group) %>%
  summarise(N = n()) %>%
  mutate(Country = "KNHANES")

pufa_count_nh <- nh %>% group_by(PUFA_group) %>%
  summarise(N = n()) %>%
  mutate(Country = "NHANES")

pufa_count_all <- bind_rows(pufa_count_knh, pufa_count_nh)

pufa_count_all <- bind_rows(
  knh %>% mutate(Country = "KNHANES"),
  nh %>% mutate(Country = "NHANES")
) %>%
  mutate(
    PUFA_group = cut(
      pufa_pct,
      breaks = seq(0, 30, by = 5),
      right = FALSE,
      include.lowest = TRUE,
      labels = c("0s", "5s", "10s", "15s", "20s", "25s")
    )
  ) %>%
  group_by(Country, PUFA_group) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(pct = N / sum(N) * 100)

y_max <- max(pred_all$upper_OR, na.rm = TRUE)
pufa_count_all <- pufa_count_all %>%
  mutate(N_scaled = pct / 100 * y_max * 0.6)

pufa_count_all <- pufa_count_all %>%
  mutate(PUFA_mid = as.numeric(gsub("s", "", PUFA_group)) + 2.5)

ggplot() +
  geom_line(data = pred_all, aes(x = pufa_pct, y = OR, color = Country), size = 1.2) +
  geom_ribbon(data = pred_all, aes(x = pufa_pct, ymin = lower_OR, ymax = upper_OR, fill = Country), alpha = 0.2) +
  geom_col(data = pufa_count_all, aes(x = PUFA_mid, y = N_scaled, fill = Country), 
           position = position_dodge(width = 5), width = 5, alpha = 0.3) +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values  = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_y_continuous(
    name = "Odds Ratio",
    limits = c(0, 2.0),
    sec.axis = sec_axis(~ . * 100, name = "Percentage of Participants (%)", breaks = seq(0, 100, 20))
  ) +
  scale_x_continuous(
    name = "Total PUFA (% of energy)",
    breaks = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
  theme_minimal(base_family = "serif") + 
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.title.y.right = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(margin = margin(r = 0), size = 16),
    axis.text.y.right = element_text(margin = margin(l = 0), size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
ggsave(
  filename = "\\\\KHU_Server2\\KDH\\최태림\\KNHANES,NHANES\\6. PUFA-Asthma\\Figure\\spline_pufa_depression.png",
  width = 8, height = 10, dpi = 1000, bg = "transparent"
)

anova_result_knh <- anova(fit_knh)
print(anova_result_knh)

anova_result_nh <- anova(fit_nh)
print(anova_result_nh)

min(knh$n3_pct, na.rm = TRUE)
max(knh$n3_pct, na.rm = TRUE)
min(nh$n3_pct, na.rm = TRUE)
max(nh$n3_pct, na.rm = TRUE)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(n3_pct, 5) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh, n3_pct = seq(0, 6, by = 0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val <- pred_knh$yhat[pred_knh$n3_pct == 0]
pred_knh_df <- pred_knh %>%
  mutate(
    OR = exp(yhat - ref_val),
    lower_OR = exp(lower - ref_val),
    upper_OR = exp(upper - ref_val),
    Country = "KNHANES"
  )

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(n3_pct, 5) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh, n3_pct = seq(0, 6, by = 0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[pred_nh$n3_pct == 0]
pred_nh_df <- pred_nh %>%
  mutate(
    OR = exp(yhat - ref_val_nh),
    lower_OR = exp(lower - ref_val_nh),
    upper_OR = exp(upper - ref_val_nh),
    Country = "NHANES"
  )

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

breaks <- seq(0, 6, by = 2)
labels <- c("0-2", "2-4", "4-6")

get_midpoint <- function(label) {
  parts <- strsplit(label, "-")[[1]]
  mean(as.numeric(parts))
}

pufa_count_all <- bind_rows(
  knh %>% mutate(Country = "KNHANES"),
  nh %>% mutate(Country = "NHANES")
) %>%
  filter(n3_pct >= 0 & n3_pct <= 6) %>%
  mutate(
    pufa_group10 = cut(n3_pct, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)
  ) %>%
  group_by(Country, pufa_group10) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(
    pct = N / sum(N) * 100,
    pufa_mid = sapply(as.character(pufa_group10), get_midpoint)
  )

y_max <- max(pred_all$upper_OR, na.rm = TRUE)
pufa_count_all <- pufa_count_all %>%
  mutate(N_scaled = pct * (1.0 / 100))
scale_factor <- 100 / 1.0 

ggplot() +
  geom_line(data = pred_all, aes(x = n3_pct, y = OR, color = Country), size = 1.2) +
  geom_ribbon(data = pred_all, aes(x = n3_pct, ymin = lower_OR, ymax = upper_OR, fill = Country), alpha = 0.2) +
  geom_col(data = pufa_count_all,
           aes(x = pufa_mid, y = N_scaled, fill = Country),
           position = position_dodge(width = 1),
           width = 1,
           alpha = 0.3) +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values  = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_y_continuous(
    name = "Odds Ratio",
    sec.axis = sec_axis(~ . * scale_factor,
                        name = "Percentage of Participants (%)",
                        breaks = seq(0, 100, 20))
  ) +
  scale_x_continuous(
    name = "n-3 PUFA (% of energy)",
    breaks = seq(0, 6, by = 2),
    limits = c(0, 6),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
  theme_minimal(base_family = "serif") +
  theme_minimal(base_family = "serif") + 
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.title.y.right = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(margin = margin(r = 0), size = 16),
    axis.text.y.right = element_text(margin = margin(l = 0), size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggsave(
  filename = "\\\\KHU_Server2\\KDH\\최태림\\KNHANES,NHANES\\6. PUFA-Asthma\\Figure\\spline_n3_depression.png",
  width = 8, height = 10, dpi = 1000, bg = "transparent"
)
anova_result_knh <- anova(fit_knh)
print(anova_result_knh)

anova_result_nh <- anova(fit_nh)
print(anova_result_nh)

min(knh$n6_pct, na.rm = TRUE)
max(knh$n6_pct, na.rm = TRUE)
min(nh$n6_pct, na.rm = TRUE)
max(nh$n6_pct, na.rm = TRUE)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(n6_pct, 3) + age_g + sex , data = knh)
pred_knh <- Predict(fit_knh, n6_pct = seq(0, 25, by=0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val <- pred_knh$yhat[pred_knh$n6_pct == 0]
pred_knh_df <- pred_knh %>%
  mutate(
    OR = exp(yhat - ref_val),
    lower_OR = exp(lower - ref_val),
    upper_OR = exp(upper - ref_val),
    Country = "KNHANES"
  )

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(n6_pct, 3) + age_g + RIAGENDR +drink_g + educ_g + incm_g + smk , data = nh)
pred_nh <- Predict(fit_nh, n6_pct = seq(0, 25, by=0.1), fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[pred_nh$n6_pct == 0]
pred_nh_df <- pred_nh %>%
  mutate(
    OR = exp(yhat - ref_val_nh),
    lower_OR = exp(lower - ref_val_nh),
    upper_OR = exp(upper - ref_val_nh),
    Country = "NHANES"
  )

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

breaks <- c(0, 5, 10, 15, 20, 25)
labels <- c("0-5", "5-10", "10-15", "15-20", "20-25")

knh$pufa_group10 <- cut(knh$n6_pct,
                        breaks = breaks,
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = labels)

nh$pufa_group10 <- cut(nh$n6_pct,
                       breaks = breaks,
                       include.lowest = TRUE,
                       right = FALSE,
                       labels = labels)

pufa_count_knh <- knh %>% group_by(pufa_group10) %>%
  summarise(N = n()) %>%
  mutate(Country = "KNHANES")

pufa_count_nh <- nh %>% group_by(pufa_group10) %>%
  summarise(N = n()) %>%
  mutate(Country = "NHANES")

pufa_count_all <- bind_rows(pufa_count_knh, pufa_count_nh)

pufa_count_all <- bind_rows(
  knh %>% mutate(Country = "KNHANES"),
  nh %>% mutate(Country = "NHANES")
) %>%
  mutate(
    pufa_group10 = cut(
      n6_pct,
      breaks = seq(0, 30, by = 5),
      right = FALSE,
      include.lowest = TRUE,
      labels = c("0s", "5s", "10s", "15s", "20s", "25s")
    )
  ) %>%
  group_by(Country, pufa_group10) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(pct = N / sum(N) * 100)

y_max <- max(pred_all$upper_OR, na.rm = TRUE)
pufa_count_all <- pufa_count_all %>%
  mutate(N_scaled = pct / 100 * y_max * 0.6)

pufa_count_all <- pufa_count_all %>%
  mutate(pufa_mid = as.numeric(gsub("s", "", pufa_group10)) + 2.5)

ggplot() +
  geom_line(data = pred_all, aes(x = n6_pct, y = OR, color = Country), size = 1.2) +
  geom_ribbon(data = pred_all, aes(x = n6_pct, ymin = lower_OR, ymax = upper_OR, fill = Country), alpha = 0.2) +
  geom_col(data = pufa_count_all, aes(x = pufa_mid, y = N_scaled, fill = Country), 
           position = position_dodge(width = 5), width = 5, alpha = 0.3) +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values  = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_y_continuous(
    name = "Odds Ratio",
    limits = c(0, 2.0),
    sec.axis = sec_axis(~ . * 100, name = "Percentage of Participants (%)", breaks = seq(0, 100, 20))
  ) +
  scale_x_continuous(
    name = "n-6 PUFA (% of energy)",
    breaks = seq(0, 25, by = 5),
    limits = c(0, 25),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
  theme_minimal(base_family = "serif") + 
  theme_minimal(base_family = "serif") + 
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.title.y.right = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(margin = margin(r = 0), size = 16),
    axis.text.y.right = element_text(margin = margin(l = 0), size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16)
  )
ggsave(
  filename = "\\\\KHU_Server2\\KDH\\최태림\\KNHANES,NHANES\\6. PUFA-Asthma\\Figure\\spline_n6_depression.png",
  width = 8, height = 10, dpi = 1000, bg = "transparent"
)

anova_result_knh <- anova(fit_knh)
print(anova_result_knh)

anova_result_nh <- anova(fit_nh)
print(anova_result_nh)

library(rms)
library(dplyr)
library(ggplot2)
library(readxl)
library(patchwork)

knh <- read_csv("\\\\KHU_Server2\\KDH\\최태림\\KNHANES,NHANES\\6. PUFA-Asthma\\knh.csv")
nh  <- read_csv("\\\\KHU_Server2\\KDH\\최태림\\KNHANES,NHANES\\6. PUFA-Asthma\\nh.csv")

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(total_pufa_mean, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh,
                    total_pufa_mean = seq(min(knh$total_pufa_mean, na.rm = TRUE),
                                          max(knh$total_pufa_mean, na.rm = TRUE),
                                          length.out = 100),
                    fun = identity,
                    conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$total_pufa_mean -
                                             quantile(knh$total_pufa_mean, 0.05, na.rm = TRUE)))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(total_pufa_mean, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh,
                   total_pufa_mean = seq(min(nh$total_pufa_mean, na.rm = TRUE),
                                         max(nh$total_pufa_mean, na.rm = TRUE),
                                         length.out = 100),
                   fun = identity,
                   conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$total_pufa_mean -
                                           min(nh$total_pufa_mean, na.rm = TRUE)))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = total_pufa_mean, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "Total PUFA intake (g/day)", y = "Odds Ratio") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))

ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_total.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")

anova_knh <- anova(fit_knh)
anova_nh  <- anova(fit_nh)
print(anova_knh)
print(anova_nh)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(n3_mean, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh,
                    n3_mean = seq(min(knh$n3_mean, na.rm = TRUE),
                                  max(knh$n3_mean, na.rm = TRUE),
                                  length.out = 100),
                    fun = identity,
                    conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$n3_mean -
                                             min(knh$n3_mean, na.rm = TRUE)))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(n3_mean, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh,
                   n3_mean = seq(min(nh$n3_mean, na.rm = TRUE),
                                 max(nh$n3_mean, na.rm = TRUE),
                                 length.out = 100),
                   fun = identity,
                   conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$n3_mean -
                                           min(nh$n3_mean, na.rm = TRUE)))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = n3_mean, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "n-3 PUFA intake (g/day)", y = "Odds Ratio") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))

ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_n6.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")

anova_knh <- anova(fit_knh)
anova_nh  <- anova(fit_nh)
print(anova_knh)
print(anova_nh)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(n6_mean, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh,
                    n6_mean = seq(min(knh$n6_mean, na.rm = TRUE),
                                  max(knh$n6_mean, na.rm = TRUE),
                                  length.out = 100),
                    fun = identity,
                    conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$n6_mean -
                                             min(knh$n6_mean, na.rm = TRUE)))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(n6_mean, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh,
                   n6_mean = seq(min(nh$n6_mean, na.rm = TRUE),
                                 max(nh$n6_mean, na.rm = TRUE),
                                 length.out = 100),
                   fun = identity,
                   conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$n6_mean -
                                           min(nh$n6_mean, na.rm = TRUE)))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = n6_mean, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "n-6 PUFA intake (g/day)", y = "Odds Ratio") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))

ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_n3.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")

anova_knh <- anova(fit_knh)
anova_nh  <- anova(fit_nh)
print(anova_knh)
print(anova_nh)

model_resid_knh <- lm(total_pufa_mean ~ energy_mean, data = knh)
knh$pufa_resid <- residuals(model_resid_knh) + mean(knh$total_pufa_mean, na.rm = TRUE)

model_resid_nh <- lm(total_pufa_mean ~ energy_mean, data = nh)
nh$pufa_resid <- residuals(model_resid_nh) + mean(nh$total_pufa_mean, na.rm = TRUE)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh, pufa_resid = seq(min(knh$pufa_resid, na.rm=TRUE),
                                              max(knh$pufa_resid, na.rm=TRUE),
                                              length.out = 300),
                    fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$pufa_resid - 0))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh, pufa_resid = seq(min(nh$pufa_resid, na.rm=TRUE),
                                            max(nh$pufa_resid, na.rm=TRUE),
                                            length.out = 300),
                   fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$pufa_resid - 0))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = pufa_resid, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Energy-adjusted PUFA intake", y = "Odds ratio") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))
ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_pufa_energy.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")

model_resid_knh <- lm(n3_mean ~ energy_mean, data = knh)
knh$pufa_resid <- residuals(model_resid_knh) + mean(knh$n3_mean, na.rm = TRUE)

model_resid_nh <- lm(n3_mean ~ energy_mean, data = nh)
nh$pufa_resid <- residuals(model_resid_nh) + mean(nh$n3_mean, na.rm = TRUE)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh, pufa_resid = seq(min(knh$pufa_resid, na.rm=TRUE),
                                              max(knh$pufa_resid, na.rm=TRUE),
                                              length.out = 300),
                    fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$pufa_resid - 0))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh, pufa_resid = seq(min(nh$pufa_resid, na.rm=TRUE),
                                            max(nh$pufa_resid, na.rm=TRUE),
                                            length.out = 300),
                   fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$pufa_resid - 0))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = pufa_resid, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Energy-adjusted n-3 PUFA intake", y = "") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))
ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_n3_energy.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")

model_resid_knh <- lm(n6_mean ~ energy_mean, data = knh)
knh$pufa_resid <- residuals(model_resid_knh) + mean(knh$n6_mean, na.rm = TRUE)

model_resid_nh <- lm(n6_mean ~ energy_mean, data = nh)
nh$pufa_resid <- residuals(model_resid_nh) + mean(nh$n6_mean, na.rm = TRUE)

dd <- datadist(knh); options(datadist = "dd")
fit_knh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + sex, data = knh)
pred_knh <- Predict(fit_knh, pufa_resid = seq(min(knh$pufa_resid, na.rm=TRUE),
                                              max(knh$pufa_resid, na.rm=TRUE),
                                              length.out = 300),
                    fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_knh <- pred_knh$yhat[which.min(abs(pred_knh$pufa_resid - 0))]
pred_knh_df <- pred_knh %>%
  mutate(OR = exp(yhat - ref_val_knh),
         lower_OR = exp(lower - ref_val_knh),
         upper_OR = exp(upper - ref_val_knh),
         Country = "KNHANES")

dd <- datadist(nh); options(datadist = "dd")
fit_nh <- lrm(depression ~ rcs(pufa_resid, 3) + age_g + RIAGENDR, data = nh)
pred_nh <- Predict(fit_nh, pufa_resid = seq(min(nh$pufa_resid, na.rm=TRUE),
                                            max(nh$pufa_resid, na.rm=TRUE),
                                            length.out = 300),
                   fun = identity, conf.int = 0.95) %>% as.data.frame()
ref_val_nh <- pred_nh$yhat[which.min(abs(pred_nh$pufa_resid - 0))]
pred_nh_df <- pred_nh %>%
  mutate(OR = exp(yhat - ref_val_nh),
         lower_OR = exp(lower - ref_val_nh),
         upper_OR = exp(upper - ref_val_nh),
         Country = "NHANES")

pred_all <- bind_rows(pred_knh_df, pred_nh_df)

ggplot(pred_all, aes(x = pufa_resid, y = OR, color = Country, fill = Country)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(aes(ymin = lower_OR, ymax = upper_OR), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_fill_manual(values = c("KNHANES" = "#007FFF", "NHANES" = "#FF6B6B")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Energy-adjusted n-6 PUFA intake", y = "") +
  theme_minimal(base_family = "serif", base_size = 16) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black", linewidth = 0.6))
ggsave("//KHU_Server2/KDH/최태림/KNHANES,NHANES/6. PUFA-Asthma/revision_n6_energy.png",
       width = 5, height = 8, dpi = 1000, bg = "transparent")
