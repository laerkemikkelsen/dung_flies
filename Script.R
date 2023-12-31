# Create colorpalette
library(RColorBrewer)
display.brewer.all() 
dungcol <- colorRampPalette(brewer.pal(8, name = "Set2"))(10)
dungcol2 <- colorRampPalette(brewer.pal(8, name = "Set1"))(10)

# Import the data into R
library(readr)
dungfly <- read.csv2("Data_final.csv")
dungfly <- dungfly[!is.na(dungfly$tibia_lenght),] #remove row that does not have 
# a number in tibia_length
dungfly <- dungfly[, !names(dungfly) %in% "male_number"] # remove the column 
# male_number, as we are not going to use it and it is included in the ID 
                                                  
# Check the classes
library(tibble)
as_tibble(dungfly)

# change the name of the time groups 
dungfly$time_group[dungfly$time_group =="A"] <- "5 min" 
dungfly$time_group[dungfly$time_group =="B"] <- "22 min"
dungfly$time_group[dungfly$time_group =="C"] <- "39 min"

# change the name of the paired and singles indicators 
dungfly$single_paired[dungfly$single_paired == "S"] <- "Single"
dungfly$single_paired[dungfly$single_paired == "P"] <- "Paired"

# change the levels of the time, so they show in the right order
dungfly$time_group=factor(dungfly$time_group, 
                          levels = c("5 min", "22 min", "39 min"))
levels(dungfly$time_group) # just to check if it was done right 

# Convert 'time_group' column to factors 
dungfly$time_group <- as.factor(dungfly$time_group)
# Convert 'single_paired' column to factors
dungfly$single_paired <- as.factor(dungfly$single_paired)
# Convert tibia_length' column to numeric
dungfly$tibia_lenght <- as.numeric(dungfly$tibia_lenght)
# COnvert 'dung_number' column to factor
dungfly$dung_number <- as.factor(dungfly$dung_number)


# Create a dataset that only contains the counts 
library(tidyverse)
# library(dplyr)
dungfly_counts <- dungfly %>%
  group_by(time_group) %>%
  summarise(total_males = n(), # column with the total per. time
            total_single_males = sum(single_paired =="Single"), # singles per time
            total_paired_males = sum(single_paired == "Paired"), # paired per time
            min_pr_dung = min())


# ggplot number of males per dung per time
library(ggplot2)

dungfly_counts_tot <- dungfly %>%
  group_by(time_group, dung_number,single_paired) %>%
  summarise(fly_count = n())

barplot1 <- ggplot(dungfly_counts_tot,
       aes(x = time_group, y = fly_count, fill = single_paired)) + 
  geom_boxplot() + 
  labs(x = "Time after dung dipostion", y = "Male flies pr. dung", 
       fill = "Mating status") +
  scale_fill_manual(values = c("#ABA300", "#00B8E7")) + 
theme(text = element_text(size = 14))
barplot1
ggsave("BarplotSingleMaleDensity.jpeg", width = 10, height = 7)


barplot2 <- ggplot(dungfly_counts_tot,
                   aes(x = time_group, y = fly_count, fill = time_group)) +
  geom_boxplot() + 
  guides(fill = F) + 
  labs(x = "Time after dung dipostion", y = "Male flies pr. dung") +
  scale_fill_manual(values = dungcol)
barplot2
ggsave("BarplotTotalDensity.jpeg", width = 10, height = 7)

 # ggplot tibia lenght all males 
 T_tibia_plot <- ggplot(dungfly,
       aes(x = time_group, y = tibia_lenght, fill = time_group)) +
  geom_boxplot() +
  labs(x = "Time after dung diposition", y = "Tibia lenght for all males (mm)") +
  scale_fill_manual(values = dungcol) + 
  guides(fill = F)
T_tibia_plot
ggsave("BarplotTotalTibialength.jpeg", width = 10, height = 7)


dungfly_tibia_tot <- dungfly %>%
  group_by(time_group, dung_number) %>%
  summarise(fly_count = n(),
            tibia_lenght = tibia_lenght)

bar_tibia_tot <- ggplot(dungfly_tibia_tot,
                  aes(x = time_group, y = tibia_lenght, fill = dung_number)) + 
  geom_boxplot() + 
  labs(x = "Time after dung dipostion", y = "Tibia length", fill = "Dung number") +
  scale_fill_manual(values = dungcol2) 
bar_tibia_tot
ggsave("barplot_tibialengthperdung.jpeg", width = 10, height = 5)

library(patchwork)
combined_plot1 <- T_tibia_plot + bar_tibia_tot + plot_annotation(tag_levels = 'A')
combined_plot1
ggsave("combinedplot_tibia.jpeg", plot = combined_plot1, width = 14, height = 7)

# ggplot tibia lenght single males 
single_males <- dungfly[dungfly$single_paired == "Single", ]

S_tibia_plot <- ggplot(single_males,
       aes(x = time_group, y = tibia_lenght, fill = time_group)) +
  geom_boxplot() +
  labs(x = "Time after diposition", y = "Tibia lenght for single males (mm)") +
  scale_fill_manual(values = dungcol) + 
  guides(fill = F)
S_tibia_plot

# ggplot tibia lenght paired males 
paired_males <- dungfly[dungfly$single_paired == "Paired", ]
P_tibia_plot <- ggplot(paired_males,
       aes(x = time_group, y = tibia_lenght, fill = time_group)) +
  geom_boxplot() +
  labs(x = "Time after diposition", y = "Tibia lenght for paired males (mm)") +
  scale_fill_manual(values = dungcol) + 
  guides(fill = F)
P_tibia_plot

# It all in one plot 
combined_plot2 <- P_tibia_plot + S_tibia_plot + plot_annotation(tag_levels = 'A')
combined_plot2
ggsave("combinedplot_single_paired.jpeg", plot = combined_plot2, width = 14, height = 7)


# Analysis 

# check if data is normal distributed - tibia lenght
hist(dungfly$tibia_lenght, 8)
hist(log(dungfly$tibia_lenght), 8)
qqnorm(log(dungfly$tibia_lenght))

# linear mixed effect model (LMM) 
library(lme4)
library(lmerTest)
# LMM model with interaction 
model1 <- lmer(log(tibia_lenght) ~ time_group * single_paired + (1|dung_number), 
               data = dungfly)
summary(model1)

# Visulization of LMM 

model1 %>%
  summary %>%
  coefficients() %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(
    lower = estimate - std_error * 1.94,
    upper = estimate + std_error * 1.94,
    #across(c(estimate, lower, upper), ~ model5@resp$family$linkinv(.x)), 
    p_value = format(summary(model1)$coefficients[, "Pr(>|t|)"], digits = 3),
    significant = ifelse(as.numeric(p_value) < 0.05, "*", "")
  ) %>%
  ggplot(aes(y = term, x = estimate, xmin = lower, xmax = upper)) +
  geom_col() +
  geom_pointrange() +
  geom_text(aes(label = paste0(p_value, significant)), hjust = c(-0.2, 1.2, 1.2, 1.2, -0.2, -0.2), vjust = 1.5) +
  theme(text = element_text(size = 14)) +
  labs(x = "Estimate", y = "Term") 
ggsave("model1.jpeg",width = 14, height = 7)


library(emmeans)
library(pbkrtest)
emm <- emmeans(model1, list(pairwise ~ time_group : single_paired), adjust = "tukey")
summary(emm)

# Check for poisson distribution - number of flies
hist(dungfly_counts_tot$fly_count, 100)

# number of flies 
model5 <- glmer(fly_count ~ time_group * single_paired + (1|dung_number), 
                data = dungfly_counts_tot, family = poisson)
summary(model5)

# visualization of GLMM 
model5 %>%
  summary %>%
  coefficients() %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(
    lower = estimate - std_error * 1.94,
    upper = estimate + std_error * 1.94,
    across(c(estimate, lower, upper), ~ model5@resp$family$linkinv(.x)),
    p_value = format(summary(model5)$coefficients[, "Pr(>|z|)"], digits = 3),
    significant = ifelse(as.numeric(p_value) < 0.05, "*", "")
  ) %>%
  ggplot(aes(y = term, x = estimate, xmin = lower, xmax = upper)) +
  geom_col() +
  geom_pointrange() +
  geom_text(aes(label = paste0(p_value, significant)), hjust = c(-0.2, 1.2, 1.2, 1.2, -0.2, -0.2), vjust = 1.5) +
  theme(text = element_text(size = 14)) +
  scale_x_log10() +
  labs(x = "log(estimate)", y = "Term")
ggsave("model5.jpeg",width = 14, height = 7)

library(emmeans)
emm2 <- emmeans(model5, list(pairwise ~ time_group : single_paired), adjust = "tukey")
summary(emm2)
plot(emm2)


overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model5) # no overdispersion 



