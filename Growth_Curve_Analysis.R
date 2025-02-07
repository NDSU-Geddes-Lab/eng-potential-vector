# Growth_Curve_Data_RW.xlsx
#
# Tabs:
#   1: Gm50 OD
#   2: Gm50 RFU
#   3: Nm50 OD
#   4: Nm50 RFU
#   5: Tc20 OD
#   6: Tc20 RFU
#
# Lines:
#   Engineered: A2:CQ8
#   Control: A11:CQ17
#
#   (ranges do not include donor)

setwd("")

library(tidyverse)
library(readxl)
library(ggforce)

#input_file <- "Growth_Curve_Data_RW.xlsx"  # Blank removed
input_file <- "241015_RW_GrowthCurveData.xlsx"  # Blank not removed

# Define variables
antibiotics <- c("Gm50","Nm50","Tc20")
measures <- c("OD","RFU")
treatments <- c("Eng","Ctrl")

# Create placeholder to store the full dataset
all_data <- NULL

# Loop through all variable combinations and read the corresponding data
# from the excel sheet.
for (a in antibiotics) {
  for (m in measures) {
    for (trt in treatments) {
      # Which sheet from the excel file to read
      sheet <- paste(a,m)
      
      # Data for engineered and control samples are in different ranges
      cells <- ""
      if (trt == "Eng") {
        cells <- "!A2:CQ8"
      } else {
        cells <- "!A11:CQ17"
      }
      
      # Read data
      dat <- read_excel(input_file, range=paste0(sheet,cells))
      
      # Transpose and remove row for time
      dat <- as.data.frame(t(dat)[-1,])
      
      # Add columns
      dat$strain <- rownames(dat)
      dat$antibiotic <- tolower(a)
      dat$treatment <- tolower(trt)
      dat$measure <- tolower(m)
      
      # Merge with the fill data
      all_data <- bind_rows(all_data, dat)
    }
  }
}

# Fix row/column names and rearrange columns
rownames(all_data) <- 1:nrow(all_data)
colnames(all_data) <- c("t1","t2","t3","t4","t5","t6",
                        "strain","antibiotic","treatment","measure")
all_data <- all_data[,c(7:10,1:6)]

# Convert data to long format
long_data <- all_data %>%
  pivot_longer(cols = c("t1","t2","t3","t4","t5","t6"),
               names_to = "time", 
               names_prefix = "t", 
               names_transform = list(time = as.factor)) %>%
  pivot_wider(names_from = "measure",
              values_from = "value")

# OD/RFU for control vs. engineered, by antibiotic and time
ggplot(data=long_data, aes(log(rfu,10), od, color=treatment)) +
  geom_point() +
  facet_grid(time~antibiotic)

# Subtract treatment from control at t6
wide_data <- long_data %>%
  pivot_wider(names_from = "treatment",
              values_from = c("od","rfu")) %>% 
  mutate(delta_od = od_eng - od_ctrl,
         delta_rfu = log(rfu_eng,10) - log(rfu_ctrl,10))

eng_labels <- read_excel("241021_Strain_Engineerability_with_class.xlsx")

wide_data_labeled <- wide_data %>%
  full_join(eng_labels, by="strain") %>%
  mutate(gm50_eng = (gm50 == "Yes")*1,
         nm50_eng = (nm50 == "Yes")*1,
         tc20_eng = (tc20 == "Yes")*1,
         eng = (engineered == "Yes")*1)

# OD/RFU by antibiotic and time, control-subtracted with ground truth labels
ggplot(data=wide_data_labeled, aes(delta_rfu, delta_od, colour=engineered)) +
  geom_point() +
  facet_grid(time~antibiotic)

# Now create a separate data framge for each antibiotic
wide_data_gm50 <- wide_data_labeled %>% filter(antibiotic == "gm50")
wide_data_nm50 <- wide_data_labeled %>% filter(antibiotic == "nm50")
wide_data_tc20 <- wide_data_labeled %>% filter(antibiotic == "tc20")

# Plot separation of eng vs. non-eng over time for each antibiotic

# All time points
# Gm50
ggplot(data=wide_data_gm50, aes(delta_rfu, delta_od, colour=gm50)) +
  geom_point() +
  facet_wrap(~time)

# Nm50
ggplot(data=wide_data_nm50, aes(delta_rfu, delta_od, colour=nm50)) +
  geom_point() +
  facet_wrap(~time)


# Tc20
ggplot(data=wide_data_tc20, aes(delta_rfu, delta_od, colour=tc20)) +
  geom_point() +
  facet_wrap(~time)


# time 6 only, with ellipses
# Gm50
ggplot(data=filter(wide_data_gm50, time==6), aes(delta_rfu, delta_od, colour=gm50)) +
  geom_point() + geom_mark_ellipse() + 
  xlab(expression(Delta ~ "RFU")) +
  ylab(expression(Delta ~ "OD")) +
  ggtitle("Gm50") + 
  guides(color = guide_legend(title = "Engineered")) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=12), 
        legend.text = element_text(size=12),
        plot.title = element_text(hjust=0.5))
  
# Nm50
ggplot(data=filter(wide_data_nm50, time==6), aes(delta_rfu, delta_od, colour=nm50)) +
  geom_point() + geom_mark_ellipse() + 
  xlab(expression(Delta ~ "RFU")) +
  ylab(expression(Delta ~ "OD")) +
  ggtitle("Nm50") + 
  guides(color = guide_legend(title = "Engineered")) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=12), 
        legend.text = element_text(size=12),
        plot.title = element_text(hjust=0.5))

# Tc20
ggplot(data=filter(wide_data_tc20, time==6), aes(delta_rfu, delta_od, colour=tc20)) +
  geom_point() + geom_mark_ellipse() + 
  xlab(expression(Delta ~ "RFU")) +
  ylab(expression(Delta ~ "OD")) +
  ggtitle("Tc20") + 
  guides(color = guide_legend(title = "Engineered")) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=12), 
        legend.text = element_text(size=12),
        plot.title = element_text(hjust=0.5))


# Statistical modeling with logistic regression
# Modeling on deltaOD and deltaRFU
# Gm50
mod_gm50 <- glm(gm50_eng ~ delta_od + delta_rfu, 
                family = "binomial", 
                data = filter(wide_data_gm50, time == 6))
summary(mod_gm50)
pchisq(mod_gm50$deviance, mod_gm50$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_gm50, main="Gm50")
par(mfrow=c(1,1))

# Nm50
mod_nm50 <- glm(nm50_eng ~ delta_od + delta_rfu, 
                family = "binomial", 
                data = filter(wide_data_nm50, time == 6))
summary(mod_nm50)
pchisq(mod_nm50$deviance, mod_nm50$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_nm50, main="Nm50")
par(mfrow=c(1,1))

# Tc20
mod_tc20 <- glm(tc20_eng ~ delta_od + delta_rfu, 
                family = "binomial", 
                data = filter(wide_data_tc20, time == 6))
summary(mod_tc20)
pchisq(mod_tc20$deviance, mod_tc20$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_tc20, main="Tc20")
par(mfrow=c(1,1))

# Modeling based on taxonomic class
# Gm50
mod_gm50_class <- glm(gm50_eng ~ 0 + class, 
                family = "binomial", 
                data = filter(wide_data_gm50, time == 6))
summary(mod_gm50_class)
pchisq(mod_gm50_class$deviance, mod_gm50_class$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_gm50_class, main="Gm50")
par(mfrow=c(1,1))

# Nm50
mod_nm50_class <- glm(nm50_eng ~ 0 + class, 
                family = "binomial", 
                data = filter(wide_data_nm50, time == 6))
summary(mod_nm50_class)
pchisq(mod_nm50_class$deviance, mod_nm50_class$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_nm50_class, main="Nm50")
par(mfrow=c(1,1))

# Tc20
mod_tc20_class <- glm(tc20_eng ~ 0 + class, 
                family = "binomial", 
                data = filter(wide_data_tc20, time == 6))
summary(mod_tc20_class)
pchisq(mod_tc20_class$deviance, mod_tc20_class$df.residual, lower.tail = F)
# Plot residuals (2x2 grid)
par(mfrow=c(2,2))
plot(mod_tc20_class, main="Tc20")
par(mfrow=c(1,1))
