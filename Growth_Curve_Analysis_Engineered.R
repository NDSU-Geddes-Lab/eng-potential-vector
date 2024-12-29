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

setwd("~/Documents/Projects/Geddes/pipelines/riley-growth-curves/")

library(tidyverse)
library(readxl)

#input_file <- "Growth_Curve_Data_RW.xlsx"  # Blank removed
input_file <- "C:/Users/Riley/OneDrive/Desktop/241015_RW_GrowthCurveData.xlsx"  # Blank not removed

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

# Compute fold change for RFU and OD
rfu <- all_data %>%
  filter(measure == "rfu") %>%
  group_by(strain, antibiotic, treatment) %>%
  summarise(delta_rfu=log(t2/t1, 10)) # Change t2/t1 to compare different time points

od <- all_data %>%
  filter(measure == "od") %>%
  group_by(strain, antibiotic, treatment) %>%
  summarise(delta_od=t2/t1)

df <- full_join(od, rfu, by=c("strain","antibiotic","treatment"))

# Plotting
ggplot(data=df, aes(delta_rfu, delta_od, color=treatment)) +
  geom_point() +
  facet_wrap(~antibiotic)

# Subtract values for RFU and OD of control from engineered samples
df2 <- df %>%
  pivot_wider(names_from = treatment, values_from = c(delta_od, delta_rfu)) %>%
  group_by(strain, antibiotic) %>%
  summarise(od=(delta_od_eng - delta_od_ctrl),
            rfu=(delta_rfu_eng - delta_rfu_ctrl))

ggplot(data=df2, aes(rfu, od, color=antibiotic, label=strain)) + 
  geom_point() + geom_text() +
  facet_wrap(~antibiotic)

# Create separate data frames for OD and RFU data
od_all <- all_data %>% 
  filter(measure == "od") %>%
  select(-measure) %>%
  pivot_longer(cols = c("t1","t2","t3","t4","t5","t6"),
               names_to = "time", 
               names_prefix = "t", 
               names_transform = list(time = as.factor), 
               values_to = "od")

rfu_all <- all_data %>% 
  filter(measure == "rfu") %>%
  select(-measure) %>%
  pivot_longer(cols = c("t1","t2","t3","t4","t5","t6"),
               names_to = "time", 
               names_prefix = "t", 
               names_transform = list(time = as.factor), 
               values_to = "rfu")

# And plot some basic boxplots
ggplot(data=od_all, aes(time, od, color=treatment)) +
  geom_boxplot() +
  facet_wrap(~antibiotic)
  
ggplot(data=rfu_all, aes(time, log(rfu, 10), color=treatment)) +
  geom_boxplot()

# Convert data to long format
long_data <- all_data %>%
  pivot_longer(cols = c("t1","t2","t3","t4","t5","t6"),
               names_to = "time", 
               names_prefix = "t", 
               names_transform = list(time = as.factor)) %>%
  pivot_wider(names_from = "measure",
              values_from = "value")

# Super plot
ggplot(data=long_data, aes(log(rfu,10), od, color=treatment)) +
  geom_point() +
  facet_grid(time~antibiotic)

# Subtract treatment from control at t6
wide_data <- long_data %>%
  pivot_wider(names_from = "treatment",
              values_from = c("od","rfu")) %>% 
  mutate(delta_od = od_eng - od_ctrl,
         delta_rfu = log(rfu_eng,10) - log(rfu_ctrl,10))

eng_labels <- read_excel("C:/Users/Riley/OneDrive/Desktop/241021_Strain_Engineerability.xlsx")

wide_data_labeled <- wide_data %>%
  full_join(eng_labels, by="strain")

ggplot(data=wide_data_labeled, aes(delta_rfu, delta_od, colour=engineered)) +
  geom_point() +
  facet_grid(time~antibiotic)

# Now separately for each antibiotic
wide_data_gm50 <- wide_data_labeled %>% filter(antibiotic == "gm50")
install.packages("writexl")
library(writexl)
write_xlsx(wide_data_gm50, "C:/Users/Riley/OneDrive/Desktop/Gm50_Engineered.xlsx")

wide_data_nm50 <- wide_data_labeled %>% filter(antibiotic == "nm50")
write_xlsx(wide_data_gm50, "C:/Users/Riley/OneDrive/Desktop/Nm50_Engineered.xlsx")

wide_data_tc20 <- wide_data_labeled %>% filter(antibiotic == "tc20")
write_xlsx(wide_data_gm50, "C:/Users/Riley/OneDrive/Desktop/Tc20_Engineered.xlsx")

# Gm50
ggplot(data=wide_data_gm50, aes(delta_rfu, delta_od, colour=gm50)) +
  geom_point() +
  scale_x_continuous(
    labels = scales::label_math(expr = 10^.x),  # Convert x-axis to powers of 10
    name = "RFU (log scale, 10^x)"
  ) +
  facet_wrap(~time) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6)  # Adjust the size of x-axis labels
  )

# Nm50
ggplot(data=wide_data_nm50, aes(delta_rfu, delta_od, colour=nm50)) +
  geom_point() +
  scale_x_continuous(
    labels = scales::label_math(expr = 10^.x),  # Convert x-axis to powers of 10
    name = "RFU (log scale, 10^x)"
  ) +
  facet_wrap(~time) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6)  # Adjust the size of x-axis labels
  )

# Tc20
ggplot(data=wide_data_tc20, aes(delta_rfu, delta_od, colour=tc20)) +
  geom_point() +
  scale_x_continuous(
    labels = scales::label_math(expr = 10^.x),  # Convert x-axis to powers of 10
    name = "RFU (log scale, 10^x)"
  ) +
  facet_wrap(~time) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6)  # Adjust the size of x-axis labels
  )

