#
# Assignment #4 - Hospital Patient Data
# Andrew Burson
#

# --- Step 1: Make the Data ---
Frequency     <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
BloodPressure <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176)
FirstAssess   <- c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1)    # bad=1, good=0
SecondAssess  <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)    # low=0, high=1
FinalDecision <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1)    # low=0, high=1

df_hosp <- data.frame(
  Frequency, BloodPressure, FirstAssess,
  SecondAssess, FinalDecision,
  stringsAsFactors = FALSE
)

# --- Step 2: Clean the Data ---
# Check for missing values, then drop them
summary(df_hosp)
df_hosp <- na.omit(df_hosp)
summary(df_hosp)

# --- Step 3: Boxplots ---
# 3 plots to compare blood pressure by different assessments

boxplot(BloodPressure ~ FirstAssess, data = df_hosp,
        names = c("Good","Bad"),
        ylab = "Blood Pressure",
        main = "BP by First MD Assessment")

boxplot(BloodPressure ~ SecondAssess, data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Second MD Assessment")

boxplot(BloodPressure ~ FinalDecision, data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Final Decision")

# --- Step 4: Histograms ---
# 2 histograms to show overall distributions

hist(df_hosp$Frequency,
     breaks = seq(0,1,0.1),
     xlab = "Visit Frequency",
     main = "Histogram of Visit Frequency")

hist(df_hosp$BloodPressure,
     breaks = 8,
     xlab = "Blood Pressure",
     main = "Histogram of Blood Pressure")

# --- Step 5: Save Plots as PNGs ---
# Each plot will be saved in the working directory

png("boxplot_FirstAssess.png")
boxplot(BloodPressure ~ FirstAssess, data = df_hosp,
        names = c("Good","Bad"),
        ylab = "Blood Pressure",
        main = "BP by First MD Assessment")
dev.off()

png("boxplot_SecondAssess.png")
boxplot(BloodPressure ~ SecondAssess, data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Second MD Assessment")
dev.off()

png("boxplot_FinalDecision.png")
boxplot(BloodPressure ~ FinalDecision, data = df_hosp,
        names = c("Low","High"),
        ylab = "Blood Pressure",
        main = "BP by Final Decision")
dev.off()

png("hist_Frequency.png")
hist(df_hosp$Frequency,
     breaks = seq(0,1,0.1),
     xlab = "Visit Frequency",
     main = "Histogram of Visit Frequency")
dev.off()

png("hist_BloodPressure.png")
hist(df_hosp$BloodPressure,
     breaks = 8,
     xlab = "Blood Pressure",
     main = "Histogram of Blood Pressure")
dev.off()

# --- Step 6: Check that PNGs were created ---
list.files(pattern = "\\.png$")
