#---
#title: "Final"
#---

# Import Packages
Packages <- c("ggplot2", "dplyr", "stargazer", "randomForest", "nnet", "caret")
lapply(Packages, library, character.only = TRUE)

# Data import
wind_turbine_data <- read.csv("uswtdb_v6_0_20230531.csv")
wind_ordinance_data <- read.csv("wind_ordinance_main.csv")
wind_resource_data <- read.csv("wtk_site_metadata.csv")
states <- read.csv("states.csv")
county_complete <- read.csv("county_complete.csv")

# clean and group wind_resource_data
wind_resource_data <- wind_resource_data[wind_resource_data$County != "Unknown", ]
wind_resource_data <- wind_resource_data[wind_resource_data$State != "Unknown", ]
wind_resource_data <- wind_resource_data %>%
  group_by(State, County) %>%
  summarise(across(c(fraction_of_usable_area, capacity, wind_speed, capacity_factor), mean), .groups = 'drop')

# Clean wind turbine data
wind_turbine_main <- wind_turbine_data[complete.cases(wind_turbine_data[, c("eia_id", "p_year", "p_cap", "t_manu", "t_model", "t_cap", "t_hh", "t_rd", "xlong", "ylat")]),]
wind_turbine_main <- wind_turbine_main[which(wind_turbine_main$p_year >= 2001),]
wind_turbine_main <- wind_turbine_main[which(!wind_turbine_main$t_state %in% c("AK", "HI")),]
wind_turbine_main$t_cap <- wind_turbine_main$t_cap/1000
wind_turbine_main$t_county <- gsub(' County', "", wind_turbine_main$t_county)

# This is how to get the mode in factoral data
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Data inspection 
wind_project_data <- wind_turbine_main %>% dplyr::group_by(eia_id) %>% 
  dplyr::summarise(p_cap_avg = mean(p_cap),
                   t_cap_avg = mean(t_cap),
                   operating_year = first(p_year),
                   hub_ht = mean(t_hh),
                   rotor_diam = mean(t_rd),
                   t_manu = calculate_mode(t_manu),
                    turbines = n(), 
                   t_model = calculate_mode(t_model),
                   state = first(t_state), 
                    county = first(t_county), 
                    t_rsa = mean(t_rsa), 
                    t_ttlh  = mean(t_ttlh))

# Wind ordinance data cleaning
wind_ordinance_data$X <- NULL
wind_ordinance_main <- wind_ordinance_data[which(wind_ordinance_data$ordinance_year >= 2001),]
ordinance_State <- wind_ordinance_main %>% dplyr::group_by(State) %>% dplyr::summarise(tot_ord = n())
wind_ordinance_main$ordinance <- 1 
wind_resource_data <- wind_resource_data[which(!wind_resource_data$State %in% c("Alaska", "Hawaii")),]

#County get Area
Area <- select(county_complete, state, name, area_2010)
Area$name <- gsub(' County', "", Area$name)

#Merging the datasets
wind_ordinance_main <- merge(wind_ordinance_main, states, by.x = c("State"), by.y = c("State"), all.x = T)
Area_Final <- merge(Area, states, by.x = "state", by.y = "State", all.x = T)
wind_resource_Final <- merge(wind_resource_data, states, by.x = c("State"), by.y = c("State"), all.x = T)
turbine_ordinance_merge <- merge(wind_turbine_main, wind_ordinance_main, by.x = c("t_state", "t_county"), by.y = c("Abbreviation", "County"), all.x=T)

#remove unused data
turbine_ordinance_merge$ordinance[is.na(turbine_ordinance_merge$ordinance)] <- 0
turbine_ordinance_merge$State <- NULL
turbine_ordinance_merge <- subset(turbine_ordinance_merge, select = -c(ordinance_year))
turbine_ordinance_merge <- subset(turbine_ordinance_merge, select = -c(case_id,faa_ors,faa_asn))
turbine_ordinance_merge <- subset(turbine_ordinance_merge, select = -c(usgs_pr_id,eia_id, t_fips))
turbine_ordinance_merge <- subset(turbine_ordinance_merge, select = -c(p_name,retrofit, retrofit_year,t_img_date,t_img_srce,xlong,ylat))

#group by State
turbine_ordinance_county <- turbine_ordinance_merge %>% 
  dplyr::group_by(t_state, t_county) %>% 
  dplyr::summarise(tot_cap = sum(t_cap), avg_p_cap = mean(p_cap),
                   avg_hh = mean(t_hh),avg_rd = mean(t_rd),
                   ordinance = mean(ordinance),
                   t_manu = calculate_mode(t_manu),
                   turbines = n(),
                                                                                                                t_model = calculate_mode(t_model),t_rsa = mean(t_rsa),t_ttlh  = mean(t_ttlh),
                                                                                                                avg_t_cap = mean(t_cap))
# More merging
# Remove the "State" column from wind_resource_Final
wind_resource_Final <- wind_resource_Final[, !(names(wind_resource_Final) %in% "State")]

# Merge the datasets using Abbreviation and t_state
merged_data <- merge(turbine_ordinance_county, wind_resource_Final, by.x = c("t_state", "t_county"), by.y = c("Abbreviation", "County"))
Area_Final <- Area_Final[, !(names(Area_Final) %in% "state")]
merged_data <- merge(merged_data, Area_Final, by.x = c("t_state", "t_county"), by.y = c("Abbreviation", "name"))

# remove avg_t_cap due to collinearity
merged_data <- merged_data %>% select(-avg_t_cap)

#Random Forest- Cross-Validation
index <- createDataPartition(y = merged_data$tot_cap, p = 0.8, list = FALSE)
training_data <- merged_data[index, ]
test_data <- merged_data[-index, ]
best_model <- NULL
best_mse <- Inf

#Loop--Cross-Validation

for (i in 1:1) {
  predictors <- training_data[, !names(training_data) %in% c("tot_cap")]
  response <- training_data$tot_cap
  
  ctrl <- trainControl(method = "cv", number = 5, repeats = 3)
  
  model <- train(x = predictors, y = response, method = "rf", trControl = ctrl,
                 tuneGrid = expand.grid(mtry = c(2,4,6,8,10)))
  
  predictions <- predict(model, newdata = test_data)
  
  mse <- mean((predictions - test_data$tot_cap)^2)
  
  if (mse < best_mse) {
    best_model <- model
    best_mse <- mse
  }
  cat("Iteration", i, "- MSE:", mse, "\n")
}
# Results--Cross-Validation
print(best_model)
print(best_model$results)
varImp(best_model, scale = TRUE)
print(paste("Best Mean Squared Error:", best_mse))

# Make prediction_table
options(scipen = 999)
predictions <- predict(best_model, newdata = test_data)
mse <- mean((predictions - test_data$tot_cap)^2)
print(paste("Mean Squared Error (MSE):", mse))
se = (predictions - test_data$tot_cap)^2
prediction_table <- data.frame(Real = test_data$tot_cap, Predicted = predictions, SE = se)
print(prediction_table)

# Modeling for effect of ordinance
# Set up for treatment effect models
index <- createDataPartition(y = merged_data$tot_cap, p = 0.8, list = FALSE)
training_data <- merged_data[index, ]
test_data <- merged_data[-index, ]

control <- merged_data[merged_data$ordinance == 0,]
treatment <- merged_data[merged_data$ordinance == 1,]

# Control Model
training_data <- control
predictors <- training_data[, !names(training_data) %in% c("tot_cap")]
response <- training_data$tot_cap

ctrl <- trainControl(method = "cv", number = 5, repeats = 3)

model_contral <- train(x = predictors, y = response, method = "rf", trControl = ctrl,
                       tuneGrid = expand.grid(mtry = c(10)))

# Treatment Model
# Split data into training and testing data sets
training_data <- treatment

predictors <- training_data[, !names(training_data) %in% c("tot_cap")]
response <- training_data$tot_cap

ctrl <- trainControl(method = "cv", number = 5, repeats = 3)

model_tretment <- train(x = predictors, y = response, method = "rf",
                        trControl = ctrl, tuneGrid = expand.grid(mtry = c(10)))

#Two model Treatment Effect
control$cf_with_treatment <- predict(model_tretment , newdata = control)
treatment$cf_with_treatment <- NA

treatment$cf_no_treatment <- predict(model_contral , newdata = treatment)
control$cf_no_treatment <- NA

treatment$treatment_effect <- treatment$tot_cap - treatment$cf_no_treatment
control$treatment_effect <- control$cf_with_treatment - control$tot_cap

control$cf_no_treatment <- NA
treatment$cf_with_treatment <- NA

treatment <- treatment[colnames(control)]

all_data <- rbind(treatment, control)
ate <- mean(all_data$treatment_effect, na.rm = TRUE)

treatment_summary <- all_data %>%
  group_by(ordinance ) %>%
  summarise(
    mean_treatment_effect = mean(treatment_effect, na.rm = TRUE),
    sd_treatment_effect = sd(treatment_effect, na.rm = TRUE),
    .groups = 'drop')

# The Plot
ggplot(treatment_summary, aes(x = ordinance, y = mean_treatment_effect, fill = ordinance)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_treatment_effect - sd_treatment_effect, ymax = mean_treatment_effect + sd_treatment_effect),
                width = 0.25, position = position_dodge(0.9)) +
  labs(title = "Treatment Effect by Treatment Status",
       x = "Treatment Status",
       y = "Mean Treatment Effect") +
  theme_minimal()

#Single Model treatment Effect
training_data <- merged_data

predictors <- training_data[, !names(training_data) %in% c("tot_cap")]
response <- training_data$tot_cap

ctrl <- trainControl(method = "cv", number = 5, repeats = 3)

model <- train(x = predictors, y = response, method = "rf",
               trControl = ctrl, tuneGrid = expand.grid(mtry = c(6)))

# Summary Stats
summary(model$results)
options(scipen = 999)
predictions <- predict(model, newdata = training_data)
mse <- mean((predictions - training_data$tot_cap)^2)
print(paste("Mean Squared Error (MSE):", mse))
se = (predictions - training_data$tot_cap)^2
prediction_table <- data.frame(Real = training_data$tot_cap, Predicted = predictions, SE = se)
print(prediction_table)
varImp(model)

# Opposite predictions from one Model
merged_data <- data.frame(merged_data)
cf_df <- merged_data
cf_df <- data.frame(cf_df)
cf_df$ordinances <- ifelse(cf_df$ordinance == 1, 0, 1)

merged_data$cf_predictions <- predict(model, newdata = cf_df)

merged_data$treatment_effect <- ifelse(merged_data$ordinance == 1,
                                       merged_data$tot_cap - merged_data$cf_predictions,
                                       merged_data$tot_cap - merged_data$tot_cap)

merged_data$treatment_effect <- ifelse(merged_data$ordinance == 1,
                                       merged_data$tot_cap- merged_data$cf_predictions,
                                       merged_data$cf_predictions - merged_data$tot_cap)

#Scatter Plot Treatment Effect
ggplot(merged_data, aes(x = tot_cap, y = cf_predictions, color = factor(ordinance))) +
  geom_point(alpha = 0.6) +
  geom_line(aes(x = tot_cap, y = tot_cap), colour = "red", linetype = "dashed") +
  labs(title = "Actual vs. Counterfactual Total Capacity Predictions", x = "Actual Total Capacity Output",
       y = "Counterfactual Predicted Total Capacity Output") +
  scale_color_manual(values = c("blue", "green"), labels = c("No Ordinance", "With Ordinance")) +
  theme_minimal()

# Calculate median of treatment effect
medians <- aggregate(treatment_effect ~ ordinance, data = merged_data, FUN = median)
print(medians)

# Box plot view Box
ggplot(merged_data, aes(x = factor(ordinance), y = treatment_effect, fill = factor(ordinance))) +
  geom_boxplot() +
  labs(title = "Distribution of Treatment Effects", x = "Group", y = "Treatment Effect") +
  scale_fill_manual(values = c("blue", "green"), labels = c("No Ordinance", "With Ordinance")) +
  theme_minimal() + ylim(-30, 30)

# Box plot different View Box
ggplot(merged_data, aes(x = factor(ordinance), y = treatment_effect, fill = factor(ordinance))) +
  geom_boxplot() +
  labs(title = "Distribution of Treatment Effects", x = "Group", y = "Treatment Effect") +
  scale_fill_manual(values = c("blue", "green"), labels = c("No Ordinance", "With Ordinance")) +
  theme_minimal() + ylim(-200, 200)

# Scatter Plot with lines
plot <- ggplot(merged_data, aes(x = tot_cap, y = cf_predictions, color = factor(ordinance))) +
  geom_point(alpha = 0.6) +  
  geom_line(aes(x = tot_cap, y = tot_cap), colour = "red", linetype = "dashed") + 
  geom_smooth(data = subset(merged_data, tot_cap >= 1000), method = "lm", se = FALSE, aes(color = factor(ordinance))) +
  geom_smooth(data = subset(merged_data, tot_cap < 1000), method = "lm", se = FALSE, aes(color = factor(ordinance))) +
  labs(title = "Actual vs. Counterfactual Total Capacity Predictions", x = "Actual Total Capacity Output",
       y = "Counterfactual Predicted Total Capacity Output") +
  scale_color_manual(values = c("black", "purple"), labels = c("No Ordinance", "With Ordinance")) +
  theme_minimal()

print(plot)

#Summary Stats for plot

# Fit linear model for data where tot_cap >= 1000
lm_above_1000_no_ordinance <- lm(cf_predictions ~ tot_cap, data = subset(merged_data, tot_cap >= 1000 & ordinance == 0))
lm_above_1000_with_ordinance <- lm(cf_predictions ~ tot_cap, data = subset(merged_data, tot_cap >= 1000 & ordinance == 1))

# Fit linear model for data where tot_cap < 1000
lm_below_1000_no_ordinance <- lm(cf_predictions ~ tot_cap, data = subset(merged_data, tot_cap < 1000 & ordinance == 0))
lm_below_1000_with_ordinance <- lm(cf_predictions ~ tot_cap, data = subset(merged_data, tot_cap < 1000 & ordinance == 1))

# Summarize and extract coefficients
summary(lm_above_1000_no_ordinance)
summary(lm_above_1000_with_ordinance)
summary(lm_below_1000_no_ordinance)
summary(lm_below_1000_with_ordinance)