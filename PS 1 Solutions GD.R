# Date:			      Jan 31, 2024
# Last Updated:   
# Author:		    	Gaurav Doshi
# Description:		Solutions for Problem Set 1: ECON 4803/8803 Spring 2024
#	

Packages <- c("ggplot2", "dplyr", "stargazer")
lapply(Packages, library, character.only = TRUE)

###  1. Wind turbine data
wind_turbine_data <- read.csv("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Project/Project Data/Final/uswtdbCSV/uswtdb_v6_0_20230531.csv")
wind_turbine_main <- wind_turbine_data[complete.cases(wind_turbine_data[, c("eia_id", "p_year", "p_cap", "t_manu", "t_model", "t_cap", "t_hh", "t_rd", "xlong", "ylat")]),]
wind_turbine_main <- wind_turbine_main[which(wind_turbine_main$p_year >= 2001),]
wind_turbine_main <- wind_turbine_main[which(!wind_turbine_main$t_state %in% c("AK", "HI")),]

# convert turbine capacity in MWs
wind_turbine_main$t_cap <- wind_turbine_main$t_cap/1000

# 6. Summary stats  
stargazer(wind_turbine_main[, c("t_cap", "t_hh", "t_rd")])

wind_project_data <- wind_turbine_main %>% dplyr::group_by(eia_id) %>% dplyr::summarise(p_cap = mean(p_cap), t_cap = mean(t_cap), operating_year = first(p_year),
                                                                                        hub_ht = mean(t_hh), rotor_diam = mean(t_rd), t_manu = first(t_manu), turbines = n(),
                                                                                        t_model = first(t_model), state = first(t_state), county = first(t_county))
wind_project_data <- as.data.frame(wind_project_data)

# 8. summary stats for projects
stargazer(wind_project_data[, c("p_cap", "turbines", "hub_ht", "rotor_diam")])

# 9. evolution of hub height and rotor diameter
annual_data <- wind_project_data %>% dplyr::group_by(operating_year) %>% dplyr::summarise(hub_ht = mean(hub_ht), rotor_diam = mean(rotor_diam))
plot_1_9 <- ggplot() +
  geom_line(data = annual_data, aes(x = operating_year, y = hub_ht, group = 1), col="blue") +
  geom_line(data = annual_data, aes(x = operating_year, y = rotor_diam, group = 1), col="red") +
  labs(x="Operating Year", y="meters", title="Rotor Diameter (Red) and Hub Height (Blue)") +
  theme_bw()
plot_1_9
ggsave("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Problem Sets/PS1/Solution/plot1_9.pdf", plot =  plot_1_9, width = 8, height = 5, units = "in", dpi = 320)

# 10.
wind_data_state <- wind_project_data %>% dplyr::group_by(state) %>% dplyr::summarise(tot_cap = sum(p_cap), tot_turbines = sum(turbines))

# top 5 states with most wind turbines
summary1_10 <- wind_data_state[order(wind_data_state$tot_turbines, decreasing = TRUE ), c("state", "tot_turbines")][1:5,]

# top 5 states with highest wind capacity
summary1_10 <- as.data.frame(summary1_10)
summary1_10 <- summary1_10 %>% cbind(wind_data_state[order(wind_data_state$tot_cap, decreasing = TRUE ), c("state", "tot_cap")][1:5,])
# total capacity in the US
sum(wind_data_state$tot_cap)

# output
stargazer(summary1_10[1:5,], summary=FALSE, rownames=FALSE)

###  2. Wind ordinance data
wind_ordinance_data <- read.csv("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Project/Project Data/Final/wind_ordinance_main.csv")
wind_ordinance_data$X <- NULL

wind_ordinance_main <- wind_ordinance_data[which(wind_ordinance_data$ordinance_year >= 2001),]
ordinance_state <- wind_ordinance_main %>% dplyr::group_by(State) %>% dplyr::summarise(tot_ord = n())

# 3. top 3 states with most and least wind ordinances
ordinance_state$State[order(ordinance_state$tot_ord, decreasing = T)][1:3]
ordinance_state$State[order(ordinance_state$tot_ord, decreasing = F)][1:3]

# 4.
ordinance_annual <- wind_ordinance_main %>% dplyr::group_by(ordinance_year) %>% dplyr::summarise(tot_ord = n())
ordinance_annual$cummulative_ord <- cumsum(ordinance_annual$tot_ord)

plot_2_4 <- ggplot() +
  geom_line(data = ordinance_annual, aes(x = ordinance_year, y = tot_ord, group = 1), col="blue") +
  geom_line(data = ordinance_annual, aes(x = ordinance_year, y = cummulative_ord, group = 1), col="red") +
  labs(x="Year", y="Number of Ordinances", title="Total new ordinances (blue) and Cummulative ordinances (red)") +
  theme_bw()
plot_2_4
ggsave("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Problem Sets/PS1/Solution/plot_2_4.pdf", plot =  plot_2_4, width = 8, height = 5, units = "in", dpi = 320)


### 3. Wind Resource Quality data
wind_resource_data <- read.csv("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Project/Project Data/Final/wtk_site_metadata.csv")
wind_resource_main <- wind_resource_data[which(wind_resource_data$power_curve != "offshore"),]
wind_resource_main <- wind_resource_main[which(!wind_resource_main$State %in% c("Alaska", "Hawaii")),]

# 5. Summary Stats (note: new update in stargazer does not allow long variable names. thus using variable index)
# 9: wind speed, 10: capacity factor, 6: fraction of usable area
stargazer(wind_resource_main[, c(9, 10, 6)])

# 6.
# top 5 states with highest average wind speed
wind_resource_state <- wind_resource_main %>% dplyr::group_by(State) %>% dplyr::summarise(wind_speed = mean(wind_speed), cap_factor = mean(capacity_factor))
summary_2_6 <- wind_resource_state[order(wind_resource_state$wind_speed, decreasing = T), c("State", "wind_speed")][1:5,]

# highest capacity factors
summary_2_6 <- as.data.frame(summary_2_6)
summary_2_6 <- summary_2_6 %>% cbind(wind_resource_state[order(wind_resource_state$cap_factor, decreasing = T), c("State", "cap_factor")][1:5,])

stargazer(summary_2_6, summary = F, rownames = F)

# 7. Refer to the pdf

#### 4. Merging the datasets
states <- read.csv("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Project/Project Data/states.csv")
wind_ordinance_main <- merge(wind_ordinance_main, states, by.x = c("State"), by.y = c("State"), all.x = T)
# create an ordinance flag
wind_ordinance_main$ordinance <- 1 

# remove the "County" string from county variable in wind turbine data
wind_turbine_main$t_county <- gsub(' County', "", wind_turbine_main$t_county)

# merging the datasets
turbine_ordinance_merge <- merge(wind_turbine_main, wind_ordinance_main, by.x = c("t_state", "t_county"), by.y = c("Abbreviation", "County"), all.x=T)

# replace NAs in ordinance flag as 0 -- these counties don't have any ordinance
turbine_ordinance_merge$ordinance[is.na(turbine_ordinance_merge$ordinance)] <- 0
turbine_ordinance_merge$State <- NULL

turbine_ordinance_merge <- subset(turbine_ordinance_merge, select = -c(ordinance_year))
turbine_ordinance_county <- turbine_ordinance_merge %>% dplyr::group_by(t_state, t_county, p_year) %>% dplyr::summarise(tot_cap = sum(p_cap), avg_cap = mean(p_cap),
                                                                                                                        ordinance = mean(ordinance))
turbine_ordinance_county <- turbine_ordinance_county %>% dplyr::group_by(p_year, ordinance) %>% dplyr::summarise(avg_cap = mean(tot_cap))
turbine_ordinance_county$avg_cap <- turbine_ordinance_county$avg_cap/1000 # express in GW

# 4. graph of county level avg capacity by ordinance status
plot_4_4 <- ggplot() +
  geom_line(data = turbine_ordinance_county, aes(x = p_year, y = avg_cap, group = factor(ordinance), col = factor(ordinance))) +
  scale_color_manual(name="", values = c("red", "blue"), labels = c("No ordinance", "Ordinance")) +
  labs(x="Year", y="Capacity (GW)", title="Avg. wind capacity of counties by ordinance status") +
  theme_bw()
plot_4_4
ggsave("~/Dropbox/gd documents/Academics/Lectures/GA Tech/ECON 4803 ML for Econ/Problem Sets/PS1/Solution/plot_4_4.pdf", plot =  plot_4_4, width = 8, height = 5, units = "in", dpi = 320)

ordinance_projects <- turbine_ordinance_merge %>% dplyr::group_by(eia_id) %>% dplyr::summarise(state = first(t_state), county = first(t_county), operating_year = first(p_year),
                                                                                               capacity = mean(p_cap), tcap = sum(t_cap), ordinance = first(ordinance), 
                                                                                               avg_hh = mean(t_hh), avg_rotor_diam = mean(t_rd))
# 6. 
turbine_ordinance_county <- ordinance_projects %>% dplyr::group_by(state, county, ordinance) %>% dplyr::summarise(avg_cap = mean(capacity), avg_rotor_diam = mean(avg_rotor_diam))

ordinance_projects %>% dplyr::group_by(ordinance) %>% dplyr::summarise(avg_cap = mean(capacity), rotor_diam = mean(avg_rotor_diam))

# 7. refer to pdf
