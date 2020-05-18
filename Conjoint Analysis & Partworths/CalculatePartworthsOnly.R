library(dplyr)
library(readxl)

henrys = read_xlsx('Henrys_Preferences.xlsx')

h_design_matrix = as.matrix.data.frame(henrys[, 5:9])

h_preference_matrix = as.matrix.data.frame(henrys[4])

henrys_model = lm(h_preference_matrix ~ h_design_matrix)

henrys_summary = summary(henrys_model)

henrys_coefficients = as.data.frame(henrys_summary$coefficients)

henrys_estimates = round(c(henrys_coefficients$Estimate), 2)

henrys_estimates

#####################################################################

chee = read_xlsx('CheeChee_Preferences.xlsx')

cc_design_matrix = as.matrix.data.frame(chee[, 5:9])

cc_preference_matrix = as.matrix.data.frame(chee[4])

cc_model = lm(cc_preference_matrix ~ cc_design_matrix)

cc_summary = summary(cc_model)

cc_coefficients = as.data.frame(cc_summary$coefficients)

cc_estimates = round(c(cc_coefficients$Estimate), 2)

cc_estimates

#####################################################################

mrj = read_xlsx('Mitesh_Preferences.xlsx')

mrj_design_matrix = as.matrix.data.frame(mrj[, 5:9])

mrj_preference_matrix = as.matrix.data.frame(mrj[4])

mrj_model = lm(mrj_preference_matrix ~ mrj_design_matrix)

mrj_summary = summary(mrj_model)

mrj_coefficients = as.data.frame(mrj_summary$coefficients)

mrj_estimates = round(c(mrj_coefficients$Estimate), 2)

mrj_estimates

######################################################################


## Importing the preferences and design matrix Excel file

# data_file <- read_xlsx('Mitesh_Preferences.xlsx')

data_file <- read_xlsx('Henrys_Preferences.xlsx')

# data_file <- read_xlsx('CheeChee_Preferences.xlsx')

## Creating design matrix input
design_matrix_input = as.matrix.data.frame(data_file[, 5:9])


## Creating preferences input
preferences_matrix = as.matrix.data.frame(data_file[4])

regression_model <- lm(preferences_matrix ~ design_matrix_input)       # regressing preferences on the design matrix

model_summary = summary(regression_model)

features_levels <- c("Screen 52 inch", 
                     "Screen 65 inch", 
                     "2D or 3D", 
                     "Sony = 1", 
                     "Price (low = 0; hi =1)")            # creating a vector of the various feature levels

partworths = data.frame(model_summary$coefficients[,1:3], row.names = c("Intercept", features_levels))           
# saving the parthworth estimates, se and tvals


attribute_names = c("Screen Size",
                    "Technology",
                    "Brand",
                    "Price")                                # creating a vector of the different attribute

Range = round(c(
  max(c(partworths$Estimate[2], partworths$Estimate[3], 0)) - min(c(partworths$Estimate[2],partworths$Estimate[3],0)),
  max(c(partworths$Estimate[4], 0)) - min(c(partworths$Estimate[4], 0)),
  max(c(partworths$Estimate[5], 0)) - min(c(partworths$Estimate[5], 0)),
  max(c(partworths$Estimate[6], 0)) - min(c(partworths$Estimate[6], 0))
), 2)

attribute_comparison = data.frame(Range,
                                  row.names = attribute_names
)

attribute_comparison$Attribute_Importance = round(((attribute_comparison$Range/sum(attribute_comparison$Range))*100), 2)
# calculating the Attribute Importance

price_savings = 2500-2000                                 # calculating the price savings
price_partworth = abs(partworths[6,1])                    # retrieving the absolute value of the partworth of Price attribute

one_unit_utility_cost = price_savings/price_partworth     # calculating the cost of 1 unit of utility

partworths$WillingnessToPay = round(partworths$Estimate*one_unit_utility_cost, 2)
# calculating the willingness to pay for a certain attribute level

intercept_cost = 1000                                     # Cost of a 2D 46 inch Sharp TV i.e. intercept
inch_52_screen = 500                                      # Cost of a 52 inch screen
inch_65_screen = 1000                                     # Cost of a 65 inch screen
tech_3D = 250                                             # Cost of using 3D Technology
brand_premium = 250                                       # Premium because of Brand Value

cost_features = cbind(intercept_cost, inch_52_screen, inch_65_screen, tech_3D, brand_premium)
# Combining the different costs into 1 vector for calculation  

my_design = c(1,0,1,0,0,1)                                # My design vector
Competing_Brand_1 = c(1,1,0,1,1,1)                        # Competing Brand 1's design vector
Competing_Brand_2 = c(1,0,1,1,0,0)                        # Competing Brand 2's design vector

design_vector_list = list(my_design, Competing_Brand_1, Competing_Brand_2)
# Saving the different design vectors in a list for calculations

design_comparison = data.frame(c("my_design", "Competing_Brand_1", "Competing_Brand_2"))
# creating a data frame to store various calculations

design_comparison = `colnames<-`(design_comparison, c("Designs"))
# renaming the column of the data frame

my_design_price <- if (my_design[6] == 1) 2500 else 2000

for (x in c(1:3)) {
  design_comparison$Utility[x] = sum(unlist(design_vector_list[x])*partworths$Estimate)
}                                                         # calculating and saving the Utilities of each design

design_comparison$Attractiveness = round(exp(design_comparison$Utility), 2)
# calculating and saving the attractiveness of each design

design_comparison$MarketShare = round(100*design_comparison$Attractiveness/sum(design_comparison$Attractiveness),4)
# calculating and saving the Market Share of each design based on the current design vector

my_design_cost = sum((my_design[-6])*cost_features)       # Cost of my design

price_list = seq(1500, 2500, by = 100)

utility_at_price = sum(my_design[1:5]*partworths$Estimate[1:5])+(partworths$Estimate[6]*(price_list-my_design_cost)/500)

attractiveness_at_price = exp(utility_at_price)

market_share_at_price = round(
  100*attractiveness_at_price/(sum(design_comparison$Attractiveness[2:3])+attractiveness_at_price), 3
)

margin_at_price = price_list-my_design_cost

profit_per_TV_at_price = round(margin_at_price*market_share_at_price/100, 2)

price_comparison = data.frame(price_list, market_share_at_price, margin_at_price, profit_per_TV_at_price)

plot(price_list, market_share_at_price,
     type = 'b',
     col = 'blue',
     xlab = "Price",
     ylab = "Market Share in %")
title(main = "Price vs Market Share")

plot(price_list, profit_per_TV_at_price,
     type = 'b',
     col = 'red',
     xlab = "Price",
     ylab = "Profit per TV"
     )
title(main = "Price vs Profit per TV")

library(ggplot2)

ggplot(data=price_comparison, aes(x=price_list, y=profit_per_TV_at_price, group = 1)) +
  xlab("Price") +
  ylab("Profit per TV") + 
  ggtitle("Price vs Profit per TV") +
  geom_line(colour="red", size=1.5) +
  geom_point(colour="red", size=4, shape=21, fill="white")

optimal_price = price_list[which.max(profit_per_TV_at_price)]

market_share_at_optimal_price = market_share_at_price[which.max(profit_per_TV_at_price)]

max_profit_per_tv_at_optimal_price = profit_per_TV_at_price[which.max(profit_per_TV_at_price)]
