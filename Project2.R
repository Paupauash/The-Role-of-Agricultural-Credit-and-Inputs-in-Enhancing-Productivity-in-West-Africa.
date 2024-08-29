##Project 2: The Role of Agricultural Credit and Inputs in Enhancing Crop Productivity in West Africa.

#Importing the databases
#Agriculture_credit database
library(readxl)
Agri_credit <- read_excel("C:/Users/Hp/Desktop/Agroeconomie/2010_2022_credit to agriculture_west_africa_FAOSTAT_data_en_8-23-2024.xls")

#Fertilizers_database
Fertil <-  read_excel("C:/Users/Hp/Desktop/Agroeconomie/Fertilizer_Agri_use_West africa_FAOSTAT_data_en_8-23-2024.xls")


#Crop productivity _database
Crop_productivity <- read_excel("C:/Users/Hp/Desktop/Agroeconomie/Crop_production_2010_2022_West_Africa_FAOSTAT_data_en_8-24-2024.xls")

#Extracting the data needed
library(dplyr)
Agri_credit1 <- Agri_credit[,c(2,4,10,11,12)]%>%                  #hold the data 
  group_by(Domain,Year,Unit) %>%
  summarize(Total_credit= sum(Value)) 

Fertil1 <- Fertil[, c(2,4,8,10,11,12)] %>% 
  group_by(Domain,Year,Unit) %>%
  summarize(Total_fertilizer= sum(Value)) 

Crop_productivity1 <- Crop_productivity %>% 
  filter(Unit=="t" & Area=="Western Africa")%>% 
  group_by(Domain,Area, Element, Year,Unit)%>% 
  summarize(Total_production= sum(Value)) 

#Joining the data together
joined_data2 <- Agri_credit1 %>%
  left_join(Fertil1, by = "Year")%>%
  left_join(Crop_productivity1 , by = "Year")

write.csv(joined_data2, "C:/Users/Hp/Desktop/Agroeconomie/Project2_data.csv") #Exporting the data


#Checking co linearity  between Agricultural credit and Fertilizer quantity
cor(Agri_credit1$Total_credit, Fertil1$Total_fertilizer)
#coefficient cor= -0.01668167. 0.02 is considerably less than 1.

plot(x = Agri_credit1$Total_credit, 
     y = Fertil1$Total_fertilizer,
     xlab = "Agricultural Credit (in million of US$)",
     ylab = "Fertilizer use (in tons)",
     main = "Agricultural Credit vs Fertilizer Use",
     pch = 16,       # Solid circle for data points
     font.axis = 2,  # Bold font for axis labels
     font.lab = 2)   # Bold font for axis titles
box(lwd = 2) # Add a thicker border around the plot

# Add a smoothed line
lines(lowess(Agri_credit1$Total_credit, Fertil1$Total_fertilizer), col = "#FF0000", lwd = 2)

#Also, from the plot we can clearly see that there is no linear relationship between the two variables,
# then we can use them as both together as explicative variables.

#Performing the linear regression model
model_multiple_regression <- lm(log(Total_production) ~ log(Total_credit) + log(Total_fertilizer), data = joined_data2)
summary(model_multiple_regression)


#The overall performance of the model
#Rsquare = 0.87 the model performs really well 

