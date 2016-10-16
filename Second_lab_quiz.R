library(tidyverse)

#load raw data
raw_data <- read_csv(file="raw_data.csv")
#View(raw_data)

str(raw_data)


#fix data - check for unusual values (e.g., -888,-999)
# n/a for this data


#tell R about categorical variables (sex)
categorical_variables <- select(raw_data, sex)
categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1,"Female"=2)
sex <- categorical_variables$sex
#str(categorical_variables)


#create scale items
neg_affect_items <- select (raw_data, afraid, angry, anxious, ashamed)
pos_affect_items <- select (raw_data, delighted, elated, enthusiastic, excited)
Neuroticism <- select (raw_data, Neuroticism)
Extraversion <- select (raw_data, Extraversion)


#check for out of range values
psych::describe(neg_affect_items)
is_bad_value <- neg_affect_items<0 | neg_affect_items>3
#View(neg_affect_items)
neg_affect_items[is_bad_value] <- NA

psych::describe(pos_affect_items) #no out of range values
psych::describe(Neuroticism) #no out of range values
psych::describe(Extraversion) #no out of range values


#obtaining scaled scores
neg_affect <- psych::alpha(as.data.frame(neg_affect_items) ,check.keys=FALSE)$scores
pos_affect <- psych::alpha(as.data.frame(pos_affect_items) ,check.keys=FALSE)$scores


#combine into analytic data
analytic_data <- cbind(categorical_variables,neg_affect, pos_affect, Neuroticism, Extraversion)
analytic_data
#save data
write_csv(analytic_data,path="analytic_data.csv")

#male and female subsets

#male
analytic_data.male <- filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data.male, pos_affect, neg_affect, Neuroticism, Extraversion)
#save
write_csv(analytic_data_male,path="analytic_data_male.csv")

#female
analytic_data.female <- filter(analytic_data, sex=="Female")
analytic_data_female <- select(analytic_data.female, pos_affect, neg_affect, Neuroticism, Extraversion)
#save
write_csv(analytic_data_male,path="analytic_data_female.csv")
