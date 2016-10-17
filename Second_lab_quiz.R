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



#correlations and histograms
library(apaTables)

#apa correlation tables_word
analytic_data
apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)
apa.cor.table(analytic_data_male, filename="Table_2_male.doc", table.number=2)
apa.cor.table(analytic_data_female, filename="Table_3_female.doc", table.number=3)

#create figures
psych::pairs.panels(as.data.frame(analytic_data) ,lm=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_male) ,lm=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_female) ,lm=TRUE)

#histograms

#load data #notnecessary?
analytic_data_female <- read_csv("analytic_data_female.csv")

#make histogram - female data, neuroticism
my.hist <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist <- my.hist + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist <- my.hist + labs(title="Neuroticism Distribution",x="Neuroticism",y="Frequency")
my.hist <- my.hist + coord_cartesian(xlim=c(0,30), ylim=c(0,1200))
my.hist <- my.hist + theme_classic()

my.hist <- my.hist + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

my.hist <- my.hist + scale_x_continuous( breaks = seq(0,25,by=5) )


print(my.hist)
ggsave(filename="Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist, width=6,heigh=6,units="in")


#make histogram - female data, neg_affect
my.hist <- ggplot(analytic_data_female,aes(neg_affect))
my.hist <- my.hist + geom_histogram(aes(y= ..count..), binwidth = .25, fill="black", color="black")
my.hist <- my.hist + labs(title="Negative affect",x="negative affect",y="frequency")
my.hist <- my.hist + coord_cartesian(xlim=c(0,5), ylim=c(0,1200))
my.hist <- my.hist + theme_classic()

my.hist <- my.hist + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


my.hist <- my.hist + scale_y_continuous( expand = c(0,0))

print(my.hist)
ggsave(filename="Figure_5_Neg_Affect_Histogram_Female.tiff")


#make scatterplot
my.plot <- qplot(neg_affect,Neuroticism,data=analytic_data_female)
print(my.plot)
#add apa elements
my.plot <- my.plot + theme_classic()
print(my.plot)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                             axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
print(my.plot)

my.plot <- qplot(x=neg_affect,y=Neuroticism,data=analytic_data_female)
my.plot <- my.plot + geom_smooth(method = "lm", se = FALSE, color="black")
print(my.plot)
