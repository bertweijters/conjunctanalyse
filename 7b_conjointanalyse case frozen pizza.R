#marktonderzoek conjoint analysis: case frozen pizza
#(install conjoint package and) open conjoint library
install.packages("conjoint")
library(conjoint)

##############Step 1: Define attributes and attribute levels#################
#generate grid with all possible combinations of attribute levels (factor levels)
c<-expand.grid(
  Crust <-c("Pan", "Thin", "Thick"),
  Cheese <- c("Romano", "Mixed", "Mozzarella"),
  Amount <-c("60g", "180g"),
  Topping <-c("Hawai","Veggie","Sausage", "Pepperoni"),
  Price <-c("1.80EUR", "2.40EUR", "3.60EUR")
  )

#Assign names to the attributes
names(c) <-c("Crust", "Cheese", "Amount","Topping","Price")

###################Step 2: Create a design################
#Select an orthogonal design from c
mydesign <- caFactorialDesign(data=c, type="fractional")
#Randomize presentation order
mydesign<-mydesign[order(sample(1:16)),]
#add an empty rating column
mydesign$rating<-rep(0,16)
print(mydesign)

# Save design into an excel file
#(install and) open xlsx package
install.packages("xlsx")
library(xlsx)
#replace 'bweijter' with your own user name on your pc (or save mydesign to another location)
#write.xlsx(as.data.frame(mydesign), "C:/Users/bweijter/OneDrive - UGent/Desktop/mydesign.xlsx")
write.xlsx(as.data.frame(mydesign), "C:/Users/bertw/Desktop/mydesign.xlsx",row.names=FALSE)


#################Step 3: Collect data################################
# Now open 'mydesign.xlsx' which you just created on your desktop in Excel 
# Type in ratings for each product profile 
# Use scores from 1='not attractive at all' to 10 = 'very attractive'
# After rating the products in Excel, copy your ratings to the clipboard
# Select the ratings (include only your ratings, but not the header 'rating') and select copy or press CTRL-C
# Read the clipboard into a vector with your ratings
ratings<-as.vector(unlist((read.table("clipboard"))))
#create a dataset for analysis
mydata<-mydesign
#insert your ratings 
mydata$rating<-ratings

#Alternatively (e.g., if using the clipboard does not work), use read.xlsx
#First save the Excel file mydesign.xlsx
mydata<-read.xlsx(file="C:/Users/bertw/Desktop/mydesign.xlsx",sheetIndex=1)


#############Step 4: Estimate utilities#######################
#Run a regression analysis with your ratings as the dependent variable
#The attribute factors are the independent variables
myconjoint<-lm(ratings~Crust+Cheese+Amount+Topping+Price,data=mydata)

##############Step 5: Report / visualize##############
# output
summary(myconjoint)
myconjoint$coefficients
# Plot your part worth utilities
install.packages("jtools")
library(jtools)
install.packages("ggstance")
library(ggstance)
install.packages("broom.mixed")
library(broom.mixed)
plot_coefs(myconjoint)


