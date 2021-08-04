# Define research question:
# What affects the air quality ? and how?

# import the package which contains the dataset to be used
install.packages('Ecdat')
library(Ecdat)

# import data to be used from the library
data(Airq)

# check the variables
names(Airq)
# Variables: description
# Dependent variable:
# airq: air quality index (the lower the better)

# Independent variables:
# vala: enterprises' values in the cities (USD Mi)
# rain: rain quantity (inches)
# coas: coast (dummy)
# dens: population density (mile^2)
# medi: average income per capita (USD)


# Descriptive analysis:
summary(Airq)
# otherwise, visualize them
plot(airq~vala, data=Airq) 
# scatter plot: vala vs airq
# However, no significant test has been performed so far

# Creating a estatistical model
# y ~ X1 + X2 + X3 + ... , where y represents the dependent variable and Xs the independent ones.

# calling the linear model to be tested first
# assumption: linear data
# linear models accept the independent variables to be continous or categorical likewise the ones avaialable.
# However, the dependent variable has to be continous, a categorical dependent variable requires a GLM model.
m1<-lm(airq~vala,data=Airq)
# output:
summary(m1)
# vala: p-value >0.05, therefore not statistically significant
# Thus, enterprises' value does not influence city air quality

# Verify other variables: coas
m2<-lm(airq~coas, data=Airq)
summary(m2)
# coas is statistically significant, thus influences airq
# visualize
plot(airq~coas, data=Airq, xlab="Coast", ylab = "Air quality", col="lightblue",ylin=c(50,170), main="Air quality analysis", cex.lab=1.3)
# the boxplot shows that cities located closer to the coast present a better air quality

# Verify other variables: medi
m3<-lm(airq~medi,data=Airq)
summary(m3)
# medi is not statistically significant

# Verify other variables: rain
m4<-lm(airq~rain, data=Airq)
summary(m4)
# also not statistically significant

# Verify other variables: dens
m5<-lm(airq~dens,data=Airq)
summary(m5)
# Likewise, not statistically significant

# all previous relations could be visualized, for example model m3:
# y=a+b*x , where a is the intercept and b is its angle and x the independent variable
plot(airq~medi, data=Airq)
summary(m3)
curve(9.936e+01+ 5.638e-04*x, add=TRUE) # where the values are the nodel intercept and coefficient
# however, this model m3 is not statistically significant as verified previously.

# the visualization can be improved
plot(airq~medi, data=Airq, xlab='Average income per capita', ylab='Air quality', main="Average income 2010", pch=16, col="blue",cex=1.3,cex.lab=1.3)
curve(9.936e+01+ 5.638e-04*x, add=TRUE,col="darkblue",lwd=2,lty=2) 
# the curve could be generated automatically using
plot(airq~medi, data=Airq, xlab='Average income per capita', ylab='Air quality', main="Average income 2010", pch=16, col="blue",cex=1.3,cex.lab=1.3)
abline(m3)
# using ggplot could be an alternative



# Multiple regression: using the variables together
mRM1<-lm(airq~vala+coas,data=Airq)
summary(mRM1)
# Now, vala becomes stastically significant when analyzing it together with coas
# visualize
plot(airq~vala, data=Airq, xlab='Enterprise value (USD)', ylab='Air quality', cex.lab=1.3,col="blue")
# add the curves sequentially to the graph:
curve(1.171e+02+ 1.999e-03*x, add=T,col="darkblue",lwd=1.4)
# coas=0, thus cities not located in coast. This conclusion can be inferred via the summary as the coefficient is 
# coasyes, and the 'omitted' category is coasno
curve(1.171e+02+ 1.999e-03*x+-2.968e+01, lty=2, add=T,col="darkblue",lwd=1.4) # coasyes
legend("bottomright",c("Coast:No", "Coast:Yes"),pch=1,lty=c(1,2),bty="n",col=c("darkblue","darkblue"))
# Cities located far from the coast are negatively associated with poor air quality as well as entreprises' values


# Proceeding with the remaining variables
mRM2<-lm(airq~vala+coas+dens,data=Airq)
summary(mRM2)
# vala and coas are still statistically significant, but not dens
# note that the values of vala and coas modified with the introduction of dens, showing that the latter is affecting the former variables,
# this is why dens should not be immediately discarded from the model.

# To decide whether to remove or not a non-statiscally signficant it is advisable to do what is called "contrast"
# Thus, set a complete model (with the insignificant variable(s)), then set the model without the non-significant variable and finally 
# contrast them with a variance analysis which will indicate if there is significant difference between the candidates
# models by comparing the residuals:

# complete model
complete_model<-lm(airq~vala+coas+dens,data=Airq) # with dens
incomplete_model<-lm(airq~vala+coas,data=Airq) # without dens
# are the models the same?
# if p-value >0.05 then there's no significance and both models are equal, therefore there is no difference between them and dens would make no difference so could be discarded,
# and we keep with the simplest model because of parsimony.
# Otherwise, if p-value<0.05 then the models are significant different and dens should not be removed from the model

# variance analysis: 
anova(complete_model, incomplete_model)
# Given the p-value of 0.57, dens can be removed from the model