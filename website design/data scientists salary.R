getwd()
setwd("D:\\ERICK\\Kinuthia R")
if(!require(readr))install.packages("readr")
library(readr)
dss <- read_csv("data_science_salaries.csv")
View(dss)

#summary data
summary(dss)
str(dss)
#categorising job title
table(dss$job_title)
dss$jobttcat=NA
dss$jobttcat[dss$job_title!="Data Scientist"]=0
dss$jobttcat[dss$job_title=="Data Scientist"]=1
dss$jobttcat=factor(dss$jobttcat,levels = c(0,1) ,labels  =c("Others","Data scientist"))


table(dss$jobttcat)
#categorise experience level
table(dss$experience_level)
dss$experience_levelcat=NA
dss$experience_levelcat[dss$experience_level=="Entry-level"|
                          dss$experience_level=="Mid-level"]=0
dss$experience_levelcat[dss$experience_level=="Executive-level"|
                          dss$experience_level=="Senior-level"]=1
dss$experience_levelcat=factor(dss$experience_levelcat,levels = c(0,1),labels = c("learner","expert"))
table(dss$experience_levelcat)




#categorise employment type
table(dss$employment_type)
dss$employment_typecat=NA
dss$employment_typecat[dss$employment_type!="Full-time"]=0
dss$employment_typecat[dss$employment_type=="Full-time"]=1
dss$employment_typecat=factor(dss$employment_typecat,levels = c(0,1), labels = c("part-time","Full-time"))
table(dss$employment_typecat)


#employee residence category
table(dss$employee_residence)
dss$employee_residencecat=NA
dss$employee_residencecat=dss$employee_residence
dss$employee_residencecat[dss$employee_residence==" China"|
                            dss$employee_residence=="Thailand"|
                            dss$employee_residence=="Vietnam"|
                            dss$employee_residence=="Qatar "|
                            dss$employee_residence=="Saudi Arabia"|
                            dss$employee_residence=="India"|
                            dss$employee_residence=="Iraq"|
                            dss$employee_residence=="Iran"|
                            dss$employee_residence=="Japan"|
                            dss$employee_residence=="Hong Kong "|
                            dss$employee_residence=="Indonesia"|
                            dss$employee_residence=="South Korea"|
                            dss$employee_residence=="Pakistan "|
                            dss$employee_residence=="Philippines"|
                            dss$employee_residence=="Algeria"|
                            dss$employee_residence=="Central African Republic"|                   dss$employee_residence=="Egypt"|
                            dss$employee_residence=="Ghana"|
                            dss$employee_residence=="Kenya"|
                            dss$employee_residence=="Nigeria"|
                            dss$employee_residence=="Singapore"|
                            dss$employee_residence=="South Africa"|
                            dss$employee_residence=="Uganda"|
                            dss$employee_residence=="Ukraine"|
                            dss$employee_residence=="Tunisia"|
                            dss$employee_residence=="Uzbekistan"|
                            dss$employee_residence=="Cyprus"]=0

dss$employee_residencecat[dss$employee_residencecat!=0]=1
dss$employee_residencecat=factor(dss$employee_residencecat,levels = c(0,1),labels=c("East","West"))


table(dss$employee_residencecat)

#creating category for salary currency
table(dss$salary_currency)
dss$salary_currencycat=NA
dss$salary_currencycat[dss$salary_currency!="USD"]=0
dss$salary_currencycat[dss$salary_currency=="USD"]=1
dss$salary_currencycat=factor(dss$salary_currencycat,levels=c(0,1),labels=c("Other currencies","USD"))
table(dss$salary_currencycat)

#creating category for salary in USD
table(dss$salary_in_usd)
dss$salary_in_usdcat=NA
dss$salary_in_usdcat[dss$salary_in_usd<=60000]=0
dss$salary_in_usdcat[dss$salary_in_usd>60000]=1
dss$salary_in_usdcat=factor(dss$salary_in_usdcat,levels=c(0,1),labels=c("Below USD 60,000","Above USD 60,000"))
table(dss$salary_in_usdcat)

#creating company location category
table(dss$company_location)
dss$company_locationcat=NA
dss$company_locationcat=dss$company_location
dss$company_locationcat[dss$company_location=="Algeria"|
                        dss$company_location=="Canada Central African Republic"|              dss$company_location=="China"|
                        dss$company_location=="Egypt"|
                        dss$company_location=="Ghana"|
                        dss$company_location=="Hong Kong"|
                        dss$company_location=="India"|
                        dss$company_location=="Indonesia"|
                        dss$company_location=="Iraq"|
                        dss$company_location=="Japan"|
                        dss$company_location=="Kenya"|
                        dss$company_location=="Nigeria"|
                        dss$company_location=="Qatar"|  
                        dss$company_location=="Singapore"| 
                        dss$company_location=="Saudi Arabia"| 
                        dss$company_location=="Thailand"| 
                        dss$company_location=="Vietnam"| 
                        dss$company_location=="South Africa"| 
                        dss$company_location=="South Korea"| 
                        dss$company_location=="Ukraine"|
                        dss$company_location=="United Arab Emirates" |
                        dss$company_location=="Pakistan"  |
                        dss$company_location=="Philippines" ]=0
dss$company_locationcat[dss$company_locationcat!=0]=1
dss$company_locationcat=factor(dss$company_locationcat,levels=c(0,1),labels=c("East","West"))

table(dss$company_locationcat)

#work year category creation
table(dss$work_year)
dss$work_yearcat=NA
dss$work_yearcat[dss$work_year<=2022]=0
dss$work_yearcat[dss$work_year>2022]=1
dss$work_yearcat=factor(dss$work_yearcat,levels=c(0,1),labels=c("Before 2022","After 2022"))
table(dss$work_yearcat)



#work models category creation
table(dss$work_models)
dss$work_modelscat=NA
dss$work_modelscat[dss$work_models=="Hybrid"|
                     dss$work_models=="Remote"]=0
dss$work_modelscat[dss$work_models=="On-site"]=1
dss$work_modelscat=factor(dss$work_modelscat,levels = c(0,1),labels=c("Flexible employment","Non-flexible employment"))

table(dss$work_modelscat)


if(!require(labelled))install.packages("labelled",dependancies=T)
library(labelled)
set_variable_labels(dss$jobttcat,"Job Title")
set_variable_labels(dss$experience_levelcat,"Experience level")
set_variable_labels(dss$work_modelscat,"Work Model")
set_variable_labels(dss$employment_typecat,"Employment Type")
set_variable_labels(dss$work_yearcat,"Work Year")
set_variable_labels(dss$employee_residencecat,"Employee Residence")
set_variable_labels(dss$salary_currencycat,"Salary currency")
set_variable_labels(dss$company_locationcat,"Company Location")
set_variable_labels(dss$company_size,"Company Size")






if(!require (table1))install.packages("table1",dependencies=T) #code to tell r if the code is already install,do not install
library(table1)
table1(~factor(dss$jobttcat)+factor(dss$experience_levelcat)+factor(dss$employment_typecat)+factor(dss$work_modelscat)+factor(dss$work_yearcat)+factor(dss$employee_residencecat)+factor(dss$salary_currencycat)+factor(dss$salary_in_usdcat)+factor(dss$company_locationcat)+factor(dss$company_size))

#Bivariate analysis(analysis of two variables in our case which is salary in usd)

table1(~factor(dss$jobttcat)+factor(dss$experience_levelcat)+factor(dss$employment_typecat)+factor(dss$work_modelscat)+factor(dss$work_yearcat)+factor(dss$employee_residencecat)+factor(dss$salary_currencycat)+factor(dss$company_locationcat)+factor(dss$company_size)|salary_in_usdcat,dss)

#Data visualisation 
install.packages("ggplot2")
library(ggplot2)
fulltime_salary=dss$salary_in_usd[dss$employment_typecat=="Full-time"]
part_time_salary=dss$salary_in_usd[dss$employment_typecat=="part-time"]
hist(fulltime_salary,col="blue",main="salary histogram-Fulltime",xlab="salary",ylab="frequency") 

hist(part_time_salary,col="green",main="salary histogram-Fulltime",xlab="salary",ylab="frequency")
barplot()
