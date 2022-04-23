#######################################
### Hult International Business School
### MSBAN - Data Science
### Air France Case - Marketing
### Due date: 7 November
### By: Team 6
#######################################

install.packages("readxl")
install.packages("plotrix")
install.packages("ploty")
install.packages("ggplot2")
install.packages("caret")
install.packages("mlbench")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ROCR")

#Downloading the libraries we are going to use
library(readxl)
library(plotrix)
library(plotly)
library(ggplot2)
library(caret)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ROCR)

# Downloading the receive data into a data frame called my_data
my_first_data <- read_excel("Documents/Archivos Hult/Maestria - Clases/Data Science/Team 6/Air France Code/Air France Case Spreadsheet Supplement.xls", sheet = "DoubleClick")
View(Air_France_Case_Spreadsheet_Supplement)

# Massaging our data: Eliminating one line because it doesn't make sense that they made sells if they didn't got clicks
my_data <- my_first_data[ -which((my_first_data$Clicks == 0)&(my_first_data$Amount != 0)) , ]

# Creating valuable KPI as variables
my_data$Revenue <- my_data$Amount - my_data$`Total Cost`
my_data$ROA <- my_data$Revenue / my_data$`Total Cost`
my_data$Probability_Book <- my_data$`Total Volume of Bookings` / my_data$Impressions

View(my_data)

#Printing summary statistics for the main variables
print("Summary Statistics for Clicks")
summary(my_data$Clicks)
print("Summary Statistics for Cost")
summary(my_data$`Click Charges`)
print("Summary Statistics for Impressions")
summary(my_data$Impressions)
print("Summary Statistics for Income")
summary(my_data$Amount)
print("Summary Statistics for Engine Click Thru Ratio")
summary(my_data$`Engine Click Thru %`)
print("Summary Statistics for Transaction Convertion")
summary(my_data$`Trans. Conv. %`)

# Creating new data frames for each search engine 
google_global <-  subset(my_data, my_data$`Publisher Name`=="Google - Global")
google_us <-  subset(my_data, my_data$`Publisher Name`=="Google - US")
msn_global <-  subset(my_data, my_data$`Publisher Name`=="MSN - Global")
msn_us <-  subset(my_data, my_data$`Publisher Name`=="MSN - US")
overture_global <-  subset(my_data, my_data$`Publisher Name`=="Overture - Global")
overture_us <-  subset(my_data, my_data$`Publisher Name`=="Overture - US")
yahoo_us <-  subset(my_data, my_data$`Publisher Name`=="Yahoo - US")

# Creating an UDF to calculate mean and standard deviation
stats <- function(var1 , var2)
                {
                  result <- data.frame("Mean","Standard Deviation")
                  for (i in 1:var1) 
                    {
                      result[i,1] <- round(mean(unlist(var2[,i])),3)
                      result[i,2] <- round(sd(unlist(var2[,i])),3)
                    }
                  return(result)  
                }

# Using the UDF in order to get the stats for each search engine 
variable_stats_1 <-  stats( 9 , my_data [ , c(12,13,14,16,17,18,19,20,22)] )
rownames(variable_stats_1) <- c("Search Engine Bid", "Clickc", "Cost", "Impressions", "Engine Click Thru %", "Average Possition", "Transaction Covnertion", "Income", "Bookings")

variable_stats <-  stats( ncol(my_data)-23 , my_data [ , 24:ncol(my_data)] )
variable_stats_gg <-  stats( ncol(my_data)-23 , google_global [ , 24:ncol(my_data)] )
variable_stats_gu <-  stats( ncol(my_data)-23 , google_us [ , 24:ncol(my_data)] )
variable_stats_mg <-  stats( ncol(my_data)-23 , msn_global [ , 24:ncol(my_data)] )
variable_stats_mu <-  stats( ncol(my_data)-23 , msn_us [ , 24:ncol(my_data)] )
variable_stats_og <-  stats( ncol(my_data)-23 , overture_global [ , 24:ncol(my_data)] )
variable_stats_ou <-  stats( ncol(my_data)-23 , overture_us [ , 24:ncol(my_data)] )
variable_stats_yu <-  stats( ncol(my_data)-23 , yahoo_us [ , 24:ncol(my_data)] )

# Converting study KPI into binaries
    my_data$Revenue_bin <- ifelse( my_data$Revenue > variable_stats[1,1],1,0)
    my_data$ROA_bin <- ifelse( my_data$ROA > variable_stats[2,1],1,0)
    my_data$Probbook_bin <- ifelse( my_data$Probability_Book > variable_stats[3,1],1,0)
    
    google_global$Revenue_bin <- ifelse( google_global$Revenue > variable_stats_gg[1,1],1,0)
    google_global$ROA_bin <- ifelse( google_global$ROA > variable_stats_gg[2,1],1,0)
    google_global$Probbook_bin <- ifelse( google_global$Probability_Book > variable_stats_gg[3,1],1,0)
    
    google_us$Revenue_bin <- ifelse( google_us$Revenue > variable_stats_gu[1,1],1,0)
    google_us$ROA_bin <- ifelse( google_us$ROA > variable_stats_gu[2,1],1,0)
    google_us$Probbook_bin <- ifelse( google_us$Probability_Book > variable_stats_gu[3,1],1,0)
    
    msn_global$Revenue_bin <- ifelse( msn_global$Revenue > variable_stats_mg[1,1],1,0)
    msn_global$ROA_bin <- ifelse( msn_global$ROA > variable_stats_mg[2,1],1,0)
    msn_global$Probbook_bin <- ifelse( msn_global$Probability_Book > variable_stats_mg[3,1],1,0)
    
    msn_us$Revenue_bin <- ifelse( msn_us$Revenue > variable_stats_mu[1,1],1,0)
    msn_us$ROA_bin <- ifelse( msn_us$ROA > variable_stats_mu[2,1],1,0)
    msn_us$Probbook_bin <- ifelse( msn_us$Probability_Book > variable_stats_mu[3,1],1,0)

    overture_global$Revenue_bin <- ifelse( overture_global$Revenue > variable_stats_og[1,1],1,0)
    overture_global$ROA_bin <- ifelse( overture_global$ROA > variable_stats_og[2,1],1,0)
    overture_global$Probbook_bin <- ifelse( overture_global$Probability_Book > variable_stats_og[3,1],1,0)

    overture_us$Revenue_bin <- ifelse( overture_us$Revenue > variable_stats_ou[1,1],1,0)
    overture_us$ROA_bin <- ifelse( overture_us$ROA > variable_stats_ou[2,1],1,0)
    overture_us$Probbook_bin <- ifelse( overture_us$Probability_Book > variable_stats_ou[3,1],1,0)
    
    yahoo_us$Revenue_bin <- ifelse( yahoo_us$Revenue > variable_stats_yu[1,1],1,0)
    yahoo_us$ROA_bin <- ifelse( yahoo_us$ROA > variable_stats_yu[2,1],1,0)
    yahoo_us$Probbook_bin <- ifelse( yahoo_us$Probability_Book > variable_stats_yu[3,1],1,0)

#Plotting different variables focusing on Roa_bin success##`Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount
    hist(my_data$`Search Engine Bid`)
    hist(my_data$Clicks, breaks = 1000)
    hist(my_data$`Click Charges`)
    hist(my_data$Impressions)
    hist(my_data$`Engine Click Thru %`)
    hist(my_data$`Avg. Pos.`)
    hist(my_data$`Trans. Conv. %`)
    hist(my_data$`Total Cost/ Trans.`)
    hist(my_data$Amount)
    hist(my_data$`Total Volume of Bookings`)
    hist(my_data$Revenue)
    hist(my_data$ROA)
    hist(my_data$Probability_Book)
    
    pie_amount <-aggregate(Amount ~ `Publisher Name`, data = my_data, sum)
    pie3D(pie_amount[,2], labels=round(pie_amount$Amount/sum(pie_amount$Amount),3)*100 ,main = "Total Income", explode = 0.1, col = hcl.colors(length(pie_amount$Amount), "Spectral"), shade = 0.5) 
    legend("topright", pie_amount[,1], fill = hcl.colors(length(pie_amount$Amount), "Spectral"), cex = 0.65 )
    
    pie_cost <-aggregate(`Click Charges` ~ `Publisher Name`, data = my_data, sum)
    pie3D(pie_cost[,2], labels=round(pie_cost$`Click Charges`/sum(pie_cost$`Click Charges`),3)*100 ,main = "Total Cost", explode = 0.1, col = hcl.colors(length(pie_cost$`Click Charges`), "Spectral"), shade = 0.5) 
    legend("topright", pie_cost[,1], fill = hcl.colors(length(pie_cost$`Click Charges`), "Spectral"), cex = 0.65 )
    
    pie_click <-aggregate(Clicks ~ `Publisher Name`, data = my_data, sum)
    pie3D(pie_click[,2], labels=round(pie_click$Click/sum(pie_click$Clicks),3)*100 ,main = "Total Clicks", explode = 0.1, col = hcl.colors(length(pie_click$Clicks), "Spectral"), shade = 0.5) 
    legend("topright", pie_click[,1], fill = hcl.colors(length(pie_click$Clicks), "Spectral"), cex = 0.65 )
    
    ggplot(data=my_data, aes(x=`Search Engine Bid`, y=Clicks, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Search Engine Bid`, y=`Click Charges`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Search Engine Bid`, y=Impressions, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Search Engine Bid`, y=`Avg. Pos.`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Search Engine Bid`, y=Amount, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Clicks, y=`Click Charges`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Clicks, y=Impressions, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Clicks, y=`Avg. Pos.`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Clicks, y=Amount, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Click Charges`, y=Impressions, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Click Charges`, y=`Avg. Pos.`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=`Click Charges`, y=Amount, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Impressions, y=`Avg. Pos.`, color=ROA_bin))+
      geom_point()
    ggplot(data=my_data, aes(x=Impressions, y=Amount, color=ROA_bin))+
      geom_point()

#Creating training and test for all matrices
index_engine <- sample(1:nrow(my_data), size=0.8*nrow(my_data))
my_data_train <- my_data[index_engine,]
my_data_test <- my_data[-index_engine,]

index_gg <- sample(1:nrow(google_global), size=0.8*nrow(google_global))
google_global_train <- google_global[index_gg,]
google_global_test <- google_global[-index_gg,]

index_gu <- sample(1:nrow(google_us), size=0.8*nrow(google_us))
google_us_train <- google_us[index_gu,]
google_us_test <- google_us[-index_gu,]

index_mg <- sample(1:nrow(msn_global), size=0.8*nrow(msn_global))
msn_global_train <- msn_global[index_mg,]
msn_global_test <- msn_global[-index_mg,]

index_mu <- sample(1:nrow(msn_us), size=0.8*nrow(msn_us))
msn_us_train <- msn_us[index_mu,]
msn_us_test <- msn_us[-index_mu,]

index_og <- sample(1:nrow(overture_global), size=0.8*nrow(overture_global))
overture_global_train <- overture_global[index_og,]
overture_global_test <- overture_global[-index_og,]

index_ou <- sample(1:nrow(overture_us), size=0.8*nrow(overture_us))
overture_us_train <- overture_us[index_ou,]
overture_us_test <- overture_us[-index_ou,]

index_yu <- sample(1:nrow(yahoo_us), size=0.8*nrow(yahoo_us))
yahoo_us_train <- yahoo_us[index_yu,]
yahoo_us_test <- yahoo_us[-index_yu,]
    
# Estimating logistic regression functions for the variable revenue binary
glm_engines_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, family = "binomial")
summary(glm_engines_r)
glm_gg_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_global_train, family = "binomial")
summary(glm_gg_r)
glm_gu_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_us_train, family = "binomial")
summary(glm_gu_r)
glm_mg_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_global_train, family = "binomial")
summary(glm_mg_r)
glm_mu_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_us_train, family = "binomial")
summary(glm_mu_r)
glm_og_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_global_train, family = "binomial")
summary(glm_og_r)
glm_ou_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_us_train, family = "binomial")
summary(glm_ou_r)
glm_yu_r <- glm( Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = yahoo_us_train, family = "binomial")
summary(glm_yu_r)

# Estimating logistic regression functions for the variable ROA binary
glm_engines_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, family = "binomial")
summary(glm_engines_R)
glm_gg_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_global_train, family = "binomial")
summary(glm_gg_R)
glm_gu_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_us_train, family = "binomial")
summary(glm_gu_R)
glm_mg_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_global_train, family = "binomial")
summary(glm_mg_R)
glm_mu_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_us_train, family = "binomial")
summary(glm_mu_R)
glm_og_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_global_train, family = "binomial")
summary(glm_og_R)
glm_ou_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_us_train, family = "binomial")
summary(glm_ou_R)
glm_yu_R <- glm( ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = yahoo_us_train, family = "binomial")
summary(glm_yu_R)

# Estimating logistic regression functions for the variable Probability for Booking binary
glm_engines_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, family = "binomial")
summary(glm_engines_p)
glm_gg_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_global_train, family = "binomial")
summary(glm_gg_p)
glm_gu_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = google_us_train, family = "binomial")
summary(glm_gu_p)
glm_mg_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_global_train, family = "binomial")
summary(glm_mg_p)
glm_mu_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = msn_us_train, family = "binomial")
summary(glm_mu_p)
glm_og_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_global_train, family = "binomial")
summary(glm_og_p)
glm_ou_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = overture_us_train, family = "binomial")
summary(glm_ou_p)
glm_yu_p <- glm( Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = yahoo_us_train, family = "binomial")
summary(glm_yu_p)

# Creating Gini trees for every variable we analyse in the last 3 chunks 
ginitree_engine_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_engine_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gg_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gg_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gu_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gu_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mg_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mg_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mu_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mu_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_og_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_og_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_ou_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_ou_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_yu_r <- rpart(Revenue_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_yu_r, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_engine_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_engine_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gg_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gg_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gu_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gu_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mg_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mg_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mu_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mu_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_og_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_og_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_ou_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_ou_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_yu_R <- rpart(ROA_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_yu_R, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_engine_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_engine_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gg_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gg_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_gu_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_gu_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mg_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mg_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_mu_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_mu_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_og_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_og_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_ou_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_ou_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

ginitree_yu_p <- rpart(Probbook_bin ~ `Search Engine Bid` + Clicks + `Click Charges` + Impressions + `Avg. Pos.` + Amount, data = my_data_train, method =  "class", cp = 0.015)
rpart.plot(ginitree_yu_p, type=1, extra=1, cex = 0.75, tweak = TRUE, xflip = TRUE, box.palette = c("steelblue4", "turquoise4"))

# Confusion matrix for engine glm functions
prediction_engine_r <- predict(glm_engines_r , my_data_test, type = "response")
confusionMatrix(data = as.factor(as.numeric(prediction_engine_r > 0.5)), reference = as.factor(as.numeric(my_data_test$Revenue_bin)))
pred_engine_r <- prediction(prediction_engine_r, my_data_test$Revenue_bin)
perf_engine_r <-  performance(pred_engine_r, "tpr", "fpr")

prediction_engine_R <- predict(glm_engines_R , my_data_test, type = "response")
confusionMatrix(data = as.factor(as.numeric(prediction_engine_R > 0.5)), reference = as.factor(as.numeric(my_data_test$ROA_bin)))
pred_engine_R <- prediction(prediction_engine_R, my_data_test$ROA_bin)
perf_engine_R <-  performance(pred_engine_R, "tpr", "fpr")

prediction_engine_p <- predict(glm_engines_p , my_data_test, type = "response")
confusionMatrix(data = as.factor(as.numeric(prediction_engine_p > 0.5)), reference = as.factor(as.numeric(my_data_test$Probbook_bin)))
pred_engine_p <- prediction(prediction_engine_p, my_data_test$Probbook_bin)
perf_engine_p <-  performance(pred_engine_p, "tpr", "fpr")

# Confusion matrix for engine Gini trees
predictgini_engine_r <- predict(ginitree_engine_r, my_data_test, type = "prob")
predictiongini_engine_r <- prediction(predictgini_engine_r[,2], my_data_test$Revenue_bin)
performance_engine_r <- performance(predictiongini_engine_r, "tpr", "fpr")
plot(performance_engine_r, color = "black")
plot(perf_engine_r, col="turquoise4", add = TRUE)

predictgini_engine_R <- predict(ginitree_engine_R, my_data_test, type = "prob")
predictiongini_engine_R <- prediction(predictgini_engine_R[,2], my_data_test$ROA_bin)
performance_engine_R <- performance(predictiongini_engine_R, "tpr", "fpr")
plot(performance_engine_R, color = "black")
plot(perf_engine_R, col="turquoise4", add = TRUE)

predictgini_engine_p <- predict(ginitree_engine_p, my_data_test, type = "prob")
predictiongini_engine_p <- prediction(predictgini_engine_p[,2], my_data_test$Probbook_bin)
performance_engine_p <- performance(predictiongini_engine_p, "tpr", "fpr")
plot(performance_engine_p, color = "black")
plot(perf_engine_p, col="turquoise4", add = TRUE)









