rm(list = ls())

getwd()
setwd('E:/DS Courses/Edwisor/Project 3')

train = read.csv('Train_data.csv')
test = read.csv('Test_data.csv')
dim(train)
dim(test)

# combining the file 
data = rbind(train,test)
names(data)
str(data)
head(data)

names(data) <- c('state', 'loyalty', 'a_code','contact', 'intl_plan', 'vm_plan', 'n_vm_mesg',
                 'min_day','calls_day', 'chrg_day', 'min_eve', 'calls_eve','chrg_eve', 
                 'mins_nit', 'calls_nit', 'chrg_nit', 'mins_intl', 'calls_intl', 'chrg_intl', 
                 'n_cs_calls', 'churn')

summary(data)



## EXPLORATORY ANALYSIS
tab <- as.data.frame(table(data$churn))
slices <- c(tab[1,2], tab[2,2]) 
lbls <- c("Churned-False", "Churned-True")
pct <- round(slices/sum(slices)*100,digits = 2)  # calculating % rounded to 2 digits
lbls <- paste(lbls, pct)                         # add percents to labels 
lbls <- paste(lbls,"%",sep="")                   # ad % to labels 

par(mar= c(1,1,1,1))
pie(slices,labels = lbls, col= c('cyan', 'red'),angle = 90,
    main="Percentage of Customer Churned")


par(mfrow=c(2,2)) # creates muitlp sub windows for plots
plot(data$loyalty~data$mins_intl,main='Tenure vs. International minutes',xlab='internationa minutes',ylab='Tenure',col=2)
plot(data$loyalty~data$min_day,main='Tenure Day minutes',xlab='Day minutes',ylab='Tenure',col=4 )
plot(data$loyalty~data$min_eve,main='Tenure vs.Evening minutes',xlab='Evening Minutes',ylab='Tenure',col=5 )
plot(data$loyalty~data$mins_nit,main='Tenure vs.Night minutes',xlab='Night Minutes',ylab='Tenure',col=3 )

par(mfrow=c(1,1))

library(dplyr)
library(ggplot2)
data %>% ggplot(aes(x=data$chrg_intl,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('red','yellow'))+labs(title='International charges desnisty split churn vs non churn' )
data %>% ggplot(aes(x=data$chrg_day,fill=churn))+ geom_density(alpha=0.7)+scale_fill_manual(values=c('blue','cyan'))+labs(title='Day charges desnisty split churn vs non churn' )

data %>% ggplot(aes(x=data$chrg_eve,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('white','blue'))+labs(title='Evening charges desnisty split churn vs non churn' )
data %>% ggplot(aes(x=data$chrg_nit,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('blue','red'))+labs(title='Night charges desnisty split churn vs non churn' )

data %>% ggplot(aes(x=data$n_cs_calls,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('white','pink'))+labs(title='Customer service call count desnisty split' )

data %>% ggplot(aes(x=data$calls_intl,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('red','yellow'))+labs(title='International calls desnisty')
data %>% ggplot(aes(x=data$calls_eve,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('red','yellow'))+labs(title='Evening calls desnisty')
data %>% ggplot(aes(x=data$calls_nit,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('red','yellow'))+labs(title='Night calls desnisty')
data %>% ggplot(aes(x=data$calls_day,fill=churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('red','yellow'))+labs(title='Day calls desnisty')


# Varios Box plots to understand the various attributes of input variables for Churned and not Churned customers
par(mfrow=c(2,2)) # creates muitlp sub windows for plots
# Boxplot to see the pattern of Total Minutes (Day/Evening/Night/International) for Churned / Not Churned Customers
boxplot(data$min_day ~ data$churn, data = data, col = "red",
        xlab = "Customer Churned",ylab = "Total Day mins")
boxplot(data$min_eve ~ data$churn, data = data, col = "green",
        xlab = "Customer Churned",ylab = "Total Evening mins")
boxplot(data$mins_nit  ~ data$churn, data = data, col = "magenta",
        xlab = "Customer Churned",ylab = "Total Night mins")
boxplot(data$mins_intl ~ data$churn, data = data, col = "purple",
        xlab = "Customer Churned",ylab = "Total International mins")



# Boxplot to see the pattern of Total Cals (Day/Evening/Night/International) for Churned / Not Churned Customers

boxplot(data$calls_day ~ data$churn, data = data, col = "purple",
        xlab = "Customer Churned",ylab = "Total Day Calls")      
boxplot(data$calls_eve ~ churn, data = data, col = "yellow",
        xlab = "Customer Churned",ylab = "Total Evening Calls")
boxplot(data$calls_nit ~ data$churn, data = data, col = "pink",
        xlab = "Customer Churned",ylab = "Total Night Calls")
boxplot(data$calls_intl~ data$churn, data = data, col = "cyan",
        xlab = "Customer Churned",ylab = "Total Int. Calls")   


# Boxplot to see the pattern of Total Charges (Day/Evening/Night/International) for Churned / Not Churned Customers

boxplot(data$chrg_day ~ data$churn, data = data, col = "red",
        xlab = "Customer Churned",ylab = "Total Day Charges")
boxplot(data$chrg_eve ~ data$churn, data = data, col = "yellow",
        xlab = "Customer Churned",ylab = "Total Evening Charges")
boxplot(data$chrg_nit ~ data$churn, data = data, col = "magenta",
        xlab = "Customer Churned",ylab = "Total Night Charges")
boxplot(data$chrg_intl ~ data$churn, data = data, col = "pink",
        xlab = "Customer Churned",ylab = "Total Int. Charges")



# Boxplot to see the pattern of Total Customer Service Calls made for Churned / Not Churned Customers

boxplot(data$n_cs_calls ~ data$churn, data = data, col = "brown",
        xlab = "Customer Churned",ylab = "Customer Calls Made")


# Boxplot to see the pattern of  stay for Churned / Not Churned Customers
boxplot(data$loyalty ~ data$churn, data = data, col = "blue",
        xlab = "Customer Churned",ylab = "Length of the Service(in days)")

sapply(data, class)

library(dplyr)
churn_pos <- filter(data,data$churn ==" True.")      # df with observations for Churned Customers 
churn_neg <- filter(data,data$churn ==" False.")    # df with observations for Not-Churned Customers
dim(churn_pos)
str(churn_pos)
dim(churn_neg)

churn_pos_intl_yes <- filter(churn_pos,churn_pos$intl_plan==" yes")
dim(churn_pos_intl_yes)
churn_neg_intl_yes <- filter(churn_neg,churn_neg$intl_plan==" yes")
dim(churn_neg_intl_yes)

# Finding mean among the various columns like minutes, calls & charges for Day/Evening/Night seperataly for Churned and 
# Not churned customers.

# Mean calculation for Mins Columns for Churned Customer.
tot_day_mins_true = mean(churn_pos$min_day)
tot_day_mins_true
tot_evening_mins_true = mean(churn_pos$min_eve)
tot_evening_mins_true
tot_ngt_mins_true = mean(churn_pos$mins_nit)
tot_ngt_mins_true
tot_int_mins_true = mean(churn_pos$mins_intl)
tot_int_mins_true

churn_neg$mins_day
# Mean calculation for Mins Columns for Not-Churned Customers
tot_day_mins_false = mean(churn_neg$min_day)
tot_day_mins_false
tot_evening_mins_false = mean(churn_neg$min_eve)
tot_evening_mins_false
tot_ngt_mins_false = mean(churn_neg$mins_nit)
tot_ngt_mins_false
tot_int_mins_false = mean(churn_neg$mins_intl)
tot_int_mins_false

# Storing the above calculated values of various Mins columns for both Churned and Not-churned in dataframe, so that we can 
# use the same to plot bars against each other.

data_mins <- structure(list(W=c(tot_day_mins_true,tot_day_mins_false),Q=c(tot_evening_mins_true,tot_evening_mins_false),
                            Y=c(tot_ngt_mins_true,tot_ngt_mins_false),Z=c(tot_int_mins_true,tot_int_mins_false)),
                       .Names = c("Day","Evening","Night","Int"),
                       class="data.frame",row.names=c(NA,-2L))
data_mins
# Plotting Bar plots.
colours <- c("red","blue")
barplot(as.matrix(data_mins), main="Customer's Avg Total Mins Usage",ylab = "Avg. Total Mins used",
        cex.lab = 1.2, cex.main = 1, beside=TRUE, col=colours,ylim = c(0,250),width = .5)

legend("topright", c("Churned->True","Churned->False"), cex=.9,
       bty="n", fill=colours)


#Like we calculated above similar caliculation is done for Mean calculation for Calls columns for Chruned Customer.

tot_day_calls_true = mean(churn_pos$calls_day)
tot_day_calls_true
tot_evening_calls_true = mean(churn_pos$calls_eve)
tot_evening_calls_true
tot_ngt_calls_true = mean(churn_pos$calls_nit)
tot_ngt_calls_true
tot_int_calls_true = mean(churn_pos_intl_yes$calls_intl)
tot_int_calls_true

# Mean calculation for Calls Columns for Not-Chruned Customers
tot_day_calls_false = mean(churn_neg$calls_day)
tot_day_calls_false
tot_evening_calls_false = mean(churn_neg$calls_eve)
tot_evening_calls_false
tot_ngt_calls_false = mean(churn_neg$calls_nit)
tot_ngt_calls_false
tot_int_calls_false = mean(churn_neg_intl_yes$calls_intl)
tot_int_calls_false


# Plotting Bar plots for Calls made for cutomers Chruned vs Not churned
data_calls <- structure(list(W=c(tot_day_calls_true,tot_day_calls_false),Q=c(tot_evening_calls_true,tot_evening_calls_false),
                             Y=c(tot_ngt_calls_true,tot_ngt_calls_false),Z=c(tot_int_calls_true,tot_int_calls_false)),
                        .Names = c("Day","Evening","Night","Int"),
                        class="data.frame",row.names=c(NA,-2L))
data_mins
colours <- c("red","blue")
barplot(as.matrix(data_calls), main="Avg Total Customer's Calls",ylab = "Avg. Total Calls",
        cex.lab = 1.2, cex.main = 1, beside=TRUE, col=colours,ylim = c(0,150),width = .5)
legend("topright", c("Churned->True","Churned->False"), cex=.9,
       bty="n", fill=colours)


#Like we calculated above similar caliculation is done for Mean calculation for Charges columns for Chruned Customer.
tot_day_chrgs_true = mean(churn_pos$chrg_day)
tot_day_chrgs_true
tot_evening_chrgs_true = mean(churn_pos$chrg_eve)
tot_evening_chrgs_true
tot_ngt_chrgs_true = mean(churn_pos$chrg_nit)
tot_ngt_chrgs_true
tot_int_chrgs_true = mean(churn_pos_intl_yes$chrg_intl)
tot_int_chrgs_true

# Mean calculation for Charges Columns for Not-Chruned Customers
tot_day_chrgs_false = mean(churn_neg$chrg_day)
tot_day_chrgs_false
tot_evening_chrgs_false = mean(churn_neg$chrg_eve)
tot_evening_chrgs_false
tot_ngt_chrgs_false = mean(churn_neg$chrg_nit)
tot_ngt_chrgs_false
tot_int_chrgs_false = mean(churn_neg_intl_yes$chrg_intl)
tot_int_chrgs_false

# Plotting Bar plots for Charges incurred for Chruned vs Not churned Customers
data_chrgs <- structure(list(W=c(tot_day_chrgs_true,tot_day_chrgs_false),Q=c(tot_evening_chrgs_true,tot_evening_chrgs_false),
                             Y=c(tot_ngt_chrgs_true,tot_ngt_chrgs_false),Z=c(tot_int_chrgs_true,tot_int_chrgs_false)),
                        .Names = c("Day","Evening","Night","Int"),
                        class="data.frame",row.names=c(NA,-2L))
data_chrgs
colours <- c("red","blue")
barplot(as.matrix(data_chrgs), main="Avg Total Charges of a Customer",ylab = "Avg Total Chrgs",
        cex.lab = 1.2, cex.main = 1, beside=TRUE, col=colours,ylim = c(0,40),width = .5)
legend("topright", c("Churned->True","Churned->False"), cex=.9,
       bty="n", fill=colours)



# Distribution of the numeric variable
h = hist(data$loyalty, breaks = 25, ylab = 'Frequency of loyalty', xlab = 'Total account length', main = 'Distribution of Account length(loyalty)', col = 'blue')

xfit = seq(min(data$loyalty),max(data$loyalty), length = 50)
yfit = dnorm(xfit, mean =mean(data$loyalty),sd=sd(data$loyalty))
yfit = yfit*diff(h$mids[1:2])*length(data$loyalty)
lines(xfit,yfit, col='red', lwd= 3)

n_m <- hist(data$n_vm_mesg, breaks = 25, ylab = 'Frequency voice mail messeges', xlab = 'Total voicemail messeges', main = 'Distribution of Voice mails', col = 'blue')
xfit = seq(min(data$n_vm_mesg),max(data$n_vm_mesg), length = 50)
yfit = dnorm(xfit, mean =mean(data$n_vm_mesg),sd=sd(data$n_vm_mesg))
yfit = yfit*diff(n_m$mids[1:2])*length(data$n_vm_mesg)
lines(xfit,yfit, col='red', lwd= 3)


m_d = hist(data$min_day, breaks = 25, ylab = 'Frequency day minutes', xlab = 'Total Day Minutes', main = 'Distribution of day minutes', col = 'blue')
xfit = seq(min(data$min_day),max(data$min_day), length = 50)
yfit = dnorm(xfit, mean =mean(data$min_day),sd=sd(data$min_day))
yfit = yfit*diff(m_d$mids[1:2])*length(data$min_day)
lines(xfit,yfit, col='red', lwd= 3)

c_d = hist(data$calls_day, breaks = 25, ylab = 'Frequency day calls', xlab = 'Total Day Calls', main = 'Distribution of day calls', col = 'blue')
xfit = seq(min(data$calls_day),max(data$calls_day), length = 50)
yfit = dnorm(xfit, mean =mean(data$calls_day),sd=sd(data$calls_day))
yfit = yfit*diff(c_d$mids[1:2])*length(data$calls_day)
lines(xfit,yfit, col='red', lwd= 3)

###############   CORRELATION PLOT    ###############
data_cor = subset(data, select = c('loyalty',"n_vm_mesg", "min_day", "calls_day",
                                   "chrg_day", "min_eve", "calls_eve","chrg_eve",
                                   "mins_nit", "calls_nit", "chrg_nit", "mins_intl",
                                   "calls_intl", "chrg_intl", "n_cs_calls"))

corplot_data = cor(data_cor)
library(corrgram)
corrgram(corplot_data, order = F,
         upper.panel = panel.pie, text.panel = panel.txt, main = 'correlation Plot')
#######################################################

# missing values
miss_count = sapply(data, function(x) sum(is.na(x)))
miss_count
data$a_code <- as.factor(data$a_code)

# dropping variable 'contact'
data$contact <- NULL
str(data)

#Now lets check the Missing values with colour map.
# install.packages('Amelia')
library(Amelia)
missmap(data)        # No missing values as the map shows no white cells which signifies for missing values
#######################################################

# Outlier Analysis
nu_index <- sapply(data,is.numeric)
nu_data <- data[,nu_index]
cnames <- colnames(nu_data)

for (i in 1:length(cnames)) {
  assign(paste0("gn",i),ggplot(aes_string(y= (cnames[i]), x= 'churn'), data= subset(data))+
  stat_boxplot(geom = 'errorbar', width=0.5)+
    geom_boxplot(outlier.color = 'red', fill = 'grey', outlier.shape = 18,
                 outlier.size = 1, notch = FALSE)+
    theme(legend.position = "bottom")+
    labs(y= cnames[i],x= "churn")+
    ggtitle(paste("Boxplot of churn for", cnames[i])))
}

# Plotting the boxplot grid
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5, ncol= 5)
gridExtra::grid.arrange(gn6,gn7,gn8,gn9,gn10, ncol= 5)
gridExtra::grid.arrange(gn11,gn12,gn13, ncol= 3)
gridExtra::grid.arrange(gn14,gn15, ncol= 2)

#  Removing outliers from only "number of voicemail messeges"
for(i in cnames){
  val <- data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  data[,i][data[,i] %in% val] = NA
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

print(paste0("Percentage of outliers in the data= ",100*length(val)/nrow(data),"%"))

sum(is.na(data))

miss_count = sapply(data, function(x) sum(is.na(x)))
miss_count
sum(is.na(data))


################  Encoding catagorical variables  ###############
data$state <- factor(data$state, levels = levels(data$state), labels = 1:length(levels(data$state)))
data$a_code <- factor(data$a_code, levels= levels(data$a_code), labels = 1:length(levels(data$a_code)))
data$intl_plan <- factor(data$intl_plan, levels = levels(data$intl_plan), labels= 1:length(levels(data$intl_plan)))
data$churn<- factor(data$churn, levels = levels(data$churn), labels = 1:length(levels(data$churn)))

###############  Splitting the dataset  ###############
library(caTools)
split <- sample.split(data$churn, SplitRatio = 2/3)
tr_data <- subset(data, split == TRUE)
ts_data <- subset(data, split == FALSE)


### Feature Scaling ###
tr_data[,c(2,6:19)] = scale(tr_data[,c(2,6:19)])
ts_data[,c(2,6:19)] = scale(ts_data[,c(2,6:19)])

library(dummies)
tr_df <- dummy.data.frame(tr_data, names = c("state","a_code","intl_plan","vm_plan"))
ts_df <- dummy.data.frame(ts_data, names = c("state","a_code","intl_plan","vm_plan"))
length(colnames(tr_df))

### Applying Kernel PCA ###
library(kernlab)
kpca <- kpca(~.,data= tr_data[-20], kernel= 'rbfdot', features= length(colnames(tr_df)))

tr_pca <- as.data.frame(predict(kpca,tr_data))
tr_pca$churn <- tr_data$churn
ts_pca <- as.data.frame(predict(kpca,ts_data))
ts_pca$churn <- ts_data$churn

#principal component analysis
prin_comp <- prcomp(tr_pca[-75])
names(prin_comp)

par(mar<- c(1,1,1,1))
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2
pr_var[1:10]  # variance of first 10 components


# PROPORTION OF EXPLAINED VARIANCE
prop_varex <- pr_var/sum(pr_var)
sum(prop_varex[1:21])

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b", col='blue', lwd= 3, cex = .8)
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b", col= 'green', lwd= 2, cex= .9)

tr_log <- tr_pca[,1:20]
tr_log$churn <- tr_pca$churn
ts_log <- ts_pca[1:20]
ts_log$churn <- ts_pca$churn

### Feature Logistic Regression ###
log_clf <- glm(churn~., family = binomial, data = tr_log)
prob_pred <- predict(log_clf, type= 'response', newdata= ts_log[-21])
log_pred <- ifelse(prob_pred > 0.5,1,0)
summary(log_clf)


cm = table(act_val= ts_log$churn, pre_val= prob_pred>.5)
cm
acc = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])

# Precision = tp/(tp+fp)
prec <- cm[1,1] / (cm[1,1]+cm[2,2])
# Recall  = tp/(tp+fn)
rec <- cm[1,1] / (cm[1,1]+cm[2,1])
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)
f_sc <- 2*(rec*prec)/(rec+prec)

scoring <- data.frame(0.5 ,acc,prec,rec,f_sc)
names(scoring) <- c("Above","ACCURACY", "PRECISION", "RECALL", "F1-SCORE")
scoring

library(ROCR)
roc_pred <- prediction(prob_pred, ts_log$churn)  
roc_pref <- performance(roc_pred, 'tpr', 'fpr')

plot(roc_pref, colorize = TRUE, print.cutoffs.at = seq(0.1,by= 0.1), lwd = 4, cex = .5)


cm2 = table(act_val= ts_log$churn, pre_val= prob_pred>.3)
cm2
acc2 <- (cm2[1,1]+cm2[2,2])/(cm2[1,1]+cm2[1,2]+cm2[2,1]+cm2[2,2])

prec2 <- cm2[1,1] / (cm2[1,1]+cm2[2,2])
rec2 <- cm2[1,1] / (cm2[1,1]+cm2[2,1])
f_sc_2 <- 2*(rec2*prec2)/(rec2+prec2)
scoring[2,] <- c(0.3 ,acc2,prec2,rec2,f_sc_2)
scoring

###############     ###############     ###############     ###############
# optimised value of mtry
library(randomForest)
bestmtry <- tuneRF(tr_log,tr_log$churn,stepFactor = 1.2,
                   improve = 0.01, trace = T, plot = T)
rf_model <- randomForest(churn~., data = tr_log)
rf_model
par(mar= c(5,4,1,1))
plot(rf_model)
# GINI index  (Feature priority)
rf_model$importance

varImpPlot(rf_model)
pre_rf <- predict(rf_model, newdata= ts_log, type= 'class')

library(caret)
print(confusionMatrix(pre_rf, ts_log$churn))

ll = confusionMatrix(pre_rf, ts_log$churn)
sapply(ll,print)


###### Gridsearch for hyperparameters tuning
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(churn~., data=tr_log, method="rf", metric= "Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

