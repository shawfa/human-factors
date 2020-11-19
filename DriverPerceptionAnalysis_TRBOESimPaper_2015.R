##################################################################################################################

# This R Script contains the analysis code for the TRB paper regarding Operational versus Simulator Environments.
# Please direct all questions or comments to the below contact information. 

##################################################################################################################

# Author: Atiyya Shaw
# Contact: atiyya@gatech.edu or 678.232.1808.
# Lab: Professor Michael P. Hunter at the Georgia Institute of Technology

##################################################################################################################
##################################################################################################################
##################################################################################################################
# PLEASE READ BELOW BEFORE YOU RUN THIS SCRIPT

# Review filenames, set working directory, and ensure that files are in the correct place prior to running.
# Load the required libraries. Note that you may have to install the packages on your computer if you are attempting
# to run packages you have never used before. Install packages in the command console using install.packages('pkgName')

##################################################################################################################
##################################################################################################################
##################################################################################################################
# PART 1: LOADING REQUIRED PACKAGES + VARIABLES

library(plyr) # This package allows for data summary
library(reshape2)
library(ggplot2)# Allows for us to reshape our data frames however we would like
kFCode = 32   # Let's us adjust our responses accordingly. This will make sense as you progress through the script. 

##################################################################################################################
##################################################################################################################
##################################################################################################################
# PART 2: READING IN OUR DATA

totalParticipants_HighSchool <-  read.csv("Cleaned_KMHS.csv", header = TRUE, stringsAsFactors = FALSE)
totalParticipants_RuralCollege <-  read.csv("Cleaned_Morehead.csv", header = TRUE, stringsAsFactors = FALSE)
totalParticipants_UrbanCollege <-  read.csv("Cleaned_Gatech.csv", header = TRUE, stringsAsFactors = FALSE)

# Let's put a tag on the data from different institutions
totalParticipants_HighSchool <- mutate(totalParticipants_HighSchool, Institution = 'HighSchool')
totalParticipants_RuralCollege <- mutate(totalParticipants_RuralCollege, Institution = 'RuralCollege')
totalParticipants_UrbanCollege <- mutate(totalParticipants_UrbanCollege, Institution = 'UrbanCollege')

##################################################################################################################
##################################################################################################################
##################################################################################################################
# PART 2: CLEANING OUR DATA + MAKING IT READY TO WORK WITH!

# First up, let's get the responses that relate to the images. 
totalParticipants_HighSchool <- totalParticipants_HighSchool[totalParticipants_HighSchool$trialcode =="experiment",]
totalParticipants_RuralCollege <- totalParticipants_RuralCollege[totalParticipants_RuralCollege$trialcode =="experiment",]
totalParticipants_UrbanCollege <- totalParticipants_UrbanCollege[totalParticipants_UrbanCollege$trialcode =="experiment",]

# Let's combine the data from the different institutions into one spreadsheet.
total <- rbind(totalParticipants_HighSchool, totalParticipants_RuralCollege, totalParticipants_UrbanCollege )
total <-data.frame(total)

# At this point, you may check the classification of the variables in your data frame by doing 
#   str(total) or summary(total) in the command window
# You should find that most of the variables are classified as characters
# Therefore, let's go ahead and change the variables that we need to be numeric into numeric variables

total$response <- as.numeric(total$response)
total$trial <- as.numeric(total$trial)
total$latency <- as.numeric(total$latency)

#Get them to 1-5 by subtracting 32 (kFcode) from the answers
total$response <- total$response - kFCode

# Let's get all the responses out that are negative (this eliminates all the 0s where people got timed out)
total$response[total$response<0]<-NA

# When conducting any analysis with latencies we might need to use the total data set. This will have
#   responses set to NA for where people timed out, but it will have the response times and all other information.

# Since we set all the responses that are negative to NA, we can use the complete.cases function in R to 
#   create a new data frame where we have eliminated all values across all columns for the timeouts. We want 
#   to be sure to keep this newly cleaned data set separate and not use it when analyzing latencies.

totalWithoutTimeouts<-total[complete.cases(total),]

# Let's make a dataset that's only simulator and one that's only OE so we can do summaries on them easily
cleanedDataSim <- totalWithoutTimeouts
cleanedDataOE <- totalWithoutTimeouts

cleanedDataSim$is.sim[cleanedDataSim$is.sim==FALSE]<-NA
cleanedDataSim<-cleanedDataSim[complete.cases(cleanedDataSim),]
cleanedDataSim <- mutate(cleanedDataSim, Env = 'Sim')

cleanedDataOE$is.sim[cleanedDataOE$is.sim==TRUE]<-NA
cleanedDataOE<-cleanedDataOE[complete.cases(cleanedDataOE),]
cleanedDataOE <- mutate(cleanedDataOE, Env = 'OE')

# We'll recombine the OE and Sim datasets now that we've marked them appropriately. 
cleanDataOESIM <- rbind (cleanedDataSim, cleanedDataOE)
cleanDataOESIM <-data.frame(cleanDataOESIM)

# We'll resave the marked OE and SIM data in totalWithoutTimeouts
totalWithoutTimeouts <- cleanDataOESIM

# To get only the task complexity data, parse the data using the line below
clean.task <- totalWithoutTimeouts[totalWithoutTimeouts$blocknum=="6"|totalWithoutTimeouts$blocknum=="7"| totalWithoutTimeouts$blocknum=="8",]
clean.task <- mutate(clean.task, Task = 'TASK')

# To get only the visual complexity data, parse the data using the line below
clean.visual <- totalWithoutTimeouts[totalWithoutTimeouts$blocknum=="14"|totalWithoutTimeouts$blocknum=="15"| totalWithoutTimeouts$blocknum=="16",]
clean.visual <- mutate(clean.visual, Task = 'VISUAL')

# We'll recombine the TASK and VISUAL datasets now that we've marked them appropriately and save over in totalWithoutTimeouts
# note that our dataset is now beautifully marked with Sim vs OE, Task vs. Visual, and KMHS vs. GaTech. 
# explore the data as we go to make sure you understand what we're dealing with.
totalWithoutTimeouts <- rbind (clean.task, clean.visual)
totalWithoutTimeouts <-data.frame(totalWithoutTimeouts)

# Alright now let's get the task complexity data frames for OE images and Sim images separately
cleanDataSimTask <-clean.task
cleanDataSimTask$is.sim[cleanDataSimTask$is.sim==FALSE]<-NA
cleanDataSimTask<-cleanDataSimTask[complete.cases(cleanDataSimTask),]

cleanDataOETask <-clean.task
cleanDataOETask$is.sim[cleanDataOETask$is.sim==TRUE]<-NA
cleanDataOETask<-cleanDataOETask[complete.cases(cleanDataOETask),]

# Alright now let's get the visual complexity data frames for OE images and Sim images separately
cleanDataSimVisual <-clean.visual
cleanDataSimVisual$is.sim[cleanDataSimVisual$is.sim==FALSE]<-NA
cleanDataSimVisual<-cleanDataSimVisual[complete.cases(cleanDataSimVisual),]

cleanDataOEVisual <-clean.visual
cleanDataOEVisual$is.sim[cleanDataOEVisual$is.sim==TRUE]<-NA
cleanDataOEVisual<-cleanDataOEVisual[complete.cases(cleanDataOEVisual),]

# At this point we've made a lot of data frames. This is a good spot to stop and check those frames and ensure that 
# nothing went wrong in the calculations.
# As a reminder - our cleaned data set that includes all institutions is called totalWithoutTimeouts.
# I'll introduce some commented output code here, please uncomment and use it to output and check the data files as you wish.
# You may definitely need to write out the totalWithoutTimeouts csv file if you want to do your own data manipulation in Excel.

#write.csv(totalWithoutTimeouts, "totalWithoutTimeouts.csv")
#write.csv(cleanedDataSim, "cleanedDataSim.csv")
#write.csv(cleanedDataOE, "cleanedDataOE.csv")
#write.csv(clean.task, "clean.task.csv")
#write.csv(clean.visual, "clean.visual.csv")
#write.csv(cleanDataSimTask, "cleanDataSimTask.csv")
#write.csv(cleanDataOETask, "cleanDataOETask.csv")
#write.csv(cleanDataSimVisual, "cleanDataSimVisual.csv")
#write.csv(cleanDataOEVisual, "cleanDataOEVisual.csv")

##################################################################################################################
##################################################################################################################
##################################################################################################################
# PART 3: SUMMARIZING + ANALYZING DATA. FUN FUN FUN. You may disregard this section at the moment, it is not relevant
# to the paper.

# Alright, why don't we get the median value for each participant for each image now.
# Recall, they saw each image 6 times, we want the median value from those six ratings. 
# That's going to introduce some 0.5s into the mix. 

totalWithoutTimeouts_sum<-ddply(totalWithoutTimeouts, .(subject, stimulusitem1, Institution, Env, Task),summarise,med=median(response))
#totalWithoutTimeouts_furthersum<-ddply(totalWithoutTimeouts, .(subject, stimulusitem1, Institution),summarise,med=median(response))

#Let's get the standard deviations for all institutions (checking for homoscedasticity -same st dev in diff groups)

totalWithoutTimeouts_sum_highschool <- subset(totalWithoutTimeouts_sum,totalWithoutTimeouts_sum$Institution == 'HighSchool')
sd_HighSchool <- sd(totalWithoutTimeouts_sum_highschool$med)

totalWithoutTimeouts_sum_ruralcollege <- subset(totalWithoutTimeouts_sum,totalWithoutTimeouts_sum$Institution == 'RuralCollege')
sd_RuralCollege <- sd(totalWithoutTimeouts_sum_ruralcollege$med)

totalWithoutTimeouts_sum_urbancollege <- subset(totalWithoutTimeouts_sum,totalWithoutTimeouts_sum$Institution == 'UrbanCollege')
sd_UrbanCollege <- sd(totalWithoutTimeouts_sum_urbancollege$med)


# Why don't we plot some of our data now.

# Let's do it first without taking medians
allImages<-ggplot(totalWithoutTimeouts, aes(x=response, fill = factor(Institution),title= "Differences across All Images")) + stat_bin (aes(y=..count..), binwidth=.5, position = "dodge") 
#xlab("Ratings of Perception") +  ylab("Counts") 
                                                                                                
                                                                                                                                      
                                                                                                                                      
#allImages <- x ggplot(totalWithoutTimeouts_sum, aes(x = med, y=count,  fill = factor(Type), title = "Novice vs. Experienced Driver Ratings")) +geom_bar(stat = "identity", position = "dodge") + xlab("Ratings of Perception") +
 # ylab("Counts") 




##################################################################################################################
##################################################################################################################
##################################################################################################################
# Alright below here, we will do some plots to get an idea of what our data looks like. 
# we will fit our data in Matlab however. You may disregard this section right now. It is not relevant to the 
# paper.

# Let's plot our ratings by image so we can see what the distributions look like
num_rows=5
num_cols=4
imageNames <- sort(unique(totalWithoutTimeouts$stimulusitem1))
numpages = ceiling((length(unique(totalWithoutTimeouts$stimulusitem1))/(num_rows*num_cols)))
pdf("ByImage.pdf", paper="letter", width=7.5, height=10.5)

#below is  how we would graph these if we weren't trying to make it on 2 pages (bc of the two pages, we have to use a for loop)
#Part.Hist <-ggplot(data=cleanedData, aes(x=response))+geom_histogram(binwidth=0.5, origin=.25)+facet_wrap(~subject, ncol=4, nrow = 20)

for(i in 1:numpages){
  imageHistData = subset(totalWithoutTimeouts, totalWithoutTimeouts$stimulusitem1 %in% imageNames[seq(((i-1)*num_rows*num_cols)+1,(i*num_rows*num_cols))])
  Image.Hist <-ggplot(data=imageHistData, aes(x=response))+geom_histogram(binwidth=1, origin=.25)
  Image.Hist <- Image.Hist +facet_wrap(~stimulusitem1, ncol=4)
  
  #imageHistData = cleanedDataWithoutTimeouts
  #image.stats<-ddply(imageHistData, .(stimulusitem1),summarise,mean=mean(response),med=median(response))
  
  #let's graph the participant mean on each graph
  
  #Image.Hist<-Image.Hist+geom_vline(data = image.stats, aes(xintercept=mean),color="red")
  
  # the lines below will graphs the overall mean and median on each graph
  #Part.Hist<-Part.Hist+geom_vline(aes(xintercept=(mean(cleanedDataWithoutTimeouts$response))),color="red")
  #Part.Hist<-Part.Hist+geom_vline( aes(xintercept=median(cleanedDataWithoutTimeouts$response)), color="blue")
  print(Image.Hist)
}
dev.off()

# Sample data: let's see if we can fit a beta distribution to this data for one image. 

imageHistData = subset(totalWithoutTimeouts, totalWithoutTimeouts$stimulusitem1 == 'CA_WW_007.jpg')

imageHistData$response[imageHistData$response == 1] <- 0.1
imageHistData$response[imageHistData$response == 2] <- 0.3
imageHistData$response[imageHistData$response == 3] <- 0.5
imageHistData$response[imageHistData$response == 4] <- 0.7
imageHistData$response[imageHistData$response == 5] <- 0.9

pdf("Test.pdf", paper="letter", width=7.5, height=10.5)
counts <-count(imageHistData$response)
counts$prob <- counts$freq/sum(counts$freq)
#fitdistr(imageHistData$response, "beta", start=list(shape1=0.1,shape2=0.1))

Image.Hist <-ggplot(counts, aes(x = x, y =prob)) +geom_bar(stat = "identity") 

Image.Hist <- Image.Hist+ curve(dbeta(x, 0.8587435, 1.657352))


#betareg(formula = prob ~ x, data = countsNew)

#> estBetaParams <- function(mu, var) {
#+     alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#+     beta <- alpha * (1 / mu - 1)
#+     return(params = list(alpha = alpha, beta = beta))
#+ }
#> x <- estBetaParams(.3413, 0.06393862)

#curve(pbeta(countsNew$prob, 0.8587435, 1.657352))
print(Image.Hist)
dev.off()

# Notes: 
# How to plot a beta curve: curve(dbeta(x, 7, 2))

##################################################################################################################
##################################################################################################################
##################################################################################################################

# Alright, now that we've done some diagnostics, let's go ahead and get our data in a nice format to run our beta fits
# in Matlab. We want to convert our responses for binning, and clean the data a little.
# We're going to write out all types of csv files here for the purpose of analyzing in Matlab.

totalWithoutTimeouts_Beta <- totalWithoutTimeouts

totalWithoutTimeouts_Beta$response[totalWithoutTimeouts_Beta$response == 1] <- 0.1
totalWithoutTimeouts_Beta$response[totalWithoutTimeouts_Beta$response == 2] <- 0.3
totalWithoutTimeouts_Beta$response[totalWithoutTimeouts_Beta$response == 3] <- 0.5
totalWithoutTimeouts_Beta$response[totalWithoutTimeouts_Beta$response == 4] <- 0.7
totalWithoutTimeouts_Beta$response[totalWithoutTimeouts_Beta$response == 5] <- 0.9  
  
write.csv(totalWithoutTimeouts_Beta, "totalWithoutTimeouts_beta.csv")
# We're going to take this CSV file and do some fun things with it in Matlab now.

# Okay, so we're going to actually write out a unique csv file for each image in R because matlab is really really stupid when it comes to 
# data processing. 

imageNames <- unique(totalWithoutTimeouts_Beta$stimulusitem1)
for (i in 1:length(imageNames))
{
  dataToWrite = subset(totalWithoutTimeouts_Beta, totalWithoutTimeouts_Beta$stimulusitem1 == imageNames[i])
  dataToWrite = cbind(dataToWrite$response)
  write.csv(dataToWrite, paste (imageNames[i],'.csv', sep = "", collapse = NULL))
}

##################################################################################################################
##################################################################################################################
##################################################################################################################

#Let's write out the OE ratings
cleanedDataOE_Beta <- cleanedDataOE

cleanedDataOE_Beta$response[cleanedDataOE_Beta$response == 1] <- 0.1
cleanedDataOE_Beta$response[cleanedDataOE_Beta$response == 2] <- 0.3
cleanedDataOE_Beta$response[cleanedDataOE_Beta$response == 3] <- 0.5
cleanedDataOE_Beta$response[cleanedDataOE_Beta$response == 4] <- 0.7
cleanedDataOE_Beta$response[cleanedDataOE_Beta$response == 5] <- 0.9  

cleanedDataOE_Beta = cleanedDataOE_Beta$response
write.csv(cleanedDataOE_Beta, "cleanedDataOE_Beta.csv")
  
#Let's write out the SIM ratings
cleanedDataSim_Beta <- cleanedDataSim

cleanedDataSim_Beta$response[cleanedDataSim_Beta$response == 1] <- 0.1
cleanedDataSim_Beta$response[cleanedDataSim_Beta$response == 2] <- 0.3
cleanedDataSim_Beta$response[cleanedDataSim_Beta$response == 3] <- 0.5
cleanedDataSim_Beta$response[cleanedDataSim_Beta$response == 4] <- 0.7
cleanedDataSim_Beta$response[cleanedDataSim_Beta$response == 5] <- 0.9  

cleanedDataSim_Beta = cleanedDataSim_Beta$response
write.csv(cleanedDataSim_Beta, "cleanedDataSim_Beta.csv")

#Let's write out the Visual Complexity ratings
clean.visual_Beta <-clean.visual

clean.visual_Beta$response[clean.visual_Beta$response == 1] <- 0.1
clean.visual_Beta$response[clean.visual_Beta$response == 2] <- 0.3
clean.visual_Beta$response[clean.visual_Beta$response == 3] <- 0.5
clean.visual_Beta$response[clean.visual_Beta$response == 4] <- 0.7
clean.visual_Beta$response[clean.visual_Beta$response == 5] <- 0.9  

clean.visual_Beta = clean.visual_Beta$response
write.csv(clean.visual_Beta, "clean.visual_Beta.csv")

#Let's write out the Task Complexity ratings
clean.task_Beta <- clean.task

clean.task_Beta$response[clean.task_Beta$response == 1] <- 0.1
clean.task_Beta$response[clean.task_Beta$response == 2] <- 0.3
clean.task_Beta$response[clean.task_Beta$response == 3] <- 0.5
clean.task_Beta$response[clean.task_Beta$response == 4] <- 0.7
clean.task_Beta$response[clean.task_Beta$response == 5] <- 0.9  

clean.task_Beta = clean.task_Beta$response
write.csv(clean.task_Beta, "clean.task_Beta.csv")


##################################################################################################################
##################################################################################################################
##################################################################################################################

#Let's write out the Task Complexity, OE ratings
cleanDataOETask_Beta <- cleanDataOETask

cleanDataOETask_Beta$response[cleanDataOETask_Beta$response == 1] <- 0.1
cleanDataOETask_Beta$response[cleanDataOETask_Beta$response == 2] <- 0.3
cleanDataOETask_Beta$response[cleanDataOETask_Beta$response == 3] <- 0.5
cleanDataOETask_Beta$response[cleanDataOETask_Beta$response == 4] <- 0.7
cleanDataOETask_Beta$response[cleanDataOETask_Beta$response == 5] <- 0.9  

cleanDataOETask_Beta = cleanDataOETask_Beta$response
write.csv(cleanDataOETask_Beta, "cleanDataOETask_Beta.csv")


#Let's write out the Task Complexity, Sim ratings
cleanDataSimTask_Beta <- cleanDataSimTask 

cleanDataSimTask_Beta$response[cleanDataSimTask_Beta$response == 1] <- 0.1
cleanDataSimTask_Beta$response[cleanDataSimTask_Beta$response == 2] <- 0.3
cleanDataSimTask_Beta$response[cleanDataSimTask_Beta$response == 3] <- 0.5
cleanDataSimTask_Beta$response[cleanDataSimTask_Beta$response == 4] <- 0.7
cleanDataSimTask_Beta$response[cleanDataSimTask_Beta$response == 5] <- 0.9  

cleanDataSimTask_Beta = cleanDataSimTask_Beta$response
write.csv(cleanDataSimTask_Beta, "cleanDataSimTask_Beta.csv")


#Let's write out the Visual Complexity, OE ratings
cleanDataOEVisual_Beta <- cleanDataOEVisual

cleanDataOEVisual_Beta$response[cleanDataOEVisual_Beta$response == 1] <- 0.1
cleanDataOEVisual_Beta$response[cleanDataOEVisual_Beta$response == 2] <- 0.3
cleanDataOEVisual_Beta$response[cleanDataOEVisual_Beta$response == 3] <- 0.5
cleanDataOEVisual_Beta$response[cleanDataOEVisual_Beta$response == 4] <- 0.7
cleanDataOEVisual_Beta$response[cleanDataOEVisual_Beta$response == 5] <- 0.9  

cleanDataOEVisual_Beta = cleanDataOEVisual_Beta$response
write.csv(cleanDataOEVisual_Beta, "cleanDataOEVisual_Beta.csv")


#Let's write out the Visual Complexity, Sim ratings



cleanDataSimVisual_Beta <- cleanDataSimVisual 

cleanDataSimVisual_Beta$response[cleanDataSimVisual_Beta$response == 1] <- 0.1
cleanDataSimVisual_Beta$response[cleanDataSimVisual_Beta$response == 2] <- 0.3
cleanDataSimVisual_Beta$response[cleanDataSimVisual_Beta$response == 3] <- 0.5
cleanDataSimVisual_Beta$response[cleanDataSimVisual_Beta$response == 4] <- 0.7
cleanDataSimVisual_Beta$response[cleanDataSimVisual_Beta$response == 5] <- 0.9  

cleanDataSimVisual_Beta = cleanDataSimVisual_Beta$response
write.csv(cleanDataSimVisual_Beta, "cleanDataSimVisual_Beta.csv")




#curve(dbeta(x,642,101),col="green");
#curve(dbeta(x,1286,130),add=TRUE,col="blue");
#curve(dbeta(x,2058,634),add=TRUE,col="orange");
#urve(dbeta(x,2131,651),add=TRUE,col="brown");