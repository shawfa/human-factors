
# THIS IS THE OFFICIAL ANALYSIS SCRIPT FOR THE ADHD DATA. DIRECT ALL QUESTIONS TO
# ATIYYA@GATECH.EDU OR MICHAEL.HUNTER@CE.GATECH.EDU. THIS SCRIPT WAS WRITTEN WITH 
# THE INTENTION OF ANALYZING SESSION 1 DATA FOR 46 PARTICIPANTS (92 FILES). 

#######################################################################################
#######################################################################################
#######################################################################################
# PART 1. LOAD ALL PACKAGES AND FUNCTIONS NEEDED
#######################################################################################
#######################################################################################
#######################################################################################
# To install new packages use: install.packages("package name") in the command window
library(ggplot2) # our trusty plot package
library(plyr) # used to reshape our data sets
library(stringr) # used for the string locate and parsing functions
library(reshape2) # using it for the melt function
library(grid) # unit function in ggplot


#######################################################################################
#######################################################################################
#######################################################################################
# PART 2. LET'S PLOT SOME OVERALL SUMMARY PLOTS SO THAT WE GET AN IDEA OF OUR DATA.
#######################################################################################
#######################################################################################
#######################################################################################
# Let's first plot the lane, speed, and response latencies for the segment and event pieces along 
# the drive.Let's recall that we have two videos for each person, so let's plot the combined lane 
# deviations, and then one with Video 1 and Video 2. We can do this all on the same plot,
# and use a legend. We are going to do this for lane deviations, lane position, speed deviations,
# speed, and response latencies. We will also want to plot by event type (accident versus 
# work zone, etc.), so let's get those in the aggregated table as well. Recall, that the way
# in which we aggregate doesn't affect the mean, but will affect the standard deviations. As we
# take std deviations on more and more aggregated items, the std deviations will get smaller 
# (mean is becoming more consistent). 

# Get all csv filesnames in file
filenames <- dir(pattern=".csv") 

# Initialize a table that we will be adding to throughout the larger loop
Aggregated_table <- data.frame(matrix(ncol=16,nrow=0)) # creating empty data frame 
colnames(Aggregated_table) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events','Lane_Deviation', 'Lane_Position', "RMS_ld", 'Speed_Deviation', 'Speed','RMS_Speed', 'Response_Latencies', 'Response_Distances')

# Initialize a table that will store the full long dataset with all of the data
Aggregated_table_long <- data.frame(matrix(ncol=10,nrow=0)) # creating empty data frame 
colnames(Aggregated_table_long) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events', "RMS_ld",'RMS_Speed')

# looping through every participant (counting by 2s, cause each participant has 2 videos)
for(i in seq(1,length(filenames), 2)) {
  ## Read in raw data file.
  dataFile <- filenames[i]
  dataFile2 <- filenames[i+1]
  dataFilesVec <- c(dataFile,dataFile2)
####################################################################################################################
  # Extract necessary information from the data file

  # Figure out which scenario the data corresponds to.
  beginParse <- str_locate(dataFile, 'ADHD_')
  endParse <- str_locate(dataFile, '.csv')
  beginParse <- beginParse[1]
  endParse <- endParse[1]-1
  scenarioName <- substr(dataFile, beginParse, endParse)
  
  beginParse <- str_locate(dataFile2, 'ADHD_')
  endParse <- str_locate(dataFile2, '.csv')
  beginParse <- beginParse[1]
  endParse <- endParse[1]-1
  scenarioName2 <- substr(dataFile2, beginParse, endParse)
  
  scenarioNamesVec <- c(scenarioName, scenarioName2)
  
  # Figure out the participant ID.
  endParse <- str_locate(dataFile, '_S')
  endParse <- endParse[1]-1
  participantID <- substr(dataFile, 1, endParse)
  
  # Figure out ADHD status.
  if (( any(grepl("441FR", participantID))) |  ( any(grepl("450FR", participantID)))  |  ( any(grepl("459FR", participantID)))  |  
      ( any(grepl("464FL", participantID)))  |  ( any(grepl("471MR", participantID)))|  ( any(grepl("419FR", participantID))) |  ( any(grepl("45MR", participantID))) |
      ( any(grepl("413FR", participantID))) |  ( any(grepl("42FR", participantID)))  | ( any(grepl("41FR", participantID)))) {Status = 'ADHD'} else {Status = 'Control'}
  
  # Figure out Medication status.
  if (( any(grepl("40MR", participantID))) | ( any(grepl("47MR", participantID))) | ( any(grepl("451FL", participantID)))) {Medication = 'Medication'} else {Medication = NA}
  
  # Figure out Gender.
  if (any(grepl("F", participantID))) {Gender = 'Female'} else {Gender = 'Male'} 
  

  ####################################################################################################################
  # Create empty vectors that will store lane deviations, speed adherences, and response latencies of Video 1 and 2 for each participant.
  
  Lane_Dev_Total <- vector('numeric')
  Speed_Total <- vector('numeric')
  Response_Total <- vector('numeric')
  Events_Total <- vector('numeric')
  Segments_Total <- vector('numeric')
  Video_Total <- vector('numeric')
  Motorcycle_Total <- vector('numeric')
  Response_Video_Total <- vector('numeric')
  Response_Events <- vector('numeric')
  Response_Motorcycle <- vector('numeric')
  
  # Create empty table for mean and errors of lane deviation and speed for each participant. This one needs to be 
  # emptied for each new participant, which is why we're putting it here.
  ADHD_table_newPart <- data.frame(matrix(ncol=16,nrow=0)) # creating empty data frame 
  colnames(ADHD_table_newPart) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events','Lane_Deviation', 'Lane_Position', "RMS_ld", 'Speed_Deviation', 'Speed','RMS_Speed', 'Response_Latencies', 'Response_Distances')
  
  # Create empty table for aggregated_long table that needs to be emptied for each new participant
  ADHD_table_newPart_long <- data.frame(matrix(ncol=10,nrow=0)) # creating empty data frame 
  colnames(ADHD_table_newPart_long) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events', "RMS_ld",'RMS_Speed')
  
  ####################################################################################################################
  # New loop to extract info from BOTH videos for all participants. 
  for (j in (1:2)) {
    
    scenarioName <- scenarioNamesVec[j]
    rawData <- read.csv(dataFilesVec[j], header=TRUE)
    
    # Figure if the scenario has leading motorcycle or not.
    if ((any(grepl('Motorcycle', scenarioNamesVec[j])))) {Motorcycle <- 'Motorcycle'} else {Motorcycle <- 'No Motorcycle'}
    
    # Extract Video #
    beginParse <- str_locate(dataFilesVec[j],'_ADHD')
    beginParse <- beginParse[1]-1
    Video <- substr(dataFilesVec[j],beginParse,beginParse)
    
  ####################################################################################################################
    # Let's associate the scenarios with the order of events, and get the coordinates for each event here.
    if ((scenarioName == 'ADHD_1') | (scenarioName == 'ADHD_Motorcycle_1'))
    {
      Event1 = 'Event1_Billboard'
      # These coordinates represent the y locations of the begin distractor events.
      Event1_y = -445321.380
      Event2 = 'Event2_Accident'
      Event2_y = -431673.14
      Event3 = 'Event3_Workzone'
      Event3_y = -419551.46
      Event4 = 'Event4_NoDistraction'
      Event4_y = -403611.92
      Event5 = 'Event5_Police'
      Event5_y = -393501.72
    }
    
    if ((scenarioName == 'ADHD_2') | (scenarioName == 'ADHD_Motorcycle_2'))
    {
      Event1 = 'Event1_Police'
      Event1_y = -445855.01
      Event2 = 'Event2_Workzone'
      Event2_y = -432213.55
      Event3 = 'Event3_Billboard'
      Event3_y = -418561.34
      Event4 = 'Event4_Accident'
      Event4_y = -403601.58
      Event5 = 'Event5_NoDistraction'
      Event5_y = -393395.78
    }
    
    if ((scenarioName == 'ADHD_3') | (scenarioName == 'ADHD_Motorcycle_3'))
    {
      Event1 = 'Event1_Workzone'
      Event1_y = -446293.69
      Event2 = 'Event2_NoDistraction'
      Event2_y = -431675.78
      Event3 = 'Event3_Accident'
      Event3_y = -419003.33
      Event4 = 'Event4_Police'
      Event4_y = -403701.1
      Event5 = 'Event5_Billboard'
      Event5_y = -392953.34
    }
    
    if ((scenarioName == 'ADHD_4') | (scenarioName == 'ADHD_Motorcycle_4'))
    {
      Event1 = 'Event1_Accident'
      Event1_y = -445753.41
      Event2 = 'Event2_NoDistraction'
      Event2_y = -431675.78
      Event3 = 'Event3_Billboard'
      Event3_y = -418561.34
      Event4 = 'Event4_Police'
      Event4_y = -403701.73
      Event5 = 'Event5_Workzone'
      Event5_y = -393938.24
    }
    
    if ((scenarioName == 'ADHD_5') | (scenarioName == 'ADHD_Motorcycle_5'))
    {
      Event1 = 'Event1_Billboard'
      Event1_y = -445313.34
      Event2 = 'Event2_Police'
      Event2_y = -431773.7
      Event3 = 'Event3_NoDistraction'
      Event3_y = -419003.78
      Event4 = 'Event4_Workzone'
      Event4_y = -404136.08
      Event5 = 'Event5_Accident'
      Event5_y = -393393.92
    }
    
    ## Extract Event names for each particular scenario.
    beginParse <- str_locate(Event1,'_')
    beginParse <- beginParse[1]+1
    endParse1 <- str_length(Event1)
    endParse2 <- str_length(Event2)
    endParse3 <- str_length(Event3)
    endParse4 <- str_length(Event4)
    endParse5 <- str_length(Event5)
    
    Event1 <- substr(Event1,beginParse,endParse1)
    Event2 <- substr(Event2,beginParse,endParse2)
    Event3 <- substr(Event3,beginParse,endParse3)
    Event4 <- substr(Event4,beginParse,endParse4)
    Event5 <- substr(Event5,beginParse,endParse5)
    
  ####################################################################################################################
    
    # Obtain 1.25 mile (3300 feets segments) around each event
    Event1_y_rangeBegin <- ((Event1_y - 3300))
    Event1_y_rangeEnd <- ((Event1_y + 3300))
    Event1_segment <- subset (rawData, (((rawData$CentroidPosition_feet__1) >= Event1_y_rangeBegin) & ((rawData$CentroidPosition_feet__1) <= Event1_y_rangeEnd)))
    
    Event2_y_rangeBegin <- ((Event2_y - 3300))
    Event2_y_rangeEnd <- ((Event2_y + 3300))
    Event2_segment <- subset (rawData, (((rawData$CentroidPosition_feet__1) >= Event2_y_rangeBegin) & ((rawData$CentroidPosition_feet__1) <= Event2_y_rangeEnd)))
    
    Event3_y_rangeBegin <- ((Event3_y - 3300))
    Event3_y_rangeEnd <- ((Event3_y + 3300))
    Event3_segment <- subset (rawData, (((rawData$CentroidPosition_feet__1) >= Event3_y_rangeBegin) & ((rawData$CentroidPosition_feet__1) <= Event3_y_rangeEnd)))
    
    Event4_y_rangeBegin <- ((Event4_y - 3300))
    Event4_y_rangeEnd <- ((Event4_y + 3300))
    Event4_segment <- subset (rawData, (((rawData$CentroidPosition_feet__1) >= Event4_y_rangeBegin) & ((rawData$CentroidPosition_feet__1) <= Event4_y_rangeEnd)))
    
    Event5_y_rangeBegin <- ((Event5_y - 3300))
    Event5_y_rangeEnd <- ((Event5_y + 3300))
    Event5_segment <- subset (rawData, (((rawData$CentroidPosition_feet__1) >= Event5_y_rangeBegin) & ((rawData$CentroidPosition_feet__1) <= Event5_y_rangeEnd)))
    
    # Obtain the segments that go around the distance ranges.
    Segment1_BeforeEvent1 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > (Event1_y_rangeBegin - 6600) & (rawData$CentroidPosition_feet__1) < Event1_y_rangeBegin))
    Segment2_BetweenEvents1and2 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > Event1_y_rangeEnd) & ((rawData$CentroidPosition_feet__1) < Event2_y_rangeBegin))
    Segment3_BetweenEvents2and3 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > Event2_y_rangeEnd) & ((rawData$CentroidPosition_feet__1) < Event3_y_rangeBegin))
    Segment4_BetweenEvents3and4 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > Event3_y_rangeEnd) & ((rawData$CentroidPosition_feet__1) < Event4_y_rangeBegin))
    Segment5_BetweenEvents4and5 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > Event4_y_rangeEnd) & ((rawData$CentroidPosition_feet__1) < Event5_y_rangeBegin))
    Segment6_AfterEvent5 <- subset(rawData, ((rawData$CentroidPosition_feet__1) > Event5_y_rangeEnd))
    
  ####################################################################################################################
    # Find Lane Deviations and Lane Positions
    
    # Segments.
    Segment1_ld <- abs((Segment1_BeforeEvent1$CentroidPosition_feet__2) + 324)
    Segment1_lane_position <- abs((Segment1_BeforeEvent1$CentroidPosition_feet__2) + 318)
    
    Segment2_ld <- abs((Segment2_BetweenEvents1and2$CentroidPosition_feet__2) + 324)
    Segment2_lane_position <- abs((Segment2_BetweenEvents1and2$CentroidPosition_feet__2) + 318)
    
    Segment3_ld <- abs((Segment3_BetweenEvents2and3$CentroidPosition_feet__2) + 324)
    Segment3_lane_position <- abs((Segment3_BetweenEvents2and3$CentroidPosition_feet__2) + 318)
    
    Segment4_ld <- abs((Segment4_BetweenEvents3and4$CentroidPosition_feet__2) + 324)
    Segment4_lane_position <- abs((Segment4_BetweenEvents3and4$CentroidPosition_feet__2) + 318)
    
    Segment5_ld <- abs((Segment5_BetweenEvents4and5$CentroidPosition_feet__2) + 324)
    Segment5_lane_position <- abs((Segment5_BetweenEvents4and5$CentroidPosition_feet__2) + 318)
    
    # Events.
    Event1_ld <- abs((Event1_segment$CentroidPosition_feet__2) + 324)
    Event1_lane_position <- abs((Event1_segment$CentroidPosition_feet__2) + 318)
    
    Event2_ld <- abs((Event2_segment$CentroidPosition_feet__2) + 324)
    Event2_lane_position<- abs((Event2_segment$CentroidPosition_feet__2) + 318)
    
    Event3_ld <- abs((Event3_segment$CentroidPosition_feet__2) + 324)
    Event3_lane_position<- abs((Event3_segment$CentroidPosition_feet__2) + 318)
    
    Event4_ld <- abs((Event4_segment$CentroidPosition_feet__2) + 324)
    Event4_lane_position<- abs((Event4_segment$CentroidPosition_feet__2) + 318)
    
    Event5_ld <- abs((Event5_segment$CentroidPosition_feet__2) + 324)
    Event5_lane_position<- abs((Event5_segment$CentroidPosition_feet__2) + 318)
    
    
    # The below is across segments. 
    Segment1_ld_mean <- mean(Segment1_ld)
    Segment2_ld_mean <- mean(Segment2_ld)
    Segment3_ld_mean <- mean(Segment3_ld)
    Segment4_ld_mean <- mean(Segment4_ld)
    Segment5_ld_mean <- mean(Segment5_ld)
    Event1_ld_mean <- mean(Event1_ld)
    Event2_ld_mean <- mean(Event2_ld)
    Event3_ld_mean <- mean(Event3_ld)
    Event4_ld_mean <- mean(Event4_ld)
    Event5_ld_mean <- mean(Event5_ld)
    
    # The below is across segments. 
    Segment1_lane_position_mean <- mean(Segment1_lane_position)
    Segment2_lane_position_mean <- mean(Segment2_lane_position)
    Segment3_lane_position_mean <- mean(Segment3_lane_position)
    Segment4_lane_position_mean <- mean(Segment4_lane_position)
    Segment5_lane_position_mean <- mean(Segment5_lane_position)
    Event1_lane_position_mean <- mean(Event1_lane_position)
    Event2_lane_position_mean <- mean(Event2_lane_position)
    Event3_lane_position_mean <- mean(Event3_lane_position)
    Event4_lane_position_mean <- mean(Event4_lane_position)
    Event5_lane_position_mean <- mean(Event5_lane_position)
    
    
    # The below is across segments for rms
    RMS_Segment1_ld <- sqrt(sum((Segment1_lane_position - Segment1_lane_position_mean )^2)/length(Segment1_lane_position))
    RMS_Segment2_ld <- sqrt(sum((Segment2_lane_position-Segment1_lane_position_mean )^2)/length(Segment2_lane_position))
    RMS_Segment3_ld <- sqrt(sum((Segment3_lane_position-Segment1_lane_position_mean )^2)/length(Segment3_lane_position))
    RMS_Segment4_ld <- sqrt(sum((Segment4_lane_position-Segment1_lane_position_mean )^2)/length(Segment4_lane_position))
    RMS_Segment5_ld <- sqrt(sum((Segment5_lane_position-Segment1_lane_position_mean )^2)/length(Segment5_lane_position))
    RMS_Event1_ld <- sqrt(sum((Event1_lane_position - Segment1_lane_position_mean )^2)/length(Event1_lane_position))
    RMS_Event2_ld <- sqrt(sum((Event2_lane_position - Segment1_lane_position_mean )^2)/length(Event2_lane_position))
    RMS_Event3_ld <- sqrt(sum((Event3_lane_position - Segment1_lane_position_mean )^2)/length(Event3_lane_position))
    RMS_Event4_ld <- sqrt(sum((Event4_lane_position - Segment1_lane_position_mean )^2)/length(Event4_lane_position))
    RMS_Event5_ld <- sqrt(sum((Event5_lane_position - Segment1_lane_position_mean )^2)/length(Event5_lane_position))
    
    
    ld_set <- c(Segment1_ld_mean,Event1_ld_mean,Segment2_ld_mean,Event2_ld_mean,Segment3_ld_mean,Event3_ld_mean,Segment4_ld_mean,
                Event4_ld_mean,Segment5_ld_mean,Event5_ld_mean)
    lane_position_set <- c(Segment1_lane_position_mean,Event1_lane_position_mean,Segment2_lane_position_mean,Event2_lane_position_mean,Segment3_lane_position_mean,Event3_lane_position_mean,Segment4_lane_position_mean,
                           Event4_lane_position_mean,Segment5_lane_position_mean,Event5_lane_position_mean)
    RMS_Lane_Position <- c(RMS_Segment1_ld,RMS_Event1_ld,RMS_Segment2_ld,RMS_Event2_ld,RMS_Segment3_ld,RMS_Event3_ld, RMS_Segment4_ld, RMS_Event4_ld,
                           RMS_Segment5_ld, RMS_Event5_ld)
                           
                           
    segment_names <- c("Segment1","Event1","Segment2","Event2","Segment3","Event3","Segment4","Event4","Segment5","Event5")
    event_names <- c("NA",Event1,"NA",Event2,"NA",Event3,"NA",Event4,"NA",Event5)
    
    RMS_ld_set_long <- c(Segment1_lane_position,Event1_lane_position, Segment2_lane_position, Event2_lane_position, Segment3_lane_position, Event3_lane_position, Segment4_lane_position,
                         Event4_lane_position, Segment5_lane_position, Event5_lane_position)
    
    
      

    
    ####################################################################################################################
    # Find Speed Adherence.
    
    # Segments.
    Segment1_Speed_Dev <- abs((Segment1_BeforeEvent1$Speed_mph_) - 60)
    Segment1_Speed <- (Segment1_BeforeEvent1$Speed_mph_) 
    
    Segment2_Speed_Dev  <- abs((Segment2_BetweenEvents1and2$Speed_mph_) - 60)
    Segment2_Speed  <- (Segment2_BetweenEvents1and2$Speed_mph_)
    
    Segment3_Speed_Dev  <- abs((Segment3_BetweenEvents2and3$Speed_mph_) - 60)
    Segment3_Speed  <- (Segment3_BetweenEvents2and3$Speed_mph_) 
    
    Segment4_Speed_Dev  <- abs((Segment4_BetweenEvents3and4$Speed_mph_) - 60)
    Segment4_Speed  <- (Segment4_BetweenEvents3and4$Speed_mph_) 
    
    Segment5_Speed_Dev  <- abs((Segment5_BetweenEvents4and5$Speed_mph_) - 60)
    Segment5_Speed <- (Segment5_BetweenEvents4and5$Speed_mph_) 
    
    # Events.
    Event1_Speed_Dev  <- abs((Event1_segment$Speed_mph_) - 60)
    Event1_Speed  <- (Event1_segment$Speed_mph_) 
    
    Event2_Speed_Dev  <- abs((Event2_segment$Speed_mph_) - 60)
    Event2_Speed  <- (Event2_segment$Speed_mph_) 
    
    Event3_Speed_Dev  <- abs((Event3_segment$Speed_mph_) - 60)
    Event3_Speed  <- (Event3_segment$Speed_mph_) 
    
    Event4_Speed_Dev  <- abs((Event4_segment$Speed_mph_) - 60)
    Event4_Speed  <- (Event4_segment$Speed_mph_)
   
    Event5_Speed_Dev  <- abs((Event5_segment$Speed_mph_) - 60)
    Event5_Speed <- (Event5_segment$Speed_mph_) 
    
    
    # The below is across segments. 
    Segment1_Speed_Dev_mean <- mean(Segment1_Speed_Dev )
    Segment2_Speed_Dev_mean <- mean(Segment2_Speed_Dev )
    Segment3_Speed_Dev_mean <- mean(Segment3_Speed_Dev )
    Segment4_Speed_Dev_mean <- mean(Segment4_Speed_Dev )
    Segment5_Speed_Dev_mean <- mean(Segment5_Speed_Dev )
    Event1_Speed_Dev_mean <- mean(Event1_Speed_Dev )
    Event2_Speed_Dev_mean<- mean(Event2_Speed_Dev )
    Event3_Speed_Dev_mean <- mean(Event3_Speed_Dev )
    Event4_Speed_Dev_mean <- mean(Event4_Speed_Dev )
    Event5_Speed_Dev_mean<- mean(Event5_Speed_Dev )
    
    # The below is across segments. 
    Segment1_Speed_mean <- mean(Segment1_Speed)
    Segment2_Speed_mean<- mean(Segment2_Speed)
    Segment3_Speed_mean <- mean(Segment3_Speed )
    Segment4_Speed_mean <- mean(Segment4_Speed)
    Segment5_Speed_mean <- mean(Segment5_Speed)
    Event1_Speed_mean <- mean(Event1_Speed)
    Event2_Speed_mean <- mean(Event2_Speed)
    Event3_Speed_mean <- mean(Event3_Speed)
    Event4_Speed_mean <- mean(Event4_Speed )
    Event5_Speed_mean<- mean(Event5_Speed)
    
    # The below is across segments for rms
    RMS_Segment1 <- sqrt(sum((Segment1_BeforeEvent1$Speed_mph_ - 60 )^2)/length(Segment1_BeforeEvent1$Speed_mph_))
    RMS_Segment2 <- sqrt(sum((Segment2_BetweenEvents1and2$Speed_mph_-60 )^2)/length(Segment2_BetweenEvents1and2$Speed_mph_))
    RMS_Segment3 <- sqrt(sum((Segment3_BetweenEvents2and3$Speed_mph_-60 )^2)/length(Segment3_BetweenEvents2and3$Speed_mph_))
    RMS_Segment4 <- sqrt(sum((Segment4_BetweenEvents3and4$Speed_mph_-60 )^2)/length(Segment4_BetweenEvents3and4$Speed_mph_))
    RMS_Segment5 <- sqrt(sum((Segment5_BetweenEvents4and5$Speed_mph_-60 )^2)/length(Segment5_BetweenEvents4and5$Speed_mph_))
    RMS_Event1 <- sqrt(sum((Event1_segment$Speed_mph_ - 60 )^2)/length(Event1_segment$Speed_mph_))
    RMS_Event2 <- sqrt(sum((Event2_segment$Speed_mph_ - 60 )^2)/length(Event2_segment$Speed_mph_))
    RMS_Event3 <- sqrt(sum((Event3_segment$Speed_mph_ - 60 )^2)/length(Event3_segment$Speed_mph_))
    RMS_Event4 <- sqrt(sum((Event4_segment$Speed_mph_ - 60 )^2)/length(Event4_segment$Speed_mph_))
    RMS_Event5 <- sqrt(sum((Event5_segment$Speed_mph_ - 60 )^2)/length(Event5_segment$Speed_mph_))
    
  
    speed_dev_set <- c(Segment1_Speed_Dev_mean,Event1_Speed_Dev_mean,Segment2_Speed_Dev_mean,Event2_Speed_Dev_mean,Segment3_Speed_Dev_mean,Event3_Speed_Dev_mean,Segment4_Speed_Dev_mean,
                Event4_Speed_Dev_mean,Segment5_Speed_Dev_mean,Event5_Speed_Dev_mean)

    speed_set <- c(Segment1_Speed_mean,Event1_Speed_mean,Segment2_Speed_mean,Event2_Speed_mean,Segment3_Speed_mean,Event3_Speed_mean,Segment4_Speed_mean,
                   Event4_Speed_mean,Segment5_Speed_mean,Event5_Speed_mean)
    
    RMS_speed_set <- c(RMS_Segment1,RMS_Event1, RMS_Segment2,RMS_Event2, RMS_Segment3,RMS_Event3,
                       RMS_Segment4,RMS_Event4,RMS_Segment5,RMS_Event5)
    
    RMS_speed_set_long <- c(Segment1_BeforeEvent1$Speed_mph_,Event1_segment$Speed_mph_, Segment2_BetweenEvents1and2$Speed_mph_, Event2_segment$Speed_mph_,Segment3_BetweenEvents2and3$Speed_mph_,Event3_segment$Speed_mph_, Segment4_BetweenEvents3and4$Speed_mph_, 
                            Event4_segment$Speed_mph_, Segment5_BetweenEvents4and5$Speed_mph_, Event5_segment$Speed_mph_)
    
    
    ####################################################################################################################
    # Let's find some response latencies
    timestamps_responses <- subset(rawData$Time, ((rawData$AuxButtons_1 == 1 )| (rawData$AuxButtons_2 == 1))  )
    diststamps_responses <- subset(rawData$CentroidPosition_feet__1, ((rawData$AuxButtons_1 == 1 )| (rawData$AuxButtons_2 == 1)))
    
    # Let's get the Diamond Timestamps, since that's what we care about in this context. The person has to respond
    # to the diamond.
    
    # The Accident scene does not have an associated diamond, and so will be labeled event. 
    # The other 4 events do have a diamond. 
    
    if ((scenarioName == 'ADHD_1') | (scenarioName == 'ADHD_Motorcycle_1'))
    {
      Diamond1 = 'Diamond1_Billboard'
      #These coordinates represent the y locations of the begin distractor events.
      Diamond1_y = -445763.820
      Diamond2 = 'Event2_Accident'
      Diamond2_y = -431673.140
      Diamond3 = 'Diamond3_Workzone'
      Diamond3_y = -419011.820
      Diamond4 = 'Diamond4_NoDistraction'
      Diamond4_y = -403611.920
      Diamond5 = 'Diamond5_Police'
      Diamond5_y = -393403.82
    }
    
    if ((scenarioName == 'ADHD_2') | (scenarioName == 'ADHD_Motorcycle_2'))
    {
      Diamond1 = 'Diamond1_Police'
      Diamond1_y = -445755.78
      Diamond2 = 'Diamond2_Workzone'
      Diamond2_y = -431675.78
      Diamond3 = 'Diamond3_Billboard'
      Diamond3_y = -419003.78
      Diamond4 = 'Event4_Accident'
      Diamond4_y = -403601.58
      Diamond5 = 'Diamond5_NoDistraction'
      Diamond5_y = -393395.78
    }
    
    if ((scenarioName == 'ADHD_3') | (scenarioName == 'ADHD_Motorcycle_3'))
    {
      Diamond1 = 'Diamond1_Workzone'
      Diamond1_y = -445755.78
      Diamond2 = 'Diamond2_NoDistraction'
      Diamond2_y = -431675.78
      Diamond3 = 'Event3_Accident'
      Diamond3_y = -419003.33
      Diamond4 = 'Diamond4_Police'
      Diamond4_y = -403603.78
      Diamond5 = 'Diamond5_Billboard'
      Diamond5_y = -393395.78
    }
    
    if ((scenarioName == 'ADHD_4') | (scenarioName == 'ADHD_Motorcycle_4'))
    {
      Diamond1 = 'Event1_Accident'
      Diamond1_y = -445753.41
      Diamond2 = 'Diamond2_NoDistraction'
      Diamond2_y = -431675.78
      Diamond3 = 'Diamond3_Billboard'
      Diamond3_y = -419003.78
      Diamond4 = 'Diamond4_Police'
      Diamond4_y = -403603.78
      Diamond5 = 'Diamond5_Workzone'
      Diamond5_y = -393395.78
    }
    
    if ((scenarioName == 'ADHD_5') | (scenarioName == 'ADHD_Motorcycle_5'))
    {
      Diamond1 = 'Diamond1_Billboard'
      Diamond1_y = -445755.78
      Diamond2 = 'Diamond2_Police'
      Diamond2_y = -431675.78
      Diamond3 = 'Diamond3_NoDistraction'
      Diamond3_y = -419003.78
      Diamond4 = 'Diamond4_Workzone'
      Diamond4_y = -403603.78
      Diamond5 = 'Event5_Accident'
      Diamond5_y = -393393.92
    }
    
    # Next, we will obtain the timestamps that correspond to when the diamond begins in the scenario. We're using the same logic that we did for events in Part 1 above.
    # Review as necessary. 
    
    # We're going to redefine raw data here simply to remove the first 20 seconds or so. This is because during the first few seconds,
    # the Centroid position is 0, which messes us up with the (rawData$CentroidPosition_feet__1) >= Diamond1_y line below. 
    rawData <- rawData[rawData$Time >= 20,]
    
    Diamond1_timestamp <- subset(rawData$Time, floor(rawData$CentroidPosition_feet__1) >= Diamond1_y)
    Diamond1_timestamp <- Diamond1_timestamp[1]
    
    Diamond2_timestamp <- subset(rawData$Time, floor(rawData$CentroidPosition_feet__1) >= Diamond2_y)
    Diamond2_timestamp <- Diamond2_timestamp[1]
    
    Diamond3_timestamp <- subset(rawData$Time, floor(rawData$CentroidPosition_feet__1) >= Diamond3_y)
    Diamond3_timestamp <- Diamond3_timestamp[1]
    
    Diamond4_timestamp <- subset(rawData$Time, floor(rawData$CentroidPosition_feet__1) >= Diamond4_y)
    Diamond4_timestamp <- Diamond4_timestamp[1]
    
    Diamond5_timestamp <- subset(rawData$Time, floor(rawData$CentroidPosition_feet__1) >= Diamond5_y)
    Diamond5_timestamp <- Diamond5_timestamp[1] 
    
    # Let's get the 1 minute segments in which the events occur. 
    
    # We will obtain 1 minute segments around the timestamps.
    Diamond1_timestamp_rangeBegin <- ((Diamond1_timestamp - 30))
    Diamond1_timestamp_rangeEnd <- ((Diamond1_timestamp + 30))
    Diamond1_response_segment <- subset (timestamps_responses, ((timestamps_responses) >= Diamond1_timestamp_rangeBegin) & ((timestamps_responses) <= Diamond1_timestamp_rangeEnd))
    Diamond1_first_response_timestamp <- Diamond1_response_segment[1]  
    Diamond1_latency <- (Diamond1_timestamp - Diamond1_first_response_timestamp)
    
    Diamond2_timestamp_rangeBegin <- ((Diamond2_timestamp - 30))
    Diamond2_timestamp_rangeEnd <- ((Diamond2_timestamp + 30))
    Diamond2_response_segment <- subset (timestamps_responses, ((timestamps_responses) >= Diamond2_timestamp_rangeBegin) & ((timestamps_responses) <= Diamond2_timestamp_rangeEnd))
    Diamond2_first_response_timestamp <- Diamond2_response_segment[1]
    Diamond2_latency <- ( Diamond2_timestamp - Diamond2_first_response_timestamp )
    
    Diamond3_timestamp_rangeBegin <- ((Diamond3_timestamp - 30))
    Diamond3_timestamp_rangeEnd <- ((Diamond3_timestamp + 30))
    Diamond3_response_segment <- subset (timestamps_responses, ((timestamps_responses) >= Diamond3_timestamp_rangeBegin) & ((timestamps_responses) <= Diamond3_timestamp_rangeEnd))
    Diamond3_first_response_timestamp <- Diamond3_response_segment[1]
    Diamond3_latency <- ( Diamond3_timestamp - Diamond3_first_response_timestamp  )
    
    Diamond4_timestamp_rangeBegin <- ((Diamond4_timestamp - 30))
    Diamond4_timestamp_rangeEnd <- ((Diamond4_timestamp + 30))
    Diamond4_response_segment <- subset (timestamps_responses, ((timestamps_responses) >= Diamond4_timestamp_rangeBegin) & ((timestamps_responses) <= Diamond4_timestamp_rangeEnd))
    Diamond4_first_response_timestamp <- Diamond4_response_segment[1]
    Diamond4_latency <- (Diamond4_timestamp - Diamond4_first_response_timestamp)
    
    Diamond5_timestamp_rangeBegin <- ((Diamond5_timestamp - 30))
    Diamond5_timestamp_rangeEnd <- ((Diamond5_timestamp + 30))
    Diamond5_response_segment <- subset (timestamps_responses, ((timestamps_responses) >= Diamond5_timestamp_rangeBegin) & ((timestamps_responses) <= Diamond5_timestamp_rangeEnd))
    Diamond5_first_response_timestamp <- Diamond5_response_segment[1]
    Diamond5_latency <- (Diamond5_timestamp - Diamond5_first_response_timestamp)
    
    response_set <- c(NA, Diamond1_latency,NA, Diamond2_latency,NA, Diamond3_latency,NA, Diamond4_latency,NA,Diamond5_latency)

    
    
    #Let's calculate the distance between when the person responds and the diamond occurs.
    # We're going to use 1.25 miles swatches for the events since that is how we defined the events segments. 
    # .625 miles * 2 = 1.25 miles = 6600 feet
    # we're going to store this stuff in the table and plot it as well and do all that fun stuff. 
    
    # We will obtain 1 minute segments around the timestamps.
    Diamond1_diststamp_rangeBegin <- ((Diamond1_y - 3300))
    Diamond1_diststamp_rangeEnd <- ((Diamond1_y + 3300))
    Diamond1_response_segment <- subset (diststamps_responses, ((diststamps_responses) >= Diamond1_diststamp_rangeBegin) & ((diststamps_responses) <= Diamond1_diststamp_rangeEnd))
    Diamond1_first_response_diststamp <- Diamond1_response_segment[1]  
    Diamond1_latency_distance <- abs(abs(Diamond1_y) - abs(Diamond1_first_response_diststamp))
    
    Diamond2_diststamp_rangeBegin <- ((Diamond2_y - 3300))
    Diamond2_diststamp_rangeEnd <- ((Diamond2_y + 3300))
    Diamond2_response_segment <- subset (diststamps_responses, ((diststamps_responses) >= Diamond2_diststamp_rangeBegin) & ((diststamps_responses) <= Diamond2_diststamp_rangeEnd))
    Diamond2_first_response_diststamp <- Diamond2_response_segment[1]  
    Diamond2_latency_distance <- abs(abs(Diamond2_y) - abs(Diamond2_first_response_diststamp))
    
    Diamond3_diststamp_rangeBegin <- ((Diamond3_y - 3300))
    Diamond3_diststamp_rangeEnd <- ((Diamond3_y + 3300))
    Diamond3_response_segment <- subset (diststamps_responses, ((diststamps_responses) >= Diamond3_diststamp_rangeBegin) & ((diststamps_responses) <= Diamond3_diststamp_rangeEnd))
    Diamond3_first_response_diststamp <- Diamond3_response_segment[1]  
    Diamond3_latency_distance <- abs(abs(Diamond3_y) - abs(Diamond3_first_response_diststamp))
    
    Diamond4_diststamp_rangeBegin <- ((Diamond4_y - 3300))
    Diamond4_diststamp_rangeEnd <- ((Diamond4_y + 3300))
    Diamond4_response_segment <- subset (diststamps_responses, ((diststamps_responses) >= Diamond4_diststamp_rangeBegin) & ((diststamps_responses) <= Diamond4_diststamp_rangeEnd))
    Diamond4_first_response_diststamp <- Diamond4_response_segment[1]  
    Diamond4_latency_distance <- abs(abs(Diamond4_y) - abs(Diamond4_first_response_diststamp))
    
    Diamond5_diststamp_rangeBegin <- ((Diamond5_y - 3300))
    Diamond5_diststamp_rangeEnd <- ((Diamond5_y + 3300))
    Diamond5_response_segment <- subset (diststamps_responses, ((diststamps_responses) >= Diamond5_diststamp_rangeBegin) & ((diststamps_responses) <= Diamond5_diststamp_rangeEnd))
    Diamond5_first_response_diststamp <- Diamond5_response_segment[1]  
    Diamond5_latency_distance <- abs(abs(Diamond5_y) - abs(Diamond5_first_response_diststamp))
    
    response_distance_set <- c(NA, Diamond1_latency_distance,NA, Diamond2_latency_distance,NA, Diamond3_latency_distance,NA, Diamond4_latency_distance,NA,Diamond5_latency_distance)
    
  
    #######################################################################################################################################
    # Create a vector with Segments and Events numbers for the long dataset. Essentially we need to label the right number of rows,
    # so this part of the script gets that dynamically for the speed and lane deviations, which are the only two that we are actually
    # doing RMSD on. 
    
    S1_length <- length(Segment1_BeforeEvent1$Speed_mph_)
    S2_length <- length(Segment2_BetweenEvents1and2$Speed_mph_)
    S3_length <- length(Segment3_BetweenEvents2and3$Speed_mph_)
    S4_length <- length(Segment4_BetweenEvents3and4$Speed_mph_)
    S5_length <- length(Segment5_BetweenEvents4and5$Speed_mph_)
    
    E1_length <- length(Event1_segment$Speed_mph_)
    E2_length <- length(Event2_segment$Speed_mph_)
    E3_length <- length(Event3_segment$Speed_mph_)
    E4_length <- length(Event4_segment$Speed_mph_)
    E5_length <- length(Event5_segment$Speed_mph_)
    
    NA1 <- rep(NA,length(1:S1_length))
    NA2 <- rep(NA,length(1:S2_length))
    NA3 <- rep(NA,length(1:S3_length))
    NA4 <- rep(NA,length(1:S4_length))
    NA5 <- rep(NA,length(1:S5_length))
    
    S1 <- rep('Segment1',length(1:S1_length))
    S2 <- rep('Segment2',length(1:S2_length))
    S3 <- rep('Segment3',length(1:S3_length))
    S4 <- rep('Segment4',length(1:S4_length))
    S5 <- rep('Segment5',length(1:S5_length))
    
    ## Extract Event names.
    
    E1 <- rep(Event1,length(1:E1_length))
    E2 <- rep(Event2,length(1:E2_length))
    E3 <- rep(Event3,length(1:E3_length))
    E4 <- rep(Event4,length(1:E4_length))
    E5 <- rep(Event5,length(1:E5_length))
    
    E1.1 <- rep('Event1',length(1:E1_length))
    E2.1 <- rep('Event2',length(1:E2_length))
    E3.1 <- rep('Event3',length(1:E3_length))
    E4.1 <- rep('Event4',length(1:E4_length))
    E5.1 <- rep('Event5',length(1:E5_length))
    
    Events <- c(NA1,E1,NA2,E2,NA3,E3,NA4,E4,NA5,E5)

    
    Segments <- c(S1,E1.1,S2,E2.1,S3,E3.1,S4,E4.1,S5,E5.1)

    ####################################################################################################################
    # Create two tables with ld and speed by segments and events. One is for the RMSD and is the long version. This will be a super big
    # table. 
    
    # Create table for mean and errors of lane deviation and speed.
    ADHD_table <- data.frame(matrix(ncol=16,nrow=10)) # creating empty data frame 
    colnames(ADHD_table) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events','Lane_Deviation', 'Lane_Position', "RMS_ld", 'Speed_Deviation', 'Speed','RMS_Speed', 'Response_Latencies', 'Response_Distances')
    
    ADHD_table$ParticipantID <- participantID
    ADHD_table$Video_no. <- Video
    ADHD_table$Status <- Status
    ADHD_table$Medication <- Medication
    ADHD_table$Motorcycle <- Motorcycle
    ADHD_table$Segments <- segment_names
    ADHD_table$Events <- event_names
    ADHD_table$Lane_Deviation <- ld_set
    ADHD_table$Lane_Position <- lane_position_set 
    ADHD_table$Speed_Deviation <- speed_dev_set
    ADHD_table$Speed <- speed_set
    ADHD_table$Response_Latencies <-response_set
    ADHD_table$Response_Distances <-response_distance_set
    ADHD_table$Gender <-Gender
    ADHD_table$RMS_Speed <- RMS_speed_set
    ADHD_table$RMS_ld<- RMS_Lane_Position 

    ADHD_table_newPart <- rbind(ADHD_table_newPart, ADHD_table)
    
    # Create table for rmsd of lane and speed here
    ADHD_table_long <- data.frame(matrix(ncol=10, nrow = length(Segments)))
    colnames(ADHD_table_long) <- c('ParticipantID','Video_no.','Gender','Status','Medication','Motorcycle','Segments','Events', "RMS_ld",'RMS_Speed')
    
    ADHD_table_long$ParticipantID <- participantID
    ADHD_table_long$Video_no. <- Video
    ADHD_table_long$Status <- Status
    ADHD_table_long$Medication <- Medication
    ADHD_table_long$Motorcycle <- Motorcycle
    ADHD_table_long$Segments <- Segments
    ADHD_table_long$Events <- Events
    ADHD_table_long$Gender <-Gender
    ADHD_table_long$RMS_ld <-RMS_ld_set_long 
    ADHD_table_long$RMS_Speed <-  RMS_speed_set_long 
    
    ADHD_table_newPart_long <- rbind(ADHD_table_newPart_long, ADHD_table_long)
    
    
    
  }
    
 ####################################################################################################################
    
Aggregated_table <- rbind(Aggregated_table, ADHD_table_newPart)
Aggregated_table_long <- rbind(Aggregated_table_long, ADHD_table_newPart_long)
  
  
}

# run the below when you want to avoid the loop. If you have a correct table previously saved, you can use that.
#write.csv(Aggregated_table, "Aggregated_table.csv")
#write.csv(Aggregated_table_long, "Aggregated_table_long.csv")
Aggregated_table_long<- read.csv("Aggregated_table_long.csv", header=TRUE) 
Aggregated_table<-read.csv("Aggregated_table.csv", header=TRUE) 


# Let's remove Event 5 from the Aggregated Table. We're doing this because the length of the final event is super short
# and also very highly variable since the actual termination of the scenario happens 1000-1200 feet after event 5 (14 seconds), 
# and the warning text telling participants to stop driving occurred even sooner at 500 feet after the final event. 
Aggregated_table <- Aggregated_table

# Let's add a column that just says either Segment or Event to our dataset. 
Segments <- Aggregated_table[Aggregated_table$Segments == 'Segment1' |  Aggregated_table$Segments == 'Segment2' |
                Aggregated_table$Segments == 'Segment3' |  Aggregated_table$Segments == 'Segment4' |
                Aggregated_table$Segments == 'Segment5',] 
Segments$SegmentType <- 'Segment'
Events <- Aggregated_table[Aggregated_table$Segments == 'Event1' |  Aggregated_table$Segments == 'Event2' |
                               Aggregated_table$Segments == 'Event3' |  Aggregated_table$Segments == 'Event4' 
                           |  Aggregated_table$Segments == 'Event5',] 
Events$SegmentType <- 'Event'

Aggregated_table <- rbind(Segments, Events)
Aggregated_table_WithoutEvent5 <- Aggregated_table[Aggregated_table$Segments != 'Event5',]

# Let's add the column that says either Segment or Event to the Aggregated_table_long
Aggregated_table_long <- Aggregated_table_long

# Let's add a column that just says either Segment or Event to our dataset. 
Segments <- Aggregated_table_long[Aggregated_table_long$Segments == 'Segment1' |  Aggregated_table_long$Segments == 'Segment2' |
                                    Aggregated_table_long$Segments == 'Segment3' |  Aggregated_table_long$Segments == 'Segment4' |
                                    Aggregated_table_long$Segments == 'Segment5',] 
Segments$SegmentType <- 'Segment'
Events <- Aggregated_table_long[Aggregated_table_long$Segments == 'Event1' |  Aggregated_table_long$Segments == 'Event2' |
                                  Aggregated_table_long$Segments == 'Event3' |  Aggregated_table_long$Segments == 'Event4' 
                           |  Aggregated_table_long$Segments == 'Event5',] 
Events$SegmentType <- 'Event'

Aggregated_table_long <- rbind(Segments, Events)
Aggregated_table_long_WithoutEvent5 <- Aggregated_table_long[Aggregated_table_long$Segments != 'Event5',]

# test code below. ignore
Aggregated_table_long_WithoutEvent5_test <- Aggregated_table_long_WithoutEvent5[Aggregated_table_long_WithoutEvent5$ParticipantID == '40MR',]
                                
write.csv(Aggregated_table_long_WithoutEvent5_test, "AAggregated_table_long_WithoutEvent5_test.csv")
####################################################################################################################

# Let's summarize the mean and standard deviation of lane deviations and speed adherence for each of the segments
Segments_table <- ddply(Aggregated_table_WithoutEvent5, .(Segments),summarise,mean_ld=mean(Lane_Deviation),stdev_ld = sd(Lane_Deviation), mean_pos=mean(Lane_Position),stdev_pos = sd(Lane_Position),
                  mean_speed_dev = mean(Speed_Deviation), stdev_speed_dev = sd(Speed_Deviation))
Segments_table<- melt(Segments_table,  id="Segments") 


# Let's plot
Segments_table$Segments <- factor(Segments_table$Segments, c("Segment1","Event1","Segment2","Event2","Segment3","Event3","Segment4","Event4","Segment5","Event5"))

png ("aggPerformanceMetrics.png", width = 600, height = 600)
aggregatePerformanceMetricsPlot <- ggplot(Segments_table, aes(x = Segments_table$Segments, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Deviations(feet or mph as applicable)') + 
  ggtitle('Driver Performance Metrics') 
#+  geom_hline(aes(x = 0, y=0))


print(aggregatePerformanceMetricsPlot)
dev.off()

# Let's summarize and look at actual speed too. We have to do this on a separate graph because the magnitude of the scale is going to
# make it impossible to see the small deviations for lane and speed adherence. 
Segments_table <- ddply(Aggregated_table, .(Segments),summarise,  mean_speed = mean(Speed), stdev_speed = sd(Speed))
Segments_table <- ddply(Aggregated_table, .(Segments),summarise,  mean_speed = mean(Speed))
Segments_table<- melt(Segments_table,  id="Segments") 

# Let's plot
Segments_table$Segments <- factor(Segments_table$Segments, c("Segment1","Event1","Segment2","Event2","Segment3","Event3","Segment4","Event4","Segment5","Event5"))
png ("aggSpeedMetrics.png", width = 600, height = 600)
aggregateSpeedMetricsPlot <- ggplot(Segments_table, aes(x = Segments_table$Segments, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Deviations(feet or mph as applicable)') + 
  ggtitle('Speed Performance Metrics')
print(aggregateSpeedMetricsPlot)
dev.off()

# Let's summarize the mean and standard deviation of lane deviations and speed adherence for each of the event types
Events_table <- ddply(Aggregated_table_WithoutEvent5, .(Events),summarise,mean_ld=mean(Lane_Deviation),stdev_ld = sd(Lane_Deviation), mean_pos=mean(Lane_Position),stdev_pos = sd(Lane_Position),
                        mean_speed_dev = mean(Speed_Deviation), stdev_speed_dev = sd(Speed_Deviation))
Events_table<- melt(Events_table,  id="Events") 
Events_table <- Events_table[Events_table$Events != 'NA',]

# Let's plot

png ("aggPerformanceMetricsEvents.png", width = 600, height = 600)
aggregatePerformanceMetricsEventsPlot <- ggplot(Events_table, aes(x = Events_table$Events, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Deviations(feet or mph as applicable)') + 
  ggtitle('Driver Performance Metrics') 


print(aggregatePerformanceMetricsEventsPlot)
dev.off()


# Let's summarize and look at actual speed for event types too. We have to do this on a separate graph because the magnitude of the scale is going to
# make it impossible to see the small deviations for lane and speed adherence. 
Events_table <- ddply(Aggregated_table_WithoutEvent5, .(Events),summarise,  mean_speed = mean(Speed), stdev_speed = sd(Speed))
Events_table <- ddply(Aggregated_table_WithoutEvent5, .(Events),summarise,  mean_speed = mean(Speed))

Events_table<- melt(Events_table,  id="Events") 

# Let's plot
png ("aggSpeedMetrics.png", width = 600, height = 600)
aggregateSpeedMetricsEventsPlot <- ggplot(Events_table, aes(x = Events_table$Events, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Deviations(feet or mph as applicable)') + 
  ggtitle('Speed Performance Metrics')
print(aggregateSpeedMetricsEventsPlot)
dev.off()


# Let's summarize the mean and standard deviation of response latencies for each of the event types

#first, we remove the NAs because it messes up our averages
Events_table <- Aggregated_table_WithoutEvent5[Aggregated_table_WithoutEvent5$Events != 'NA',]
Events_table <- Events_table[Events_table$Events != 'Accident',]

Events_table<-Events_table[complete.cases(Events_table$Response_Latencies),]

Events_table <- ddply(Events_table, .(Events),summarise,mean_response=mean(Response_Latencies),std_response=sd(Response_Latencies))
Events_table<- melt(Events_table,  id="Events") 

# Let's plot

png ("aggPerformanceMetricsEventsResponseLatencies.png", width = 600, height = 600)
aggregatePerformanceMetricsEventsResponseLatenciesPlot <- ggplot(Events_table, aes(x = Events_table$Events, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Time (seconds)') + 
  ggtitle('Driver Performance Metrics') 
#+  geom_hline(aes(x = 0, y=0))


print(aggregatePerformanceMetricsEventsResponseLatenciesPlot)
dev.off()


# Let's summarize the mean and standard deviation of response distances for each of the event types

#first, we remove the NAs because it messes up our averages
Events_table <- Aggregated_table_WithoutEvent5[Aggregated_table_WithoutEvent5$Events != 'NA',]
Events_table <- Events_table[Events_table$Events != 'Accident',]

Events_table<-Events_table[complete.cases(Events_table$Response_Distances),]

Events_table <- ddply(Events_table, .(Events),summarise,mean_response=mean(Response_Distances),std_response=sd(Response_Distances))
Events_table<- melt(Events_table,  id="Events") 

# Let's plot

png ("aggPerformanceMetricsEventsResponseDistances.png", width = 600, height = 600)
aggregatePerformanceMetricsEventsResponseDistancesPlot <- ggplot(Events_table, aes(x = Events_table$Events, y = value, color = variable)) + geom_line(aes(group=variable)) +  geom_point() +
  theme(axis.title.x = element_text(face="bold", colour="#000000", size=14), axis.title.y= element_text(face="bold", colour="#000000", size=12),
        axis.text.x  = element_text(angle=90, hjust =1, size=14, colour="#000000"), axis.text.y = element_text(face="bold", colour="#000000", size=14), axis.title.y = element_text(size=14), 
        plot.title = element_text(size=14, face="bold", vjust=2),
        legend.position = 'bottom',legend.key.size = unit(.9, "cm")) + xlab('Drive Segments') + ylab('Distance (feet)') + 
  ggtitle('Driver Performance Metrics') 
#+  geom_hline(aes(x = 0, y=0))


print(aggregatePerformanceMetricsEventsResponseDistancesPlot)
dev.off()

#Let's summarize the data in some forms where we can test Gender (as a significant variable)

Gender_table <- ddply(Aggregated_table, .(ParticipantID, Gender),summarise,mean_lanepos=mean(Lane_Position),
                      sd_lanepos=sd(Lane_Position), mean_speed=mean(Speed), sd_speed=sd(Speed))
write.csv(Gender_table, "Gender_table.csv")



#______________________________________________________NOTES____________________________________________________________

#write.csv(Aggregated_table, "Aggregated_table.csv")


#XX ADHD/Control Motorcyle Video 1 Segment 1 Mean Lane Dev
#XX ADHD/Control Motorcyle Video 1 Segment 2 Mean Lane Dev
#XX ADHD/Control Motorcyle Video 1 Segment 3 Mean Lane Dev
#XX ADHD/Control Motorcyle Video 2 Segment 1 Mean Lane Dev
#XX ADHD/Control Motorcyle Video 2 Segment 2 Mean Lane Dev
#XX ADHD/Control Motorcyle Video 2 Segment 3 Mean Lane Dev



#######################################################################################
#######################################################################################
#######################################################################################
# PART 3. LET'S PUT OUR DATA INTO SOME DATA FRAMES WHERE WE CAN RUN ANOVAS. 
# You do need to run the loop portion of PART 2 because we need the aggregated table. 
# You do not need to run the graphs portion above. 
#######################################################################################
#######################################################################################
#######################################################################################

# RESPONSE DISTANCES
# Let's summarize response distances across both videos, and remember that here we won't use the set where we have removed 
# the last event, because then we actually lose the last work zone that participant 467FR didn't hit. 
AnovaDATA <- Aggregated_table
AnovaDATA_Events <- ddply(AnovaDATA, .(ParticipantID, Events, Gender, Status),summarise,mean_response=mean(Response_Distances))
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "Accident",]
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "NA",]
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide.csv")

# Let's summarize response distances only for video 1, and remember that here we won't use the set where we have removed 
# the last event, because then we actually lose the last work zone that participant 467FR didn't hit. 
AnovaDATA_video1 <- Aggregated_table[ Aggregated_table$Video_no. == 1 ,]
AnovaDATA_Events <- ddply(AnovaDATA_video1, .(ParticipantID, Events, Gender, Status),summarise,mean_response=mean(Response_Distances))
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "Accident",]
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "NA",]
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide.csv")

# RESPONSE LATENCIES
# Let's summarize response latencies across both videos, and remember that here we won't use the set where we have removed 
# the last event, because then we actually lose the last work zone that participant 467FR didn't hit. 
AnovaDATA <- Aggregated_table
AnovaDATA_Events <- ddply(AnovaDATA, .(ParticipantID, Events, Gender, Status),summarise,mean_response=mean(Response_Latencies))
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "Accident",]
AnovaDATA_Events<-AnovaDATA_Events[complete.cases(AnovaDATA_Events$Events),]

AnovaDATA_Events_wide <- reshape(AnovaDATA_Events, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide_ResponseLatencies.csv")

# Let's summarize response latencies only for video 1, and remember that here we won't use the set where we have removed 
# the last event, because then we actually lose the last work zone that participant 467FR didn't hit. 
AnovaDATA_video1 <- Aggregated_table[ Aggregated_table$Video_no. == 1 ,]
AnovaDATA_Events <- ddply(AnovaDATA_video1, .(ParticipantID, Events, Gender, Status),summarise,mean_response=mean(Response_Latencies))
AnovaDATA_Events<-AnovaDATA_Events[AnovaDATA_Events$Events != "Accident",]
AnovaDATA_Events<-AnovaDATA_Events[complete.cases(AnovaDATA_Events$Events),]
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide_ResponseLatencies_Video1.csv")



# SEGMENT VERSUS EVENT FOR THREE METRICS: RMS LANE DEV + RMS SPEED + MEAN SPEED
# Let's summarize mean speed here for video1 and 2 , and this time we're going to use the data set that doesn't
# include the last segment. Let's make it for segment versus event first - so our repeated measures will be for segment and event.
#AnovaDATA <- Aggregated_table_WithoutEvent5[ Aggregated_table_WithoutEvent5$Video_no. == 1 ,]
AnovaDATA <- Aggregated_table_WithoutEvent5
AnovaDATA_Events_Speed <- ddply(AnovaDATA, .(ParticipantID, SegmentType, Gender, Status),summarise,meanspeed_response=mean(Speed))
#AnovaDATA_SegmentType_wide_meanSpeed <- reshape(AnovaDATA_Events, timevar = "SegmentType", idvar = c("ParticipantID", "Gender", "Status"), direction = "wide")


# I have to make a giant dataset that holds all the speed and lane positions and all that good stuff, so that
# when I take the standard deviation I can do it for the entire vector rather than just for a bit.
# Cool so we did this, and it's called Aggregated_table_long_WithoutEvent5
# Let's summarize RMSD for speed and lane deviations here now, and then we can put it into the other dataset
# uses both Video 1 and Video 2
AnovaDATA_Events_rms <- ddply(Aggregated_table_long_WithoutEvent5, .(ParticipantID, SegmentType, Gender, Status),summarise,RMSD_Speed_60mph = 
                                sqrt(sum((RMS_Speed - 60 )^2)/length(RMS_Speed)), RMSD_Speed_Average = 
                                sqrt(sum((RMS_Speed - mean(RMS_Speed) )^2)/length(RMS_Speed)), RMSD_Speed_sd_check = 
                                sd(RMS_Speed), RMSD_ld=
                                sqrt(sum((RMS_ld - mean(RMS_ld) )^2)/length(RMS_ld)))


# Let's combine the rms speed, ld, and mean speed datasets now. 
AnovaDATA_Events_rms$mean_speed <- AnovaDATA_Events_Speed$meanspeed_response
# Let's convert it to the wide format so that we can run the ANOVA in SPSS
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events_rms, timevar = "SegmentType", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide.csv")

# EVENT TYPES FOR THREE METRICS:  RMS LANE DEV + RMS SPEED + MEAN SPEED
# Now let's do our three metrics as we did above for the different event types
AnovaDATA <- Aggregated_table_WithoutEvent5
AnovaDATA_Events_Speed<-AnovaDATA[complete.cases(AnovaDATA$Events),]
AnovaDATA_Events_Speed <- ddply(AnovaDATA_Events_Speed, .(ParticipantID, Events, Gender, Status),summarise,meanspeed_response=mean(Speed))

AnovaDATA <-Aggregated_table_long_WithoutEvent5
AnovaDATA_Events_rms<-AnovaDATA[complete.cases(AnovaDATA$Events),]
AnovaDATA_Events_rms <- ddply(AnovaDATA_Events_rms, .(ParticipantID, Events, Gender, Status),summarise,RMSD_Speed_60mph = 
                                sqrt(sum((RMS_Speed - 60 )^2)/length(RMS_Speed)), RMSD_Speed_Average = 
                                sqrt(sum((RMS_Speed - mean(RMS_Speed) )^2)/length(RMS_Speed)), RMSD_Speed_sd_check = 
                                sd(RMS_Speed), RMSD_ld=
                                sqrt(sum((RMS_ld - mean(RMS_ld) )^2)/length(RMS_ld)))

AnovaDATA_Events_rms$mean_speed <- AnovaDATA_Events_Speed$meanspeed_response
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events_rms, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_five_wide.csv")

                        
# VIDEO 1 ONLY: SEGMENT VERSUS EVENT FOR THREE METRICS: RMS LANE DEV + RMS SPEED + MEAN SPEED
AnovaDATA <- Aggregated_table_WithoutEvent5[ Aggregated_table_WithoutEvent5$Video_no. == 1 ,]
AnovaDATA_Events_Speed <- ddply(AnovaDATA, .(ParticipantID, SegmentType, Gender, Status),summarise,meanspeed_response=mean(Speed))

AnovaDATA <- Aggregated_table_long_WithoutEvent5[ Aggregated_table_long_WithoutEvent5$Video_no. == 1 ,]
AnovaDATA_Events_rms <- ddply(AnovaDATA, .(ParticipantID, SegmentType, Gender, Status),summarise,RMSD_Speed_60mph = 
                                sqrt(sum((RMS_Speed - 60 )^2)/length(RMS_Speed)), RMSD_Speed_Average = 
                                sqrt(sum((RMS_Speed - mean(RMS_Speed) )^2)/length(RMS_Speed)), RMSD_Speed_sd_check = 
                                sd(RMS_Speed), RMSD_ld=
                                sqrt(sum((RMS_ld - mean(RMS_ld) )^2)/length(RMS_ld)))


# Let's combine the rms speed, ld, and mean speed datasets now. 
AnovaDATA_Events_rms$mean_speed <- AnovaDATA_Events_Speed$meanspeed_response
# Let's convert it to the wide format so that we can run the ANOVA in SPSS
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events_rms, timevar = "SegmentType", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide_VIDEO1.csv")


#  VIDEO 1: EVENT TYPES FOR THREE METRICS:  RMS LANE DEV + RMS SPEED + MEAN SPEED
# Now let's do our three metrics as we did above for the different event types
AnovaDATA <- Aggregated_table_WithoutEvent5[ Aggregated_table_WithoutEvent5$Video_no. == 1 ,]
AnovaDATA_Events_Speed<-AnovaDATA[complete.cases(AnovaDATA$Events),]
AnovaDATA_Events_Speed <- ddply(AnovaDATA_Events_Speed, .(ParticipantID, Events, Gender, Status),summarise,meanspeed_response=mean(Speed))

AnovaDATA <- Aggregated_table_long_WithoutEvent5[ Aggregated_table_long_WithoutEvent5$Video_no. == 1 ,]
AnovaDATA_Events_rms<-AnovaDATA[complete.cases(AnovaDATA$Events),]
AnovaDATA_Events_rms <- ddply(AnovaDATA_Events_rms, .(ParticipantID, Events, Gender, Status),summarise,RMSD_Speed_60mph = 
                                sqrt(sum((RMS_Speed - 60 )^2)/length(RMS_Speed)), RMSD_Speed_Average = 
                                sqrt(sum((RMS_Speed - mean(RMS_Speed) )^2)/length(RMS_Speed)), RMSD_Speed_sd_check = 
                                sd(RMS_Speed), RMSD_ld=
                                sqrt(sum((RMS_ld - mean(RMS_ld) )^2)/length(RMS_ld)))
# note, we now have a whole bunch of NAS that we didn't have previously because for one video each participant only saw 4 events,
# since we removed event 5 for those confounding reasons associated with end of scenario.Let's figure out how to run
# a repeated measures ANOVA in SPSS just ignoring those missing values

AnovaDATA_Events_rms$mean_speed <- AnovaDATA_Events_Speed$meanspeed_response
AnovaDATA_Events_wide <- reshape(AnovaDATA_Events_rms, timevar = "Events", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_five_wide_VIDEO1.csv")

#  BASED ON HUNTER'S COMMENTS ON FIRST ROUND OF HIS REVIEW FOR TRB PAPER - LET'S SUMMARIZE BY M-CYCLE and NO-MYCLE ACROSS
# the FOUR PERFORMANCE METRICS : RMS LANE DEV + RMS SPEED + MEAN SPEED (remember we do mean speed a little differently, don't get confused)
# WE ARE USING THE LONG DATASET HERE AGAIN. 
AnovaDATA <- Aggregated_table_WithoutEvent5
AnovaDATA_MCYCLE_Speed <- ddply(AnovaDATA, .(ParticipantID, Motorcycle, Gender, Status),summarise,meanspeed_response=mean(Speed))

AnovaDATA <-Aggregated_table_long_WithoutEvent5
AnovaDATA_MCYCLE_rms <- ddply(AnovaDATA, .(ParticipantID, Motorcycle, Gender, Status),summarise,RMSD_Speed_60mph = 
                                sqrt(sum((RMS_Speed - 60 )^2)/length(RMS_Speed)), RMSD_Speed_Average = 
                                sqrt(sum((RMS_Speed - mean(RMS_Speed) )^2)/length(RMS_Speed)), RMSD_Speed_sd_check = 
                                sd(RMS_Speed), RMSD_ld=
                                sqrt(sum((RMS_ld - mean(RMS_ld) )^2)/length(RMS_ld)))

AnovaDATA_MCYCLE_rms$mean_speed <- AnovaDATA_MCYCLE_Speed$meanspeed_response
AnovaDATA_MYCYCLE_wide <- reshape(AnovaDATA_MCYCLE_rms, timevar = "Motorcycle", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_MYCYCLE_wide, "AnovaDATA_Mcycle_five_wide.csv")

# response latencies for mcycle and not
AnovaDATA <- Aggregated_table
AnovaDATA_Events<-AnovaDATA[complete.cases(AnovaDATA$Response_Latencies),]
AnovaDATA_Events <- ddply(AnovaDATA_Events, .(ParticipantID, Motorcycle, Gender, Status),summarise,mean_response=mean(Response_Latencies))

AnovaDATA_Events_wide <- reshape(AnovaDATA_Events, timevar = "Motorcycle", idvar = c("ParticipantID", "Gender", "Status"),
                                 direction = "wide")
write.csv(AnovaDATA_Events_wide, "AnovaDATA_Events_wide_ResponseLatencies_MCYCLE.csv")


