#Solo 1 Assignment Skeleton Code
#MSDS 450, Winter 2019

#Load Data
setwd("~/R/MSDS 450/Solo 1")

load("apphappyData.RData") #Load data
ls() #Load dataframe
## [1] "apphappy.3.labs.frame" "apphappy.3.num.frame" #

#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(proxy)
library(VIM) #Missingness Map
library(mice)
library(plyr) 
library(likert) #Visualize Likert Scale Data
require(ggplot2)
library(factoextra) #Density Clustering
library(ggpubr) #Density Clustering
library(dbscan) #Density Clustering
library(fpc) #Density Clustering
library(reshape)
library(NbClust) #Provides 30 indexes for determining the optimal number of clusters in a data set

# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


numdata.df <- apphappy.3.num.frame
labsdata <- apphappy.3.labs.frame 

###################################### Exploratory Data Analysis #####################################

#Descriptive Statistics
numdata.df$caseID <- as.numeric(numdata.df$caseID)

str(numdata.df) #1800 rows/people and 89 columns/variables/questions
head(numdata.df)
tail(numdata.df)
summary(numdata.df) 
dim(numdata.df)
describe(labsdata) #Obtain percentages (proportions) for questions 24 to 26. 
describe(numdata) 

#Check for Missingness
sapply(numdata.df, function(x) sum(is.na(x)))
sum(is.na(numdata.df))
aggr_plot <- aggr(numdata.df, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(numdata.df), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))
#NAs: q5r1, q12, and q57. '

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(numdata.df,2,pMiss)

#Run imputation
numdata.df <- mice(numdata.df,m=5,maxit=50,meth='pmm',seed=500)
summary(numdata.df)

#Check N/A values have been removed
numdata <- complete(numdata.df,1) #Create new data frame of imputed data. 
apply(numdata,2,pMiss)
summary(numdata)
sapply(numdata, function(x) sum(is.na(x)))
str(numdata)

#Respondent Demographics

#q1. Which of the following best describes your age? 
q1<-ggplot(labsdata) +
  geom_bar( aes(q1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q1. Age Range" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q1

#q48. Which of the following best describes the highest level of education you have attained? 
q48<-ggplot(labsdata) +
  geom_bar( aes(q48),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q48. Education" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q48


#q49. Which of the following best describe your marital status? 
q49<-ggplot(labsdata) +
  geom_bar( aes(q49),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q49. Marital Status" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q49

#q54. Which of the following best describes your race? 
q54<-ggplot(labsdata) +
  geom_bar( aes(q54),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q54. Race" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q54

#q55. Do you consider yourself to be of Hispanic or Latino ethnicity?
q55<-ggplot(labsdata) +
  geom_bar( aes(q55),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q55. Hispanic or Latino Ethnicity?" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q55


#q56. Which of the following best describes your household annual income before taxes?
q56<-ggplot(labsdata) +
  geom_bar( aes(q56),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q56. Household Annual Income Before Taxes" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q_56<-q56 + theme(axis.text.x = element_text(angle=45))

#q57. Please indicate your gender.
q57<-ggplot(numdata) +
  geom_bar( aes(q57),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q57. Gender" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
q57_annotate<-q57 + annotate("text", x = 1, y = 500, label = "Male",color="white") +
  annotate("text", x = 2, y = 500, label = "Female",color="white")
q57_annotate

#Mutiplots
multiplot(q1, q48, q49,q54,q55, q_56, q57_annotate,cols=2)

#Technology/App Usage

#q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

par(mfrow=c(1,2))
q2_MyMultResp_labels<-data.frame(numdata[3:11])

names(q2_MyMultResp_labels) <- make.names(names(q2_MyMultResp_labels)) 
colnames(q2_MyMultResp_labels)[1] <- "iPhone"       
colnames(q2_MyMultResp_labels)[2] <- "iPod touch"   
colnames(q2_MyMultResp_labels)[3] <- "Android"   
colnames(q2_MyMultResp_labels)[4] <- "BlackBerry"   
colnames(q2_MyMultResp_labels)[5] <- "Nokia"   
colnames(q2_MyMultResp_labels)[6] <- "Windows"   
colnames(q2_MyMultResp_labels)[7] <- "HP"   
colnames(q2_MyMultResp_labels)[8] <- "Tablet"   
colnames(q2_MyMultResp_labels)[9] <- "Other"   

str(q2_MyMultResp_labels)

q2_MyMultResp<-data.frame(Freq = colSums(q2_MyMultResp_labels[1:9]),
               Pct.of.all.Resp = (colSums(q2_MyMultResp_labels[1:9])/sum(q2_MyMultResp_labels[1:9]))*100,
               Pct.Check.All.That.Apply = (colSums(q2_MyMultResp_labels[1:9])/nrow(q2_MyMultResp_labels[1:9]))*100)
q2_MyMultResp


barplot(q2_MyMultResp[[3]]
        ,names.arg=row.names(q2_MyMultResp)
        ,main = "q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.8)

#q4. Do you use any of the following kinds of Apps? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

q4_MyMultResp_labels<-data.frame(numdata[13:23])

names(q4_MyMultResp_labels) <- make.names(names(q4_MyMultResp_labels)) 
colnames(q4_MyMultResp_labels)[1] <- "Music"       
colnames(q4_MyMultResp_labels)[2] <- "TV"   
colnames(q4_MyMultResp_labels)[3] <- "Ent."   
colnames(q4_MyMultResp_labels)[4] <- "TV Show"   
colnames(q4_MyMultResp_labels)[5] <- "Gaming"   
colnames(q4_MyMultResp_labels)[6] <- "Social"   
colnames(q4_MyMultResp_labels)[7] <- "Gen.News"   
colnames(q4_MyMultResp_labels)[8] <- "Shop"   
colnames(q4_MyMultResp_labels)[9] <- "Pub.News"   
colnames(q4_MyMultResp_labels)[10] <- "Other"  
colnames(q4_MyMultResp_labels)[11] <- "None"

str(q4_MyMultResp_labels)

q4_MyMultResp<-data.frame(Freq = colSums(q4_MyMultResp_labels[1:11]),
                          Pct.of.all.Resp = (colSums(q4_MyMultResp_labels[1:11])/sum(q4_MyMultResp_labels[1:11]))*100,
                          Pct.Check.All.That.Apply = (colSums(q4_MyMultResp_labels[1:11])/nrow(q4_MyMultResp_labels[1:11]))*100)
q4_MyMultResp

barplot(q4_MyMultResp[[3]]
        ,names.arg=row.names(q4_MyMultResp)
        ,main = "q4. Do you use any of the following kinds of Apps? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.7)

par(mfrow=c(1,1))

#q11. How many Apps do you have on your smartphone/iPod Touch/Tablet? If you have more than of
#these device, please tell us the total number of Apps. 
q11<-ggplot(labsdata) +
  geom_bar( aes(q11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q11. Number of Apps on smartphone/iPod Touch/Tablet" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q11

#q12. Of your Apps, what percent were free to download? 
q12<-ggplot(numdata) +
  geom_bar(aes(q12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q12. Percent free to download (Apps)" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
q12_annotate<-q12_annotate<-q12 + annotate("text", x = 1, y = 15, label = "None", color="white")+ 
  annotate("text", x = 2, y = 190, label = "1% - 25%",color="white")+ 
  annotate("text", x = 3, y = 290, label = "26% - 50%",color="white")+ 
  annotate("text", x = 4, y = 400, label = "51% - 75",color="white")+ 
  annotate("text", x = 5, y = 450, label = "76% - 99",color="white")+ 
  annotate("text", x = 6, y = 365, label = "All",color="white")
q12_annotate

multiplot(q11,q12_annotate, cols=1)

#q13. How many times per week do you visit each of the following websites? (Select all that apply)
#Facebook
q13r1<-ggplot(labsdata) +
  geom_bar(aes(q13r1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r1. How many times per week do you visit: Facebook" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r1

#Twitter
q13r2<-ggplot(labsdata) +
  geom_bar(aes(q13r2),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r2. How many times per week do you visit: Twitter" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r2

#Myspace
q13r3<-ggplot(labsdata) +
  geom_bar(aes(q13r3),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r3. How many times per week do you visit: Myspace" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r3

#Pandora Radio
q13r4<-ggplot(labsdata) +
  geom_bar(aes(q13r4),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r4. How many times per week do you visit: Pandora Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r4

#Vevo
q13r5<-ggplot(labsdata) +
  geom_bar(aes(q13r5),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r5. How many times per week do you visit: Vevo" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r5

#YouTube
q13r6<-ggplot(labsdata) +
  geom_bar(aes(q13r6),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r6. How many times per week do you visit: YouTube" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r6

#AOL Radio
q13r7<-ggplot(labsdata) +
  geom_bar(aes(q13r7),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r7. How many times per week do you visit: AOL Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r7

#Last.fm
q13r8<-ggplot(labsdata) +
  geom_bar(aes(q13r8),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r8. How many times per week do you visit: Last.fm" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r8

#Yahoo Entertainment and Music
q13r9<-ggplot(labsdata) +
  geom_bar(aes(q13r9),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r9. How many times per week do you visit: Yahoo Ent. and Music" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r9

#IMDB
q13r10<-ggplot(labsdata) +
  geom_bar(aes(q13r10),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r10. How many times per week do you visit: IMDB" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r10

#LinkedIn
q13r11<-ggplot(labsdata) +
  geom_bar(aes(q13r11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r11. How many times per week do you visit: LinkedIn" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r11

#Netflix
q13r12<-ggplot(labsdata) +
  geom_bar(aes(q13r12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r12. How many times per week do you visit: Netflix" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r12

multiplot(q13r1, q13r2, q13r3,q13r4, q13r5, q13r6,q13r7, q13r8, q13r9,q13r10, q13r11, q13r12, cols=3)

#Age Range vs. iPhone
a=table(numdata$q1,numdata$q2r1)
a
barplot(a, main = "Age Range vs. iPhone")

#Age Range vs. Android
b=table(numdata$q1,numdata$q2r3)
b
barplot(b, main = "Age Range vs. Android")

#Age Range vs. Music
c=table(numdata$q1,numdata$q4r1)
c
barplot(c, main = "Age Range vs. Music")

#Age Range vs. Gaming
d=table(numdata$q1,numdata$q4r5)
d
barplot(d, main = "Age Range vs. Gaming")

#Age Range vs. Social
e=table(numdata$q1,numdata$q4r6)
e
barplot(e, main = "Age Range vs. Social")

#Age Range vs. Entertainment
f=table(numdata$q1,numdata$q4r3)
f
barplot(f, main = "Age Range vs. Entertainment")

######

#Income vs. iPhone
a=table(numdata$q56,numdata$q2r1)
a
barplot(a, main = "Income vs. iPhone")

#Income vs. Android
b=table(numdata$q56,numdata$q2r3)
b
barplot(b, main = "Income vs. Android")

#Income vs. Music
c=table(numdata$q56,numdata$q4r1)
c
barplot(c, main = "Income vs. Music")

#Income vs. Gaming
d=table(numdata$q56,numdata$q4r5)
d
barplot(d, main = "Income vs. Gaming")

#Income vs. Social
e=table(numdata$q56,numdata$q4r6)
e
barplot(e, main = "Income vs. Social")

#Income vs. Entertainment
f=table(numdata$q56,numdata$q4r3)
f
barplot(f, main = "Income vs. Entertainment")

######

#Gender vs. iPhone
a=table(numdata$q57,numdata$q2r1)
a
barplot(a, main = "Gender vs. iPhone")

#Gender vs. Android
b=table(numdata$q57,numdata$q2r3)
b
barplot(b, main = "Gender vs. Android")

#Gender vs. Music
c=table(numdata$q57,numdata$q4r1)
c
barplot(c, main = "Gender vs. Music")

#Gender vs. Gaming
d=table(numdata$q57,numdata$q4r5)
d
barplot(d, main = "Gender vs. Gaming")

#Gender vs. Social
e=table(numdata$q57,numdata$q4r6)
e
barplot(e, main = "Gender vs. Social")
       
#Gender vs. Entertainment
f=table(numdata$q57,numdata$q4r3)
f
barplot(f, main = "Gender vs. Entertainment")

######

#Education vs. iPhone
a=table(numdata$q48,numdata$q2r1)
a
barplot(a, main = "Education vs. iPhone")

#Education vs. Android
b=table(numdata$q48,numdata$q2r3)
b
barplot(b, main = "Education vs. Android")

#Education vs. Music
c=table(numdata$q48,numdata$q4r1)
c
barplot(c, main = "Education vs. Music")

#Education vs. Gaming
d=table(numdata$q48,numdata$q4r5)
d
barplot(d, main = "Education vs. Gaming")

#Education vs. Social
e=table(numdata$q48,numdata$q4r6)
e
barplot(e, main = "Education vs. Social")

#Education vs. Entertainment
f=table(numdata$q48,numdata$q4r3)
f
barplot(f, main = "Education vs. Entertainment")

######

#Ethnicity vs. iPhone
a=table(numdata$q54,numdata$q2r1)
a
barplot(a, main = "Ethnicity vs. iPhone")

#Ethnicity vs. Android
b=table(numdata$q54,numdata$q2r3)
b
barplot(b, main = "Ethnicity vs. Android")

#Ethnicity vs. Music
c=table(numdata$q54,numdata$q4r1)
c
barplot(c, main = "Ethnicity vs. Music")

#Ethnicity vs. Gaming
d=table(numdata$q54,numdata$q4r5)
d
barplot(d, main = "Ethnicity vs. Gaming")

#Ethnicity vs. Social
e=table(numdata$q54,numdata$q4r6)
e
barplot(e, main = "Ethnicity vs. Social")

#Ethnicity vs. Entertainment
f=table(numdata$q54,numdata$q4r3)
f
barplot(f, main = "Ethnicity vs. Entertainment")

###################################### Create subsets #####################################

#Subset of questions 24 to 26 (attitudinal questions) for numdata
numsub <- subset(numdata, select=c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9","q24r10","q24r11",
                                             "q24r12","q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9","q25r10","q25r11",
                                             "q25r12","q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11","q26r12",
                                             "q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

str(numsub) #1800 observations and 40 variables
summary(numsub) #Likert scale, min is 1 and max is 6. 


#Subset of questions 24 to 26 (attitudinal questions) for labsdata
labsub <- subset(labsdata, select=c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9","q24r10","q24r11",
                                   "q24r12","q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9","q25r10","q25r11",
                                   "q25r12","q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11","q26r12",
                                   "q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

str(labsub) #1800 observations and 40 variables
summary(labsub) #Likert scale, min is 1 and max is 6. 

#Questions 24 to 26 (Visualizing Likert Scale Data)
library(likert)

likert(labsub)
Result = likert(labsub)
plot(Result,type="bar")

### Correlation Matrix of subset ###

require(corrplot)
numsubcorrelation <- cor(numsub)

##Correlation Plot 3 w/ Numbers
corrplot(numsubcorrelation, method="shade", addCoef.col="black", 
addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
tl.srt=45,number.cex = 0.6,tl.cex = 0.6, addcolorlabel="no", order="AOE",insig = "p-value")

### PCA Plots ###

pca <-princomp(numsub)
plot(pca$scores[,1],pca$scores[,2]) #First 2 principal components only explain 0.3614 of the variation in the data. 

names(pca)
str(pca)
summary(pca)
head(pca$scores)

#Find outliers where respondents selected all 1's
sortapca<-sort(pca$scores[,1], decreasing = TRUE)
sortapca
head(sortapca)

numsub["431",]
numsub["2176",]
numsub["1083",]
numsub["230",]
numsub["1870",]
numsub["1185",]

#Find outliers where respondents selected all 5's or 6's
sortapca2<-sort(pca$scores[,1], decreasing = FALSE)
sortapca2
head(sortapca2)

numsub["243",]
numsub["2391",]
numsub["858",]

#Remove outliers? #No
#numsub <- numsub[-c(27, 111, 156, 224, 259, 287, 380, 545, 
                   # 625, 647, 728, 960, 1046, 1122, 1153, 1224, 1227, 1315, 1336, 1359, 
                    #1478, 1534, 1573, 1597, 1791, 1466, 1001, 141, 1419, 844, 1614, 1035, 1282, 247, 801,
                    #1040), ]

#############################################################
##create 'derived' variables - means of similar variables ###
#############################################################

attach(numsub)
numsub$q24a <- (q24r1+q24r2+q24r3+q24r5+q24r6)/5
numsub$q24b <- (q24r7+q24r8)/2
numsub$q24c <- (q24r10+q24r11)/2
numsub$q24d <- (q24r4+q24r9+q24r12)/3

numsub$q25a <- (q25r1+q25r2+q25r3+q25r4+q25r5)/5
numsub$q25b <- (q25r7+q25r8)/2
numsub$q25c <- (q25r9+q25r10+q25r11)/3
numsub$q25d <- (q25r6+q25r12)/2

numsub$q26a <- (q26r3+q26r4+q26r5+q26r6+q26r7)/5
numsub$q26b <- (q26r8+q26r9+q26r10)/3
numsub$q26c <- q26r11
numsub$q26d <- (q26r12+q26r13+q26r14)/3
numsub$q26e <- (q26r15+q26r16+q26r17+q26r18)/4

numsub2 <- subset(numsub, select=
                    c("q24a","q24b","q24c",
                      "q25a","q25b","q25c",
                      "q26a","q26b","q26d","q26e"))

pca <-princomp(numsub2)
plot(pca$scores[,1],pca$scores[,2])
names(pca)
head(pca$scores)
str(pca$scores)
summary(pca)

##Correlation Plot 3 w/ Numbers
require(corrplot)
mcor <- cor(numsub2)
corrplot(mcor, method="shade", addCoef.col="black", 
         addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
         tl.srt=45,number.cex = 0.9,tl.cex = 0.9, addcolorlabel="no", order="AOE",insig = "p-value")

###################################################################
############### Kmeans Cluster ###################
###################################################################

#Create a 'scree' plot to determine the num of clusters
#'Sweep' through 1 to 15 clusters (standard, see slide 22)
wssplot <- function(numsub2, nc=15, seed=1234) {
  wss <- (nrow(numsub2)-1)*sum(apply(numsub2,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub2, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub2) #Elbow at 2, although no clear elbow. Try 5 for benchmark

# Elbow method (alternative); #intercept specifies elbow
fviz_nbclust(numsub2, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method #Recommends 2
fviz_nbclust(numsub2, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
#set.seed(123)
#fviz_nbclust(numsub2, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
 # labs(subtitle = "Gap statistic method")

#NbClust: Determining the Best Number of Clusters in a Data Set (#Recommends 2)
#It provides 30 indexes for determining the optimal number of clusters in a data set and offers the best clustering scheme from different results to the user.
NbClust(data = numsub2, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

### k-means with 5 clusters ###
clusterresults_5 <- kmeans(numsub2,5) #k-means
names(clusterresults_5) #sub objects of this result file and can access these with $
clusterresults_5$withinss #withinss for each of the clusters (e.g., sitting in cluster centroid, distance with all the people computing)
clusterresults_5$tot.withinss #Total withinss for the clusters
clusterresults_5$totss #Total withins for 1 cluster
clusterresults_5$betweenss #total withins-totss
clusterresults_5$size #Gives the count of people that are sitting in each cluster
rsquare <- clusterresults_5$betweenss/clusterresults_5$totss
rsquare #r-squared: 0.5128982

### Create a PC (Principal Componenet plot) ###

plot(clusterresults_5, data=numsub2) #PCA analysis, plot PC on x and y axis and then will plot the clusters
clusterresults_5$centers 
head(clusterresults_5$cluster) #Shows cluster membership

### Create a Silhouette Plot ###
dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults_5$cluster, dE2)
str(sk2)
plot(sk2) #Average Silhouette: 0.25

### k-means with 3 clusters ###
clusterresults_3 <- kmeans(numsub2,3) #k-means
names(clusterresults_3) #sub objects of this result file and can access these with $
clusterresults_3$withinss #withinss for each of the clusters (e.g., sitting in cluster centroid, distance with all the people computing)
clusterresults_3$tot.withinss #Total withinss for the clusters
clusterresults_3$totss #Total withins for 1 cluster
clusterresults_3$betweenss #total withins-totss
clusterresults_3$size #Gives the count of people that are sitting in each cluster
rsquare <- clusterresults_3$betweenss/clusterresults_3$totss
rsquare #r-squared: 0.4346976

### Create a PC (Principal Componenet plot) ###

plot(clusterresults_3, data=numsub2) #PCA analysis, plot PC on x and y axis and then will plot the clusters
clusterresults_3$centers 
head(clusterresults_3$cluster) #Shows cluster membership

### Create a Silhouette Plot ###
dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults_3$cluster, dE2)
str(sk2)
plot(sk2) #Average Silhouette: 0.32

### k-means with 2 clusters ###
clusterresults <- kmeans(numsub2,2) #k-means
names(clusterresults) #sub objects of this result file and can access these with $
clusterresults$withinss #withinss for each of the clusters (e.g., sitting in cluster centroid, distance with all the people computing)
clusterresults$tot.withinss #Total withinss for the clusters
clusterresults$totss #Total withins for 1 cluster
clusterresults$betweenss #total withins-totss
clusterresults$size #Gives the count of people that are sitting in each cluster
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare #r-squared: 0.34039

### Create a PC (Principal Componenet plot) ###

plot(clusterresults, data=numsub2) #PCA analysis, plot PC on x and y axis and then will plot the clusters
clusterresults$centers 
head(clusterresults$cluster) #Shows cluster membership

### Create a Silhouette Plot ###
dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2) #Average Silhouette: 0.45, #Best

#Values near one mean that the observation is well placed in its cluster; 
#values near 0 mean that it's likely that an observation might really belong in some other cluster. 
#Within each cluster, the value for this measure is displayed from smallest to largest. 
#If the silhouette plot shows values close to one for each observation, the fit was good; 
#if there are many observations closer to zero, it's an indication that the fit was not good. 
#The sihouette plot is very useful in locating groups in a cluster analysis that may not be doing a good job; 
#in turn this information can be used to help select the proper number of clusters. 
#If many points have a low or negative value, then the clustering configuration may have too many or too few clusters.
#1 = Perfect, 0 is on the wall, and -1 is completely wrong. 
#Good news, it's on positive side, more than 0 which is good. 

### Produce csv Files ###
newdf <- as.data.frame(clusterresults$cluster) #creates dataframe for k-means cluster
write.csv(newdf, file = "clusterresults.csv") #cluster results assigned to each respondent
write.csv(numsub2, file = "numsub2.csv") #File of responses for subset of questions

###################################################################
################### Hierarchical Clustering #######################
###################################################################

### Single  Linkage ###
numsub2.dist = dist(numsub2) #Computes euclidean distances
require(maptree)
hclustmodel <- hclust(dist(numsub2), method = 'single') 

#Create Cluster Dendrogram 
plot(hclustmodel) 

cut.2 <- cutree(hclustmodel, k=2) 

clusterresults$centers 
head(clusterresults$cluster) #Cluster Membership 

#Create Cluster Dendrogram Post Cut
rect.hclust(hclustmodel,k=2, border="red") 

#Create a Silhouette Plot
plot(silhouette(cut.2,numsub2.dist)) #Average Silhouette: 0.57, but extremely imbalanced. Not Recommended. 
head(cut.2) #Classifys each respondent into a cluster. 

### Average  Linkage ###
numsub2.dist = dist(numsub2) #Computes euclidean distances
require(maptree)
hclustmodel <- hclust(dist(numsub2), method = 'average') 

#Create Cluster Dendrogram 
plot(hclustmodel) 

cut.2 <- cutree(hclustmodel, k=2) 

clusterresults$centers 
head(clusterresults$cluster) #Cluster Membership 

#Create Cluster Dendrogram Post Cut
rect.hclust(hclustmodel,k=2, border="red") 

#Create a Silhouette Plot
plot(silhouette(cut.2,numsub2.dist)) #Average Silhouette: 0.44, but extremely skewed and imbalanced. Not Recommended. 
head(cut.2) #Classifys each respondent into a cluster. 

### Complete Linkage ###
numsub2.dist = dist(numsub2) #Computes euclidean distances
require(maptree)
hclustmodel <- hclust(dist(numsub2), method = 'complete') 

#Create Cluster Dendrogram 
plot(hclustmodel) 

cut.2 <- cutree(hclustmodel, k=2) 

clusterresults$centers 
head(clusterresults$cluster) #Cluster Membership 

#Create Cluster Dendrogram Post Cut
rect.hclust(hclustmodel,k=2, border="red") 

#Create a Silhouette Plot
plot(silhouette(cut.2,numsub2.dist)) #Average Silhouette: 0.20, somewhat imbalanced 2 to 1. Recommendation: K-means
head(cut.2) #Classifys each respondent into a cluster. 

### ward.D2 ###
numsub2.dist = dist(numsub2) #Computes euclidean distances
require(maptree)
hclustmodel <- hclust(dist(numsub2), method = 'ward.D2') 

#Create Cluster Dendrogram 
plot(hclustmodel) 

cut.2 <- cutree(hclustmodel, k=2) 

clusterresults$centers 
head(clusterresults$cluster) #Cluster Membership 

#Create Cluster Dendrogram Post Cut
rect.hclust(hclustmodel,k=2, border="red") 

#Create a Silhouette Plot
plot(silhouette(cut.2,numsub2.dist)) #Average Silhouette: 0.24, somewhat imbalanced. Recommendation: K-means
head(cut.2) #Classifys each respondent into a cluster. 

### Produce csv Files ###
write.csv(cut.2, file = "cut2results.csv")

#Between Sum of Squares and Total Sum of Squares 
require(proxy)
numsub2mat <- as.matrix(numsub2)
overallmean <- matrix(apply(numsub2mat,2,mean),nrow=1)
overallmean
TSS <- sum(dist(numsub2mat,overallmean)^2)
TSS #Compute TSS 17010.06

#Weighted Sum Statistic
combcutdata <- cbind(numsub2,cut.2)
head(combcutdata)

require(reshape)
combcutdata <- rename(combcutdata, c(cut.2="cluster"))
head(combcutdata) 

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("q24a","q24b","q24c","q25a","q25b","q25c",
                                  "q26a","q26b","q26d","q26e"))
clust1 <- as.matrix(clust1,rowby=T)
dim(clust1)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dim(clust1mean)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("q24a","q24b","q24c","q25a","q25b","q25c",
                                  "q26a","q26b","q26d","q26e"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

WSS <- sum(dis1,dis2)
WSS #12078.64

BSS <- TSS - WSS
BSS #4931.425

### calculating the % of Between SS/ Total SS ###
rsquare <- BSS/TSS
rsquare #0.2899122, compared to k-mean's rsquare of: 0.34039; Note: Use r-square and silouette to compare different methods. 

######################################################
################### PAM Method #######################
######################################################

my.k.choices <- 2:8 #Sweep through using average silhouette width (k-means uses WSS)
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(numsub2, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width)) #Optimal number is 2, given average silo of 0.1399

clusterresultsPAM <-pam(numsub2,2)
summary(clusterresultsPAM)

#Create cluster plot of PAM
plot(clusterresultsPAM, which.plots=1) #Different symbols denote various clusters; overlap

#Create a Silhouette Plot
plot(clusterresultsPAM, which.plots=2) #Average Silhouette: 0.27
#Cluster sizes somewhat imbalanced.
#PAM is better than hierarchical, but k-means is still better in terms of average silouette.

######################################################
######## Density based clustering from Lecture ###
######################################################

## Find optimal values of 2 parameters epsilon, minpts, knee of the scree plot
dbscan::kNNdistplot(numsub2, k =  5)
abline(h = 2.5, lty = 2)

# Compute DBSCAN (package:fpc)
set.seed(123)
db <- fpc::dbscan(numsub2, eps = 2.5, MinPts = 5) #Use MinPts of 5,6, or 7

# Plot DBSCAN results (package:factoextra)
fviz_cluster(db, data = numsub2, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "light blue", ggtheme = theme_classic())

print(db) ## 0 means outliers, other values belong to cluster

# Cluster membership. Noise/outlier observations are coded as 0
# A random subset is shown
db$cluster[sample(1:1089, 20)]

#Note:
#Showing how to do this model or mix-model for real-life, but for Solo 1 it's not appropriate.
#As a result, instead, focus on k-means, hierarchical, and PAM. Gives intro idea EDA and clustering.
#Deep dive, take this as a starting point, build upon, and work on it, and then Wednesday for deep
#dive for Solo 1. 

###################
## Model based clustering
##################
library(mclust)
mclust_2 <- Mclust(numsub2,2)
plot(fit,data=numsub2, what="density") # plot results
#plot(mclust_2,data=numsub2, what="BIC") # plot results

summary(mclust_2) # display the best model

dev.off()
dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(mclust_2$classification, dE2)
str(sk2)
plot(sk2)

#Not appropriate given likert scale data. 

###############################################
## Comparison of cluster results  #############
###############################################
##corrected or adjusted rand index lies between 0 & 1
## perfect match between 2 clustering methods means 1, no match means 0
## any number in between represents 'kind of' % of matched pairs 

#k-means vs. pam
clstat <- cluster.stats(numsub2.dist, clusterresults$cluster, clusterresultsPAM$cluster)
names(clstat)
clstat$corrected.rand #0.6701434

#Hierarchical vs. k-means
clstat <- cluster.stats(numsub2.dist, clusterresults$cluster, cut.2)
clstat$corrected.rand #0.500193

#Hierarchical vs. pam
clstat <- cluster.stats(numsub2.dist, clusterresultsPAM$cluster, cut.2)
clstat$corrected.rand #0.601595

####################################### Profiling #######################################################
#Create a dataset that combines original data with cluster information, used to create profiles
newdf <- read.csv("clusterresults.csv") #File that contains cluster results assigned to each respondent

#Demographics: Age Range (q1), Education (q48), Marital status (q49), Race (q54), Ethnicity(q55), Income(q56), Gender(q57)
combdata <- cbind(numsub2 ,newdf,numdata$q1,
                  numdata$q48,
                  numdata$q49,
                  numdata$q54,
                  numdata$q55,
                  numdata$q56,
                  numdata$q57)
head(combdata)
require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
aggregate(combdata,by=list(byvar=combdata$cluster), median) #For each cluster, show median response for the subset of q's

#Technology/App Usage
combdata_consump <- cbind(numsub2 ,newdf,numdata$q11,numdata$q12)
head(combdata_consump)
require(reshape)
combdata_consump <- rename(combdata_consump, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
aggregate(combdata_consump,by=list(byvar=combdata_consump$cluster), median) #For each cluster, show median response for the subset of q's

#By looking at the profile, you will be able to decide what kind of products or services you would want to provide
#for each of these segments. 

####################################### Segment 1 #######################################################

#Subsetting on Segment 1 (numdata)
combdata_seg <- cbind(newdf,numdata)
combdata_seg <- rename(combdata_seg, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment1<-combdata_seg[combdata_seg$cluster ==1, ]
str(combdata_segment1)

#Subsetting on Segment 1 (labsdata)
combdata_seg_labs <- cbind(newdf,labsdata)
combdata_seg_labs <- rename(combdata_seg_labs, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment1_labs<-combdata_seg_labs[combdata_seg_labs$cluster ==1, ]
str(combdata_segment1_labs)

#Subsetting on Segment 1 (labsdata) Survey
combdata_seg_labs_survey <- cbind(labsub,newdf)
combdata_seg_labs_survey <- rename(combdata_seg_labs_survey, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_seg_labs_survey<-combdata_seg_labs_survey[combdata_seg_labs_survey$cluster ==1, ]
combdata_seg_labs_survey <-data.frame(combdata_seg_labs_survey[1:40])
str(combdata_seg_labs_survey)

#Respondent Demographics

#q1. Which of the following best describes your age? 
q1<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q1. Age Range" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q1

#q48. Which of the following best describes the highest level of education you have attained? 
q48<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q48),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q48. Education" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q48

#q49. Which of the following best describe your marital status? 
q49<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q49),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q49. Marital Status" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q49

#q54. Which of the following best describes your race? 
q54<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q54),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q54. Race" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q54

#q55. Do you consider yourself to be of Hispanic or Latino ethnicity?
q55<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q55),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q55. Hispanic or Latino Ethnicity?" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q55

#q56. Which of the following best describes your household annual income before taxes?
q56<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q56),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q56. Household Annual Income Before Taxes" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q_56<-q56 + theme(axis.text.x = element_text(angle=45))
q_56

#q57. Please indicate your gender.
q57<-ggplot(combdata_segment1) +
  geom_bar( aes(q57),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q57. Gender" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
q57_annotate<-q57 + annotate("text", x = 1, y = 300, label = "Male",color="white") +
  annotate("text", x = 2, y = 300, label = "Female",color="white")
q57_annotate

#Mutiplots
multiplot(q1, q48, q49,q54,q55, q_56, q57_annotate,cols=2)

#Technology/App Usage

par(mfrow=c(1,2))
#q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

q2_MyMultResp_labels<-data.frame(combdata_segment1[5:14])

names(q2_MyMultResp_labels) <- make.names(names(q2_MyMultResp_labels)) 
colnames(q2_MyMultResp_labels)[1] <- "iPhone"       
colnames(q2_MyMultResp_labels)[2] <- "iPod touch"   
colnames(q2_MyMultResp_labels)[3] <- "Android"   
colnames(q2_MyMultResp_labels)[4] <- "BlackBerry"   
colnames(q2_MyMultResp_labels)[5] <- "Nokia"   
colnames(q2_MyMultResp_labels)[6] <- "Windows"   
colnames(q2_MyMultResp_labels)[7] <- "HP"   
colnames(q2_MyMultResp_labels)[8] <- "Tablet"   
colnames(q2_MyMultResp_labels)[9] <- "Other"   

str(q2_MyMultResp_labels)

q2_MyMultResp<-data.frame(Freq = colSums(q2_MyMultResp_labels[1:9]),
                          Pct.of.all.Resp = (colSums(q2_MyMultResp_labels[1:9])/sum(q2_MyMultResp_labels[1:9]))*100,
                          Pct.Check.All.That.Apply = (colSums(q2_MyMultResp_labels[1:9])/nrow(q2_MyMultResp_labels[1:9]))*100)
q2_MyMultResp

barplot(q2_MyMultResp[[3]]
        ,names.arg=row.names(q2_MyMultResp)
        ,main = "q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.8)

#q4. Do you use any of the following kinds of Apps? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

q4_MyMultResp_labels<-data.frame(combdata_segment1[16:26])

names(q4_MyMultResp_labels) <- make.names(names(q4_MyMultResp_labels)) 
colnames(q4_MyMultResp_labels)[1] <- "Music"       
colnames(q4_MyMultResp_labels)[2] <- "TV"   
colnames(q4_MyMultResp_labels)[3] <- "Ent."   
colnames(q4_MyMultResp_labels)[4] <- "TV Show"   
colnames(q4_MyMultResp_labels)[5] <- "Gaming"   
colnames(q4_MyMultResp_labels)[6] <- "Social"   
colnames(q4_MyMultResp_labels)[7] <- "Gen.News"   
colnames(q4_MyMultResp_labels)[8] <- "Shop"   
colnames(q4_MyMultResp_labels)[9] <- "Pub.News"   
colnames(q4_MyMultResp_labels)[10] <- "Other"  
colnames(q4_MyMultResp_labels)[11] <- "None"

str(q4_MyMultResp_labels)

q4_MyMultResp<-data.frame(Freq = colSums(q4_MyMultResp_labels[1:11]),
                          Pct.of.all.Resp = (colSums(q4_MyMultResp_labels[1:11])/sum(q4_MyMultResp_labels[1:11]))*100,
                          Pct.Check.All.That.Apply = (colSums(q4_MyMultResp_labels[1:11])/nrow(q4_MyMultResp_labels[1:11]))*100)
q4_MyMultResp

barplot(q4_MyMultResp[[3]]
        ,names.arg=row.names(q4_MyMultResp)
        ,main = "q4. Do you use any of the following kinds of Apps? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.7)

par(mfrow=c(1,1))

#q11. How many Apps do you have on your smartphone/iPod Touch/Tablet? If you have more than of
#these device, please tell us the total number of Apps. 
q11<-ggplot(combdata_segment1_labs) +
  geom_bar( aes(q11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q11. Number of Apps on smartphone/iPod Touch/Tablet" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q11

#q12. Of your Apps, what percent were free to download? 
q12<-ggplot(combdata_segment1) +
  geom_bar(aes(q12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q12. Percent free to download (Apps)" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
q12_annotate<-q12 + annotate("text", x = 1, y = 15, label = "None", color="white")+ 
  annotate("text", x = 2, y = 90, label = "1% - 25%",color="white")+ 
  annotate("text", x = 3, y = 140, label = "26% - 50%",color="white")+ 
  annotate("text", x = 4, y = 190, label = "51% - 75",color="white")+ 
  annotate("text", x = 5, y = 300, label = "76% - 99",color="white")+ 
  annotate("text", x = 6, y = 240, label = "All",color="white")
q12_annotate

multiplot(q11,q12_annotate, cols=1)

#q13. How many times per week do you visit each of the following websites? (Select all that apply)
#Facebook
q13r1<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r1. Facebook" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r1

#Twitter
q13r2<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r2),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r2. Twitter" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r2

#Myspace
q13r3<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r3),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r3. Myspace" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r3

#Pandora Radio
q13r4<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r4),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r4. Pandora Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r4

#Vevo
q13r5<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r5),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r5. Vevo" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r5

#YouTube
q13r6<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r6),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r6. YouTube" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r6

#AOL Radio
q13r7<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r7),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r7. AOL Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r7

#Last.fm
q13r8<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r8),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r8. Last.fm" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r8

#Yahoo Entertainment and Music
q13r9<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r9),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r9. Yahoo Ent. and Music" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r9

#IMDB
q13r10<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r10),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r10. IMDB" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r10

#LinkedIn
q13r11<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r11. LinkedIn" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r11

#Netflix
q13r12<-ggplot(combdata_segment1_labs) +
  geom_bar(aes(q13r12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r12. Netflix" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r12

multiplot(q13r1, q13r2, q13r3,q13r4, q13r5, q13r6,q13r7, q13r8, q13r9,q13r10, q13r11, q13r12, cols=3)

#Questions 24 to 26 (Visualizing Likert Scale Data)
library(likert)
likert(combdata_seg_labs_survey)
Result = likert(combdata_seg_labs_survey)
plot(Result,type="bar")

####################################### Segment 2 #######################################################

#Subsetting on Segment 2 (numdata)
combdata_seg <- cbind(newdf,numdata)
combdata_seg <- rename(combdata_seg, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment2<-combdata_seg[combdata_seg$cluster ==2, ]
str(combdata_segment2)

#Subsetting on Segment 2 (labsdata)
combdata_seg_labs <- cbind(newdf,labsdata)
combdata_seg_labs <- rename(combdata_seg_labs, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_segment2_labs<-combdata_seg_labs[combdata_seg_labs$cluster ==2, ]
str(combdata_segment2_labs)

#Subsetting on Segment 2 (labsdata) Survey
combdata_seg_labs_survey2 <- cbind(labsub,newdf)
combdata_seg_labs_survey2 <- rename(combdata_seg_labs_survey2, c(clusterresults.cluster="cluster")) #rename clusterresults.cluster to cluster
combdata_seg_labs_survey2<-combdata_seg_labs_survey2[combdata_seg_labs_survey2$cluster ==2, ]
combdata_seg_labs_survey2 <-data.frame(combdata_seg_labs_survey2[1:40])
str(combdata_seg_labs_survey2)

#Respondent Demographics

#q1. Which of the following best describes your age? 
q1<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q1. Age Range" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q1

#q48. Which of the following best describes the highest level of education you have attained? 
q48<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q48),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q48. Education" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q48

#q49. Which of the following best describe your marital status? 
q49<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q49),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q49. Marital Status" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q49

#q54. Which of the following best describes your race? 
q54<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q54),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q54. Race" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q54

#q55. Do you consider yourself to be of Hispanic or Latino ethnicity?
q55<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q55),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q55. Hispanic or Latino Ethnicity?" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q55

#q56. Which of the following best describes your household annual income before taxes?
q56<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q56),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q56. Household Annual Income Before Taxes" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q_56<-q56 + theme(axis.text.x = element_text(angle=45))

#q57. Please indicate your gender.
q57<-ggplot(combdata_segment2) +
  geom_bar( aes(q57),colour="#2b8cbe",fill="#2b8cbe" ) +
  ggtitle("q57. Gender" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
q57_annotate<-q57 + annotate("text", x = 1, y = 300, label = "Male",color="white") +
  annotate("text", x = 2, y = 300, label = "Female",color="white")
q57_annotate

#Mutiplots
multiplot(q1, q48, q49,q54,q55, q_56, q57_annotate,cols=2)

par(mfrow=c(1,2))
#Technology/App Usage

#q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

q2_MyMultResp_labels<-data.frame(combdata_segment2[5:14])

names(q2_MyMultResp_labels) <- make.names(names(q2_MyMultResp_labels)) 
colnames(q2_MyMultResp_labels)[1] <- "iPhone"       
colnames(q2_MyMultResp_labels)[2] <- "iPod touch"   
colnames(q2_MyMultResp_labels)[3] <- "Android"   
colnames(q2_MyMultResp_labels)[4] <- "BlackBerry"   
colnames(q2_MyMultResp_labels)[5] <- "Nokia"   
colnames(q2_MyMultResp_labels)[6] <- "Windows"   
colnames(q2_MyMultResp_labels)[7] <- "HP"   
colnames(q2_MyMultResp_labels)[8] <- "Tablet"   
colnames(q2_MyMultResp_labels)[9] <- "Other"   

str(q2_MyMultResp_labels)

q2_MyMultResp<-data.frame(Freq = colSums(q2_MyMultResp_labels[1:9]),
                          Pct.of.all.Resp = (colSums(q2_MyMultResp_labels[1:9])/sum(q2_MyMultResp_labels[1:9]))*100,
                          Pct.Check.All.That.Apply = (colSums(q2_MyMultResp_labels[1:9])/nrow(q2_MyMultResp_labels[1:9]))*100)
q2_MyMultResp


barplot(q2_MyMultResp[[3]]
        ,names.arg=row.names(q2_MyMultResp)
        ,main = "q2. Do you own any of the following smartphones or other web-enabled devices? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.8)

#q4. Do you use any of the following kinds of Apps? (Select all that apply)
#Pct.of.all.Resp = Out of all the responses how many chose it
#Pct.Check.All.That.Apply =  Out of each question, what is the percentage?

q4_MyMultResp_labels<-data.frame(combdata_segment2[16:26])

names(q4_MyMultResp_labels) <- make.names(names(q4_MyMultResp_labels)) 
colnames(q4_MyMultResp_labels)[1] <- "Music"       
colnames(q4_MyMultResp_labels)[2] <- "TV"   
colnames(q4_MyMultResp_labels)[3] <- "Ent."   
colnames(q4_MyMultResp_labels)[4] <- "TV Show"   
colnames(q4_MyMultResp_labels)[5] <- "Gaming"   
colnames(q4_MyMultResp_labels)[6] <- "Social"   
colnames(q4_MyMultResp_labels)[7] <- "Gen.News"   
colnames(q4_MyMultResp_labels)[8] <- "Shop"   
colnames(q4_MyMultResp_labels)[9] <- "Pub.News"   
colnames(q4_MyMultResp_labels)[10] <- "Other"  
colnames(q4_MyMultResp_labels)[11] <- "None"

str(q4_MyMultResp_labels)

q4_MyMultResp<-data.frame(Freq = colSums(q4_MyMultResp_labels[1:11]),
                          Pct.of.all.Resp = (colSums(q4_MyMultResp_labels[1:11])/sum(q4_MyMultResp_labels[1:11]))*100,
                          Pct.Check.All.That.Apply = (colSums(q4_MyMultResp_labels[1:11])/nrow(q4_MyMultResp_labels[1:11]))*100)
q4_MyMultResp

barplot(q4_MyMultResp[[3]]
        ,names.arg=row.names(q4_MyMultResp)
        ,main = "q4. Do you use any of the following kinds of Apps? (Select all that apply)"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Smartphones or other web-enabled devices"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.7)

par(mfrow=c(1,1))

#q11. How many Apps do you have on your smartphone/iPod Touch/Tablet? If you have more than of
#these device, please tell us the total number of Apps. 
q11<-ggplot(combdata_segment2_labs) +
  geom_bar( aes(q11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q11. Number of Apps on smartphone/iPod Touch/Tablet" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q11

#q12. Of your Apps, what percent were free to download? 
q12<-ggplot(combdata_segment2) +
  geom_bar(aes(q12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q12. Percent free to download (Apps)" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

q12_annotate<-q12 + annotate("text", x = 1, y = 7, label = "None", color="white")+ 
  annotate("text", x = 2, y = 100, label = "1% - 25%",color="white")+ 
  annotate("text", x = 3, y = 150, label = "26% - 50%",color="white")+ 
  annotate("text", x = 4, y = 215, label = "51% - 75",color="white")+ 
  annotate("text", x = 5, y = 140, label = "76% - 99",color="white")+ 
  annotate("text", x = 6, y = 110, label = "All",color="white")
q12_annotate
multiplot(q11,q12_annotate, cols=1)


#q13. How many times per week do you visit each of the following websites? (Select all that apply)
#Facebook
q13r1<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r1. Facebook" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r1

#Twitter
q13r2<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r2),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r2. Twitter" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r2

#Myspace
q13r3<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r3),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r3. Myspace" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r3

#Pandora Radio
q13r4<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r4),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r4. Pandora Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r4

#Vevo
q13r5<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r5),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r5. Vevo" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r5

#YouTube
q13r6<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r6),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r6. YouTube" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r6

#AOL Radio
q13r7<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r7),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r7. AOL Radio" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r7

#Last.fm
q13r8<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r8),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r8. Last.fm" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r8

#Yahoo Entertainment and Music
q13r9<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r9),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r9. Yahoo Ent. and Music" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r9

#IMDB
q13r10<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r10),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r10. IMDB" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r10

#LinkedIn
q13r11<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r11),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r11. LinkedIn" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r11

#Netflix
q13r12<-ggplot(combdata_segment2_labs) +
  geom_bar(aes(q13r12),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("q13r12. Netflix" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())
q13r12

multiplot(q13r1, q13r2, q13r3,q13r4, q13r5, q13r6,q13r7, q13r8, q13r9,q13r10, q13r11, q13r12, cols=3)

#Questions 24 to 26 (Visualizing Likert Scale Data)
library(likert)
likert(combdata_seg_labs_survey2)
Result = likert(combdata_seg_labs_survey2)
plot(Result,type="bar")
