#Charlotte Rivard
#Cleaning the Simmons Alumni Dataset 1960-2018

Alumni <- read.csv("Alumni.csv")
library(mosaic)

#Fixing dead
Alumni$Dead <- ifelse(Alumni$Dead=="Yes", 1 , 0)
tally(Alumni$Dead)

#Checking out age distribution
#Alumni$Age <- as.numeric(Alumni$Age)
histogram(Alumni$Age)
#Weird there are a lot of strange young/impossible ages
young <- Alumni[Alumni$Age < 18,]
favstats(Alumni$Age)
boxplot(Alumni$Age)

deadLoc <- data.frame(tally(Alumni$Dead ~ Alumni$loc))
#84 of the dead alumni do not have a listed location

#Alumni by class
class <- data.frame(tally(Alumni$Class))
class
barplot(class$Freq, xlab= class$X)

#Major1
major1 <- data.frame(tally(Alumni$Major1))
major1 <- major1[order(-major1$Freq),] #Ordered by major from largest to smallest
barplot(major1$Freq, xlab= major1$X)
topMajors <- head(major1)
barplot(topMajors$Freq, xlab= "Top Majors")

#Major2
major2 <- data.frame(tally(Alumni$Major2))
major2 <- major2[order(-major2$Freq),] #Ordered by major from largest to smallest
barplot(major2$Freq, xlab= major2$X)
topMajor2 <- head(major2)
barplot(topMajor2$Freq, xlab= "Top Majors")

#rows that we do not have location data about...
which(Alumni$City=="") #138 rows, so that many unique locations
c <- data.frame(tally(Alumni$City))

Alumni$loc<-paste0(Alumni$City,", ",Alumni$State)
write.csv(Alumni, file = "AlumniClean.csv")


#Plotly example on https://plot.ly/r/scattermapbox/
#My API Key for google: AIzaSyBi_XJ_z1Zq37QfjN5-lbNgm9UVclJnq9U

#Are the distributions of people who are not found the same as those who are. 
#Like are all the nursing majors hiding?
missingLoc <- Alumni[which(Alumni$loc==", "),] #Dataset of the alumni w/ missing location
nrow(missingLoc)
#[1] 138
alumniLoc <- Alumni[which(Alumni$loc!=", "),] #Dataset of the alumni found
nrow(alumniLoc)
#[1] 21934




#Major1
major1 <- data.frame(tally(alumniLoc$Major1))
major1 <- major1[order(-major1$Freq),] #Ordered by major from largest to smallest
barplot(major1$Freq, xlab= major1$X)
topMajors <- head(major1)
barplot(topMajors$Freq, xlab= "Top Majors")

#17595 --> says she's from class of 1998, but she's only 25

#By industry could use some work, perhaps narrowing down categories. 

#Loads of missing data in this set


# CS Majors (153 recorded) 
cs <- alumniLoc[alumniLoc$Major1=="Computer Science"|alumniLoc$Major2=="Computer Science",]

# Nursing Majors  (3510 located)
nurs <- alumniLoc[alumniLoc$Major1=="Nursing"|alumniLoc$Major2=="Nursing",]

# Looking at which professions we could track
professions <- data.frame(tally(alumniLoc$Profession))
#there are the most nurses around...

#Nurses
Alumni$Nurse <- 0
for(i in 1:nrow(Alumni)){
  if((grepl("Nurse", Alumni$Position[i]))){
    Alumni$Nurse[i] <- 1
  }
  if((grepl("Nurse", Alumni$Profession[i]))){
    Alumni$Nurse[i] <- 1
  }
}
nurses <- Alumni[Alumni$Nurse==1,]
nrow(nurses)
write.csv(nurses, "nurses.csv")

#^^of those who are identified nurses now (1073) how many were nursing majors 964 + 5 = 969
majoredNursing <- nurses[nurses$Major1=="Nursing"|nurses$Major2=="Nursing",]

#Wages of nurses across the US 
https://www.bls.gov/oes/2017/may/oes291141.htm


#Attorneys
att <- alumniLoc[alumniLoc$Profession=="Attorney",]
ad <- data.frame(tally(att$Major1))
head(ad[order(-ad$Freq),])

ad2<- data.frame(tally(att$Major2))
head(ad2[order(-ad2$Freq),]) #^Were mainly goverment or political science majors

#Social Worker
soc <- alumniLoc[alumniLoc$Profession=="Social Worker",]
so <- data.frame(tally(soc$Major1))
head(so[order(-so$Freq),])

so2<- data.frame(tally(soc$Major2))
head(so2[order(-so2$Freq),])

#Physical Therapy
pt <- alumniLoc[alumniLoc$Profession=="Physical Therapist",]
ptm <- data.frame(tally(pt$Major1))
head(ptm[order(-ptm$Freq),])

pt2<- data.frame(tally(pt$Major2))
head(pt2[order(-pt2$Freq),]) #<-- mainly PT Majors, makes sense

#Should I do clustering? 
#https://developers.google.com/maps/documentation/javascript/marker-clustering

#Looking at which industries to track
industries <- data.frame(tally(alumniLoc$Industry))

#Looking into Computers industry 
#(if I wanted to do it by regular expressions how would I? This expression ^Computers - .+ )
#https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

alumniLoc$CS[alumniLoc$Industry == "Computers - System Design/Services"]<- 1
alumniLoc$CS[alumniLoc$Industry == "Computers - Product Manufacturing"]<- 1
alumniLoc$CS[alumniLoc$Industry == "Computers- Internet/Web Services"]<- 1
tally(alumniLoc$CS)
csIndustry <- alumniLoc[alumniLoc$CS==1,]

csm <- data.frame(tally(csIndustry$Major1))
csm<- csm[csm$Freq > 0,]
csm[order(-csm$Freq),] #There are so many ways people end up working on computers :)

csm2<- data.frame(tally(csIndustry$Major2))
csm2<- csm2[csm2$Freq > 0,]
csm2[order(-csm2$Freq),]

write.csv(csIndustry, "csIndustry.csv")

#Practice with nlp
for (i in nrow(Alumni){
  if("nurse" %in% Alumni$Position[i]){
    Alumni
  }
}


