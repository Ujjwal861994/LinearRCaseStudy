#########################################################################################
#
# A crime map of India in R - Crime against women
# Designed and developed by : Ujjwal Aggarwal
# Date : 16 Sep 2018
#
########################################################################################

setwd("E:\\Ujjwal\\Git_crime_against_women\\dataset")
indiacrime = read.csv("crime_against_women_stateWise.csv", header = TRUE, check.names = FALSE)

setwd("..")
# Create an output directory if it does not exist
if(!file.exists("output")) {
  dir.create("output")
}

# Use check.names = FALSE otherwise year get read as x2001, x2002 etc
statecrime <- function(indiacrime, row, state,crime) {
  year <- c(2001:2014)
  # 
  atitle <- paste(state," - ", crime)
  # Read the crime values from the XL
  thecrime <- as.numeric(indiacrime[row,3:16])
  
  # Calculate the minimum and maximum
  ymin <- min(thecrime) - 300
  ymax <- max(thecrime) + 500
  print(ymin)
  print(ymax)
  
  # Set the intervals and ymin & ymax based on the values
  if(ymin > 8000) {
    interval <- 2000
    ymin <- min(thecrime) - 1500
    ymax <- max(thecrime) + 10000
  }
  else {
    interval <- 100
    
  }
  

  crimeplot <- paste(crime,".jpg")
  jpeg(crimeplot)
  
  # Plot the details of the crime
  plot(year,thecrime ,pch= 15, col="red", xlab = "Year", ylab= crime, main = atitle,
       ,xlim=c(2001,2020),ylim=c(ymin,ymax), axes=FALSE)
  
  # Set the axes
  axis(side=1, at=c(2000:2020))
  axis(side=2, at=seq(ymin, ymax, by=interval))
  box()
  
  # Fit a linear regression model
  lmfit <-lm(thecrime~year)
  
  # Draw the lmfit line
  abline(lmfit)
  
  # Calculate the projected incidents of the crime
  nyears <-c(2013:2020)
  nthecrime <- rep(0,length(nyears))
  
  # Projected crime incidents from 2013 to 2018 using a linear regression model
  for (i in seq_along(nyears)) {
    nthecrime[i] <- lmfit$coefficients[2] * nyears[i] + lmfit$coefficients[1]
    
  }
  
  # Add the legend
  alegend <- paste(
    "Projected ",crime, " in ", state)
  points(nyears,nthecrime,pch= 17, col="blue")
  legend( x="topleft", 
          legend=c(alegend),
          col=c("red"), bty="n" , lwd=1, lty=c(2), 
          pch=c(15) )
  
  dev.off()
  # Write the projected crime rate in a file
  nthecrime <- round(nthecrime,2)
  nthecrime <- c(state, nthecrime, "\n")
  print(nthecrime)
  setwd("..")
  #write(nthecrime,file=fileconn, ncolumns=9, append=TRUE,sep="\t")
  filename <- paste(crime,".txt")
  # Write the output in the ./output directory
  setwd("../output")
  cat(nthecrime, file=filename, sep=",",append=TRUE)
}

if(!file.exists("./statewise_crime_charts")) {
  dir.create("./statewise_crime_charts")
}
setwd("./statewise_crime_charts")
# 1. Andhra Pradesh 
# Make seperate folders for each state
if(!file.exists("Andhra Pradesh")) {
  dir.create("Andhra Pradesh")
}
setwd("Andhra Pradesh")
i <- 1
statecrime(indiacrime, i, "Andhra Pradesh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Andhra Pradesh")
statecrime(indiacrime, i, "Andhra Pradesh","Total crimes against women")

setwd("../statewise_crime_charts")
# 3. Assam
if(!file.exists("Assam")) {
  dir.create("Assam")
}
setwd("Assam")
i <- 3
statecrime(indiacrime, i, "Assam","Rape")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Assam")
statecrime(indiacrime, i, "Assam","Total crimes against women")


setwd("../statewise_crime_charts")
# 2. Arunachal  Pradesh
if(!file.exists("Arunachal  Pradesh")) {
  dir.create("Arunachal  Pradesh")
}
setwd("Arunachal  Pradesh")
i <- 2
statecrime(indiacrime, i, "Arunachal  Pradesh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal Pradesh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal Pradesh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal Pradesh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Arunachal  Pradesh")
statecrime(indiacrime, i, "Arunachal  Pradesh","Total crimes against women")


setwd("../statewise_crime_charts")
# 4. Bihar
if(!file.exists("Bihar")) {
  dir.create("Bihar")
}
setwd("Bihar")
i <- 4
statecrime(indiacrime, i, "Bihar","Rape")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Bihar")
statecrime(indiacrime, i, "Bihar","Total crimes against women")

setwd("../statewise_crime_charts")
# 5. Chattisgarh
if(!file.exists("Chattisgarh")) {
  dir.create("Chattisgarh")
}
setwd("Chattisgarh")
i <- 5
statecrime(indiacrime, i, "Chattisgarh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Chattisgarh")
statecrime(indiacrime, i, "Chattisgarh","Total crimes against women")
setwd("../statewise_crime_charts")
# 6. GOA
if(!file.exists("GOA")) {
  dir.create("GOA")
}
setwd("GOA")
i <- 6
statecrime(indiacrime, i, "GOA","Rape")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/GOA")
statecrime(indiacrime, i, "GOA","Total crimes against women")

setwd("../statewise_crime_charts")
# 7. Gujarat
if(!file.exists("Gujarat")) {
  dir.create("Gujarat")
}
setwd("Gujarat")
i <- 7
statecrime(indiacrime, i, "Gujarat","Rape")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Gujarat")
statecrime(indiacrime, i, "Gujarat","Total crimes against women")

setwd("../statewise_crime_charts")
# 8. Haryana
if(!file.exists("Haryana")) {
  dir.create("Haryana")
}
setwd("Haryana")
i <- 8
statecrime(indiacrime, i, "Haryana","Rape")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Haryana")
statecrime(indiacrime, i, "Haryana","Total crimes against women")

setwd("../statewise_crime_charts")
# 9. Himachal Pradesh
if(!file.exists("Himachal Pradesh")) {
  dir.create("Himachal Pradesh")
}
setwd("Himachal Pradesh")
i <- 9
statecrime(indiacrime, i, "Himachal Pradesh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Himachal Pradesh")
statecrime(indiacrime, i, "Himachal Pradesh","Total crimes against women")

setwd("../statewise_crime_charts")
# 10. JammuKashmir
if(!file.exists("JammuKashmir")) {
  dir.create("JammuKashmir")
}
setwd("JammuKashmir")
i <- 10
statecrime(indiacrime, i, "JammuKashmir","Rape")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/JammuKashmir")
statecrime(indiacrime, i, "JammuKashmir","Total crimes against women")

setwd("../statewise_crime_charts")
# 11. Jharkhand
if(!file.exists("Jharkhand")) {
  dir.create("Jharkhand")
}
setwd("Jharkhand")
i <- 11
statecrime(indiacrime, i, "Jharkhand","Rape")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Jharkhand")
statecrime(indiacrime, i, "Jharkhand","Total crimes against women")

setwd("../statewise_crime_charts")
# 12. Karnataka
if(!file.exists("Karnataka")) {
  dir.create("Karnataka")
}
setwd("Karnataka")
i <- 12
statecrime(indiacrime, i, "Karnataka","Rape")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Karnataka")
statecrime(indiacrime, i, "Karnataka","Total crimes against women")

setwd("../statewise_crime_charts")
# 13. Kerala
if(!file.exists("Kerala")) {
  dir.create("Kerala")
}
setwd("Kerala")
i <- 13
statecrime(indiacrime, i, "Kerala","Rape")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Kerala")
statecrime(indiacrime, i, "Kerala","Total crimes against women")

setwd("../statewise_crime_charts")
# 14. Madhya Pradesh
if(!file.exists("Madhya Pradesh")) {
  dir.create("Madhya Pradesh")
}
setwd("Madhya Pradesh")
i <- 14
statecrime(indiacrime, i, "Madhya Pradesh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Madhya Pradesh")
statecrime(indiacrime, i, "Madhya Pradesh","Total crimes against women")

setwd("../statewise_crime_charts")
# 15. Maharastra
if(!file.exists("Maharastra")) {
  dir.create("Maharastra")
}
setwd("Maharastra")
i <- 15
statecrime(indiacrime, i, "Maharastra","Rape")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Maharastra")
statecrime(indiacrime, i, "Maharastra","Total crimes against women")

setwd("../statewise_crime_charts")
# 16. Manipur
if(!file.exists("Manipur")) {
  dir.create("Manipur")
}
setwd("Manipur")
i <- 16
statecrime(indiacrime, i, "Manipur","Rape")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Manipur")
statecrime(indiacrime, i, "Manipur","Total crimes against women")

setwd("../statewise_crime_charts")
# 17. Meghalaya
if(!file.exists("Meghalaya")) {
  dir.create("Meghalaya")
}
setwd("Meghalaya")
i <- 17
statecrime(indiacrime, i, "Meghalaya","Rape")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Meghalaya")
statecrime(indiacrime, i, "Meghalaya","Total crimes against women")

setwd("../statewise_crime_charts")
# 18. Mizoram
if(!file.exists("Mizoram")) {
  dir.create("Mizoram")
}
setwd("Mizoram")
i <- 18
statecrime(indiacrime, i, "Mizoram","Rape")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Mizoram")
statecrime(indiacrime, i, "Mizoram","Total crimes against women")

setwd("../statewise_crime_charts")
# 19. Nagaland
if(!file.exists("Nagaland")) {
  dir.create("Nagaland")
}
setwd("Nagaland")
i <- 19
statecrime(indiacrime, i, "Nagaland","Rape")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Nagaland")
statecrime(indiacrime, i, "Nagaland","Total crimes against women")

setwd("../statewise_crime_charts")
# 20. Odisha
if(!file.exists("Odisha")) {
  dir.create("Odisha")
}
setwd("Odisha")
i <- 20
statecrime(indiacrime, i, "Odisha","Rape")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Odisha")
statecrime(indiacrime, i, "Odisha","Total crimes against women")

setwd("../statewise_crime_charts")
# 21. Punjab
if(!file.exists("Punjab")) {
  dir.create("Punjab")
}
setwd("Punjab")
i <- 21
statecrime(indiacrime, i, "Punjab","Rape")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Punjab")
statecrime(indiacrime, i, "Punjab","Total crimes against women")

setwd("../statewise_crime_charts")
# 22. Rajasthan
if(!file.exists("Rajasthan")) {
  dir.create("Rajasthan")
}
setwd("Rajasthan")
i <- 22
statecrime(indiacrime, i, "Rajasthan","Rape")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Rajasthan")
statecrime(indiacrime, i, "Rajasthan","Total crimes against women")

setwd("../statewise_crime_charts")
# 23. Sikkim
if(!file.exists("Sikkim")) {
  dir.create("Sikkim")
}
setwd("Sikkim")
i <- 23
statecrime(indiacrime, i, "Sikkim","Rape")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Sikkim")
statecrime(indiacrime, i, "Sikkim","Total crimes against women")

setwd("../statewise_crime_charts")
# 24. Tamil Nadu
if(!file.exists("Tamil Nadu")) {
  dir.create("Tamil Nadu")
}
setwd("Tamil Nadu")
i <- 24
statecrime(indiacrime, i, "Tamil Nadu","Rape")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Tamil Nadu")
statecrime(indiacrime, i, "Tamil Nadu","Total crimes against women")

setwd("../statewise_crime_charts")
# 25. Tripura
if(!file.exists("Tripura")) {
  dir.create("Tripura")
}
setwd("Tripura")
i <- 25
statecrime(indiacrime, i, "Tripura","Rape")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Tripura")
statecrime(indiacrime, i, "Tripura","Total crimes against women")

setwd("../statewise_crime_charts")
# 26 Uttar Pradesh
if(!file.exists("Uttar Pradesh")) {
  dir.create("Uttar Pradesh")
}
setwd("Uttar Pradesh")
i <- 26
statecrime(indiacrime, i, "Uttar Pradesh","Rape")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Uttar Pradesh")
statecrime(indiacrime, i, "Uttar Pradesh","Total crimes against women")

setwd("../statewise_crime_charts")
# 27 Uttarakhand
if(!file.exists("Uttarakhand")) {
  dir.create("Uttarakhand")
}
setwd("Uttarakhand")
i <- 27
statecrime(indiacrime, i, "Uttarakhand","Rape")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/Uttarakhand")
statecrime(indiacrime, i, "Uttarakhand","Total crimes against women")

setwd("../statewise_crime_charts")
# 28 West Bengal
if(!file.exists("West Bengal")) {
  dir.create("West Bengal")
}
setwd("West Bengal")
i <- 28
statecrime(indiacrime, i, "West Bengal","Rape")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Kidnapping& Abduction")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Dowry Deaths")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Assault on Women")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Insult to modesty")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Cruelty by husband_relatives")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Imporation of girls from foreign country")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Immoral traffic act")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Dowry prohibition act")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Indecent representation of Women Act")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Commission of Sati Act")
i <- i+38
setwd("../statewise_crime_charts/West Bengal")
statecrime(indiacrime, i, "West Bengal","Total crimes against women")

