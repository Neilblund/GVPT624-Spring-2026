# Neil note: 
# Data comes from ACD2EPR: https://icr.ethz.ch/data/epr/acd2epr/ which links the
# Ethnic Power Relations Data set to the UCDP actors data. Code below has been
# minimally modified from the original RScript make the code workable (one
# package used in the original analysis is defunct and there were some minor
# typos and directory issues)


# Download replication data
file<-'https://files.prio.org/Journals/JPR/2017/54/2/Lars-Erik%20Cederman,%20Kristian%20Skrede%20Gleditsch%20&%20Julian%20Wucherpfennig.zip'
download.file(file,
              mode = 'wb',
              dest = 'cederman_gleditsch_wucherpfennig.zip'
              )


#  Assuming decline_data_jpr.dta is in your current working directory...
mydata <- haven::read_dta(unz('cederman_gleditsch_wucherpfennig.zip',"decline_data_jpr.dta"))

mydata <- subset(mydata, isrelevant==1) 

mydata$regsample <- ifelse(mydata$isrelevant==1 & mydata$status_monopoly==0 & mydata$status_dominant==0, 1, 0)

# cutoffs for training data
t1 <- 1999  # Gurr
t2 <- 1994  # Kaplan

basemodel <- formula(incidence_flag~ family_peaceyears + waryears)

# Gurr model 
m1 <-glm(
  update(basemodel, ~ . +excluded
  ),
  family=binomial(link="logit"), data=subset(mydata, regsample==1 & year<=t1), maxit=2000 )
summary(m1)

m2a <-glm(
  update(basemodel, ~ . + year),
  family=binomial(link="logit"), data=subset(mydata, regsample==1 & year<=t1) )
summary(m2a)

# generate predicted probabilities
# Gurr Model
mydata$yhat <- predict(m1, mydata, type ="response")
yhat.xb <- predict(m1, mydata, type ="link", se.fit=TRUE)
mydata$yhat.lo <- 1/(1+exp(-yhat.xb$fit+2*yhat.xb$se.fit))
mydata$yhat.hi <- 1/(1+exp(-yhat.xb$fit-2*yhat.xb$se.fit))

# Kaplan model
mydata$trend <- predict(m2a, mydata, type ="response")
trend.xb <- predict(m2a, mydata, type ="link", se.fit=TRUE)
mydata$trend.lo <- 1/(1+exp(-trend.xb$fit+2*trend.xb$se.fit))
mydata$trend.hi <- 1/(1+exp(-trend.xb$fit-2*trend.xb$se.fit))


# collapse data
yeardata <- aggregate(cbind(incidence_flag, yhat, yhat.lo, yhat.hi, trend, trend.lo, trend.hi)~year, 
                      data=subset(mydata, regsample==1), FUN=sum)

# plot preparations
yeargt1 <- yeardata$year[yeardata$year>=t1]
yearst1 <- yeardata$year[yeardata$year<=t1]
yeargt2 <- yeardata$year[yeardata$year>=t1]
yearst2 <- yeardata$year[yeardata$year<=t1]

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
col1alpha <- add.alpha("#009E73", alpha=.3)
col2alpha <- add.alpha("#e79f00", alpha=.3)

col1 <- "#009E73"
col2 <- "#e79f00"

plot(yeardata$incidence_flag ~ yeardata$year , type="h", lwd=7, las=1, col="gray", frame=FALSE, axes=TRUE, ylim=c(0,50), cex.lab=1.3, cex.axis=1.3,
     #main = "Predicting the Future of Ethnic Conflict",
     ylab="Number of groups in conflict", xlab="")
lines(yeardata$trend[yeardata$year<=t1] ~ yearst2, lwd=3, lty=2, ) # changed
lines(yeardata$trend[yeardata$year>=t1] ~ yeargt2, lwd=3, lty=2, col=col2) # changed
lines(yeardata$yhat[yeardata$year<=t1] ~ yearst1, lwd=3, col="black")
lines(yeardata$yhat[yeardata$year>=t1] ~ yeargt1, lwd=3, col=col1)
polygon(c(yeargt1, rev(yeargt1)), c(yeardata$yhat.lo[yeardata$year>=t1], rev(yeardata$yhat.hi[yeardata$year>=t1])), col=col1alpha, border = NA)
polygon(c(yeargt2, rev(yeargt2)), c(yeardata$trend.lo[yeardata$year>=t1], rev(yeardata$trend.hi[yeardata$year>=t1])), col=col2alpha, border = NA) # changed
abline(v=t1, lwd=2, col="black")
h <- 37
h2 <- 50
t3 <- 2013
w <- 3
text((1946+t1)/2, h, "training")
text((t1+t3)/2, h2, "prediction")
arrows(1946, h, (1946+t1)/2-w, h, code=1, angle=10, lwd=2)
arrows((1946+t1)/2+w, h, t1, h, code=2, angle=10, lwd=2)

arrows(t1, h2, (t1+t3)/2-w, h2, code=1, angle=10, lwd=2)
arrows((t3+t1)/2+w, h2, t3, h2, code=2, angle=10, lwd=2)

text((2009+t1)/2+4, 29, "accommodation", col=col1, cex=1, font=2 )
text((2009+t2)/2+6, 47, "trend", col=col2, cex=1, font=2 )
#dev.off()

# evaluating in sample predictions

errors<-function(observed, predicted){
  n <- length(observed)
  rmse <- sqrt(sum((observed - predicted)^2) *  (1/n))
  mape <- mean(abs((predicted - observed)/observed)) * 100
  values<-c("RMSE" = rmse,
    "MAPE" = mape
    )
  return(values)
  
  
}

# In sample predictions
in.Gurr <- errors(observed = yeardata$incidence_flag[yeardata$year<t1],
       predicted = yeardata$yhat[yeardata$year<t1])

# In sample trends
in.trend <- errors(observed = yeardata$incidence_flag[yeardata$year<t1],
       predicted = yeardata$trend[yeardata$year<t1])



# out of sample predictions
out.Gurr<-errors(observed = yeardata$incidence_flag[yeardata$year>=t1],
       predicted = yeardata$yhat[yeardata$year>=t1])

# out of sample trends
out.trend<-errors(observed = yeardata$incidence_flag[yeardata$year>=t1],
       predicted = yeardata$trend[yeardata$year>=t1]
       )


# The trend model performs better prior to 1980
in.Gurr
in.trend
# But the accommodation model performs better after 1980
out.Gurr
out.trend




