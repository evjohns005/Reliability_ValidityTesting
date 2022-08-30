library(corpcor)
library(GPArotation)
library(psych)
library(IDPmisc)

studentSurvey = read.csv("C:/Users/evjoh/Downloads/studentSurvey.csv")
head(studentSurvey)

names(studentSurvey)
ncol(studentSurvey)
studentSurvey1 <- studentSurvey[,31:42]
View(studentSurvey1)
studentSurvey2 <- NaRV.omit(studentSurvey1)

# Testing Assumptions:
## Sample size= 500 (Assumption met)
## Absense of Multicollinearity
studentSurveyMatrix <- cor(studentSurvey2)
View(studentSurveyMatrix)

## Assumption Met + Some relationship between survey items is seen since all values are above 0.3
## To doublecheck findings in correlation matrix:

cortest.bartlett(studentSurvey2)
## Shows significant; meaning there is a suitable correlation to proceed with a factor analysis

## Checking for determinants (for further proof)
det(studentSurveyMatrix)
## Assumption met; shows a sufficient relation between variables to proceed with factor analysis


# Factor Analysis:
ncol(studentSurvey2)
ssModel1 <- principal(studentSurvey2, nfactor = 12, rotate = "none")
ssModel1

## For a visual view:
plot(ssModel1$values, type="b")

## Second Pass to Test the suspected number of factors
ssModel2 <- principal(studentSurvey2, nfactor = 1, rotate = "none")
ssModel2

## Examing Residuals to determine model fit:
residuals <- factor.residuals(studentSurveyMatrix, ssModel2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
largeResid <- abs(residuals) > 0.05
sum(largeResid)
sum(largeResid/nrow(residuals))

### Only having one factor is a good model fit for the data since the output shows only 45% of residuals are large

# Out of curiousity, even though only one eigenvalue showed above 1, going to try 2 factors
ssModel3 <- principal(studentSurvey2, nfactor = 2, rotate = "none")
ssModel3
residuals1 <- factor.residuals(studentSurveyMatrix, ssModel3$loadings)
residuals1 <- as.matrix(residuals1[upper.tri(residuals1)])
largeResid1 <- abs(residuals1) > 0.05
sum(largeResid1)
sum(largeResid1/nrow(residuals1))

# Value did not change; confirms that one factor is still a good fit

## Rotating the factors to determine where each survey item fits:
ssModel4 <- principal(studentSurvey2, nfactors = 1, rotate = "oblimin")
ssModel4

print.psych(ssModel4, cut = .3, sort=TRUE)


# Calculating Reliability:
library(psych)

studentSurvey3 <- studentSurvey2[, 1:12]
alpha(studentSurvey3)

### Inter-rate reliability: hows a raw_alpha score of 0.92 which is good reliability

### Inter-Item Reliability values greater than 0.3 in r.drop column and non missing response frequency looks relatively evenly distributed

## Conclusion shows that this is a very reliable survey.
