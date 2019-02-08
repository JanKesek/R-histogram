The customer claims that they’ve been shortchanged because their datacannot
have arisen from a distribution with meanµ=80, so the true meanweight must 
be less than 80. To investigate this claim, the manufacturer con-ducts a 
hypothesis test using a significance level of??=0.05.
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)
H0: mean=80
Halter: mean<80
n<-length(snacks)
snack.mean<-mean(snacks)
snack.sd <- sd(snacks)
GIVEN THIS SD WHAT IS PROB OF OBSERVING SAMPLE MEAN (n=44) of 78.91
44-1=43 DEGREES OF FREEDOM
OR LESS IF THE TRUE MEAN IS 80 GRAMS?
  TEST STATISTICS: MEAN-MEAN0 / (sd/sqrt(n))
snack.standarderror<-snack.sd/sqrt(n)
snack.T <- (snack.mean - 80)/snack.standarderror
#PT: PROBABILITY OF RETURNING NUMBER SMALLER THAN ARGUMENT
pt(snack.T, df=n-1)
#IT MEANS: IF H0 IS TRUE THERE IS ONLY PT% CHANCE THAT WE WILL OBSERVE
#SAMPLE MEAN OF 78.91 OR LESS AS RANDOM PHENOMEN
#PT < ALPHA=0.05, SO WE REJECT H0, WE SUGGEST TRUE MEAN IS < 80 GRAMS
t.test(x=snacks, mu= 80, alternative = "less")


#T-TEST FOR 2 SETS OF VALUES

snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,
             79.21,81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,
             79.08,79.47,78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)

