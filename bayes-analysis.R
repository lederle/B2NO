library(rethinking)

B2NO_Demand <- data.frame(Year = rep(2017, 38),
       Month = rep(4:7, c(12, 6, 8, 12)),
       Day = c(10, 11, 12, 13, 14, 17, 19, 20, 21, 24, 25, 28, 
               10, 11, 12, 16, 22, 31, 
               1, 6, 8, 9, 26, 27, 29, 30, 
               3, 7, 10, 11, 12, 18, 19, 20, 21, 24, 25, 26),
       Wait = c(2, 1, 4, 3, 3, 1, 1, 0, 0, 2, 2, 4,
                2, 1, 2, 5, 3, 3,
                3, 2, 6, 5, 7, 10, 3, 3,
                3, 0, 2, 1, 1, 1, 1, 1, 1, 4, 4, 5),
       Census = c(16, 14, 15, 14, 15, 16, 16, 13, 14, 14, 14, 13,
                  13, 13, 11, 13, 14, 15,
                  11, 12, 14, 11, 11, 10, 11, 11,
                  12, 10, 12, 13, 14, 13, 14, 11, 11, 14, 14, 14))


m.1 <- map(
  alist(
    Wait ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0, 100)
  ),
  data = B2NO_Demand
)

post <- extract.samples(m.1)
lambda_dist <- exp(post$a)

B2NO_census <- read.csv("out.csv", header = TRUE, sep = "^", stringsAsFactors = FALSE)

B2NO_census$depart <- c(-99,
diff(B2NO_census$CumDischarges) +
diff(B2NO_census$CumTransfersOutWard) +
diff(B2NO_census$CumTransfersOut) +
diff(B2NO_census$CumTransfersOutService))

B2NO_census_trim <- B2NO_census[2:273,]

m.2 <- map(
  alist(
    depart ~ dzipois(p, lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(0, 1),
    al ~ dnorm(0, 10)
  ),
  data = B2NO_census_trim
)

logistic(-1.1)
exp(.76)
posterior_depart <- extract.samples(m.2)
hist(logistic(posterior_depart$ap))
hist(exp(posterior_depart$al))
