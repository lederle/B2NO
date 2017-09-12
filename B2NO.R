library(tidyverse)
library(lubridate)
library(nycflights13)
library(RODBC)
library(ggplot2)

B2NO_Demand <- tibble(Year = rep(2017, 38),
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

ggplot(data = B2NO_Demand) + geom_bar(mapping = aes(x = Wait))
summary(B2NO_Demand$Wait)
var(B2NO_Demand$Wait)

library(vcd)
gf <- goodfit(B2NO_Demand$Wait, type = "poisson", method = "ML")
summary(gf)
plot(gf)
# fit is good enough, mean is the ML estimate
mean(B2NO_Demand$Wait)
rpois(10, 2.684)

# Get daily census
options(stringsAsFactors = FALSE)
a01handle <- odbcDriverConnect('driver={SQL Server};server=VHACDWa01.vha.med.va.gov;database=CDWWork;trusted_connection=true')
res <- sqlQuery(a01handle,
                "SELECT 
                 DC.[Sta3n]
                ,[CensusDate]
                ,[PatientsRemaining]
                ,[CumDischarges]
                ,[CumTransfersOutWard]
                ,[CumPatientsRemaining]
                ,[CumTransfersIn]
                ,[CumTransfersOut]
                ,[CumTransfersOutService]
                ,[CumTransfersInService]
                ,[CumAdmissions]
                ,[CumLosses]
                FROM [CDWWork].[Inpat].[DailyCensus] AS DC
                INNER JOIN Dim.WardLocation AS WL ON WL.WardLocationSID = DC.WardLocationSID
                WHERE DC.Sta3n = 553 AND CensusDate >= '2017-01-01' AND WL.WardLocationName = 'B2 NO'"
                )
out <-
diff(res$CumDischarges) +
diff(res$CumTransfersOutWard) +
diff(res$CumTransfersOut) +
diff(res$CumTransfersOutService)
summary(out)
var(out)

  # these results indicate need for zero inflated model
gf_out_pois <- goodfit(out, type = "poisson", method = "ML")
summary(gf_out_pois)
plot(gf_out_pois)

gf_out_nb <- goodfit(out, type = "nbinomial", method = "ML")
summary(gf_out_nb)
plot(gf_out_nb)