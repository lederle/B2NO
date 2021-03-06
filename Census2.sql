SELECT TOP 1000 
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
  WHERE DC.Sta3n = 553 AND CensusDate >= '2017-01-01' AND WL.WardLocationName = 'B2 NO'