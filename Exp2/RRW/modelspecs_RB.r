###############################
#### MODEL SPECIFICATIONS #####
###############################


grpVars <- c("defaultKilled")

#fixedNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(1, 1, 0.25)

  fixedNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#freeNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(7, 1, 0.25)

  freeNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeDB
  #Add DB as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.25, 0, 0.001)

  freeDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeB
  #Add B as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(200, 5, 0.01)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeValue
  #Add v as a fixed parameter (v = -0.07)
  #add a column for the fixed parameter.  This is needed because we will be adding another vc effect later
    x1 <- "default"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v1 <- 1

    columnName <- "VCconstantColumn"
    df.code <- data.frame(logic = c(x1), value = c(v1))
    GroupByVariables <- grpVars
    parameter <- "vc"
    ParameterName <- "vConstant"
    parameterBounds <- c(0.35, -0.35, 0.001)

  freeVC <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #now predict the split of "defaultKilled" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
    x1 <- "defaultKilled == 'defaultLVO'"
    #when the defaultKilled is LVI then bias towards the upperBound (save HVI: bias = no action)
    v1 <- 1
    x2 <- "defaultKilled == 'defaultHVO'"
    #when the defaultKilled is HVI then bias towards the lowerBound (save LVI: bias = no action)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "defaultKilledSEColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "sDKbias"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "s"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.8, -0.8, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    defaultKilledSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


#########################
### build models
#########################

simpleModelList.nSDfree <- NULL
simpleModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(freeNSD, freeDB, freeB))

#side default killed SE bias
defaultKilledSEModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(defaultKilledSE))




allModels <- list(simpleModelList.nSDfree= simpleModelList.nSDfree,
  defaultKilledSEModelList.nSDfree = defaultKilledSEModelList.nSDfree)

allFixedModels <- NULL
