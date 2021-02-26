require(lpSolve)
library(fpp2)

autoAPP <- function(objFunc,forecast,prod0,dias0,vecDays,Workers0,Inv0=0,Backord0=0,periodos=2,subcontract=FALSE,overtime=FALSE,idle=FALSE){ #extraSigns=NULL,extraConstraints=NULL
  
  per=periodos
  numVariables = (9*per)+3 #Número general de variables
  numConstraints = (3*per)+3 #Número general de restricciones
  k=prod0/(dias0*Workers0) #Aggr units per worker per day
  
  if(per==2){

    variablesNames= c('W0','I0','B0', 
                      'W1','H1','F1','I1','B1','P1','S1','O1','U1',
                      'W2','H2','F2','I2','B2','P2','S2','O2','U2')
    
    constrainstNames= c('InitW', 'InitIn', 'InitBk',
                        'Wo1','Wo2',
                        'In1', 'In2',
                        'Pr1','Pr2')
    
    #Matriz de variables y coeficientes
    A <- matrix(0, nrow = numConstraints, ncol = numVariables, 
                dimnames = list(constrainstNames, variablesNames))
    
    
    A['InitW', 'W0'] = 1
    A['InitIn', 'I0'] = 1
    A['InitBk', 'B0'] = 1
    
    intialSigns <- rep("=", 3)
    
    initialsRHS <- c(Workers0,
                     Inv0,
                     Backord0)
    
    A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
    A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
    
    workersSigns <- rep("=", 2)
    
    workersRHS <- rep(0,2)
    
    #Si hay subcontratos se agrega a la ecuación de inventario
    if(subcontract){
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = -1;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = -1;
    }else{
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = 0;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = 0;
    }
    
    inventorySigns <- rep("=", 2)
    inventoryRHS 
    
    #Consideración en la ec. de producción del tiempo extra y el ocio
    if(overtime & !idle){
      A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 0;
      A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 0;
    }else{
      if(!overtime & idle){
        A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 1;
        A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 1;
      }
      else{
        if(!overtime & !idle){
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 0;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 0;
        }
        else{
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 1;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 1;
        }
      }
    }
    productionSigns <- rep("<=", 2)
    productionRHS <- rep(0,2)
    
    # Find the optimal solution
    aggregateProductionPlan <-  lp(direction = "min",
                                   objective.in = objFunc,
                                   const.mat = A,
                                   const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                                   const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                                   int.vec = c(4,13) # Wi = integers
    )
    
    #Print solution
    print(paste("The total cost is: ", aggregateProductionPlan$objval))
    best_sol <- aggregateProductionPlan$solution
    names(best_sol) <- variablesNames
    print(best_sol)
  }
  
  if(per==3){
    
    variablesNames= c('W0','I0','B0', 
                      'W1','H1','F1','I1','B1','P1','S1','O1','U1',
                      'W2','H2','F2','I2','B2','P2','S2','O2','U2',
                      'W3','H3','F3','I3','B3','P3','S3','O3','U3')
    
    constrainstNames= c('InitW', 'InitIn', 'InitBk',
                        'Wo1','Wo2', 'Wo3',
                        'In1', 'In2','In3',
                        'Pr1','Pr2','Pr3')
    
    A <- matrix(0, nrow = numConstraints, ncol = numVariables, 
                dimnames = list(constrainstNames, variablesNames))
    
    A['InitW', 'W0'] = 1
    A['InitIn', 'I0'] = 1
    A['InitBk', 'B0'] = 1
    
    intialSigns <- rep("=", 3)
    
    initialsRHS <- c(Workers0,
                     Inv0,
                     Backord0)
    
    A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
    A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
    A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
    
    workersSigns <- rep("=", 3)
    
    workersRHS <- rep(0,3)
    
    if(subcontract){
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = -1;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = -1;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = -1;
    }else{
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = 0;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = 0;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = 0;
    }
    
    inventorySigns <- rep("=", 3)
    
    inventoryRHS <- -forecast
    
    if(overtime & !idle){
      A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 0;
      A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 0;
      A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 0;
    }else{
      if(!overtime & idle){
        A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 1;
        A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 1;
        A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 1;
      }
      else{
        if(!overtime & !idle){
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 0;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 0;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 0;
        }
        else{
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 1;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 1;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 1;
        }
      }
    }
    
    productionSigns <- rep("<=", 3)
    productionRHS <- rep(0,3)
    
    # Find the optimal solution
    aggregateProductionPlan <-  lp(direction = "min",
                                   objective.in = objFunc,
                                   const.mat = A,
                                   const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                                   const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                                   int.vec = c(4,13,22) # Wi = integers
    )
    
    #Print solution
    print(paste("The total cost is: ", aggregateProductionPlan$objval))
    best_sol <- aggregateProductionPlan$solution
    names(best_sol) <- variablesNames
    print(best_sol)
  }
  
  if(per==4){
    
    variablesNames= c('W0','I0','B0', 
                      'W1','H1','F1','I1','B1','P1','S1','O1','U1',
                      'W2','H2','F2','I2','B2','P2','S2','O2','U2',
                      'W3','H3','F3','I3','B3','P3','S3','O3','U3',
                      'W4','H4','F4','I4','B4','P4','S4','O4','U4')
    
    constrainstNames= c('InitW', 'InitIn', 'InitBk',
                        'Wo1','Wo2', 'Wo3','Wo4',
                        'In1', 'In2','In3','In4',
                        'Pr1','Pr2','Pr3','Pr4')
    
    A <- matrix(0, nrow = numConstraints, ncol = numVariables, 
                dimnames = list(constrainstNames, variablesNames))
    
    
    A['InitW', 'W0'] = 1
    A['InitIn', 'I0'] = 1
    A['InitBk', 'B0'] = 1
    
    intialSigns <- rep("=", 3)
    
    initialsRHS <- c(Workers0,
                     Inv0,
                     Backord0)
    
    A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
    A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
    A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
    A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;
    
    workersSigns <- rep("=", 4)
    
    workersRHS <- rep(0,4)
    
    if(subcontract){
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = -1;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = -1;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = -1;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = -1;
    }else{
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = 0;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = 0;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = 0;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = 0;
    }
    
    inventorySigns <- rep("=", 4)
    
    inventoryRHS <- -forecast
    
    if(overtime & !idle){
      A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 0;
      A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 0;
      A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 0;
      A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 0;
    }else{
      if(!overtime & idle){
        A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 1;
        A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 1;
        A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 1;
        A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 1;
      }
      else{
        if(!overtime & !idle){
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 0;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 0;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 0;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 0;
        }
        else{
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 1;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 1;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 1;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 1;
        }
      }
    }
   
    productionSigns <- rep("<=", 4)
    productionRHS <- rep(0,4)
    
    # Find the optimal solution
    aggregateProductionPlan <-  lp(direction = "min",
                                   objective.in = objFunc,
                                   const.mat = A,
                                   const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                                   const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                                   int.vec = c(4,13,22,31) # Wi = integers
    )
    
    #Print solution
    print(paste("The total cost is: ", aggregateProductionPlan$objval))
    best_sol <- aggregateProductionPlan$solution
    names(best_sol) <- variablesNames
    print(best_sol)
  }
  
  if(per==5){
    
    variablesNames= c('W0','I0','B0', 
                        'W1','H1','F1','I1','B1','P1','S1','O1','U1',
                        'W2','H2','F2','I2','B2','P2','S2','O2','U2',
                        'W3','H3','F3','I3','B3','P3','S3','O3','U3',
                        'W4','H4','F4','I4','B4','P4','S4','O4','U4',
                        'W5','H5','F5','I5','B5','P5','S5','O5','U5')
    
    constrainstNames= c('InitW', 'InitIn', 'InitBk',
                          'Wo1','Wo2', 'Wo3','Wo4','Wo5',
                          'In1', 'In2','In3','In4','In5',
                          'Pr1','Pr2','Pr3','Pr4','Pr5')
    
    A <- matrix(0, nrow = numConstraints, ncol = numVariables, 
                dimnames = list(constrainstNames, variablesNames))

    A['InitW', 'W0'] = 1
    A['InitIn', 'I0'] = 1
    A['InitBk', 'B0'] = 1
    
    intialSigns <- rep("=", 3)
    
    initialsRHS <- c(Workers0,
                     Inv0,
                     Backord0)
    
    A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
    A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
    A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
    A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;
    A['Wo5', 'W5'] = 1; A['Wo5', 'W4'] = -1; A['Wo5', 'H5'] = -1; A['Wo5', 'F5'] = 1;
    
    workersSigns <- rep("=", 5)
    
    workersRHS <- rep(0,5)
    
    if(subcontract){
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = -1;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = -1;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = -1;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = -1;
      A['In5', 'I5'] = 1; A['In5', 'B5'] = -1; A['In5', 'I4'] = -1; A['In5', 'B4'] = 1; A['In5', 'P5'] = -1; A['In5', 'S5'] = -1;
    }else{
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = 0;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = 0;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = 0;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = 0;
      A['In5', 'I5'] = 1; A['In5', 'B5'] = -1; A['In5', 'I4'] = -1; A['In5', 'B4'] = 1; A['In5', 'P5'] = -1; A['In5', 'S5'] = 0;
    }

    inventorySigns <- rep("=", 5)
    
    inventoryRHS <- -forecast
    
    if(overtime & !idle){
      A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 0;
      A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 0;
      A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 0;
      A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 0;
      A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = -1; A['Pr5', 'U5'] = 0;
    }else{
      if(!overtime & idle){
        A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 1;
        A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 1;
        A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 1;
        A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 1;
        A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = 0; A['Pr5', 'U5'] = 1;
      }
      else{
        if(!overtime & !idle){
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 0;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 0;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 0;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 0;
          A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = 0; A['Pr5', 'U5'] = 0;
        }
        else{
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 1;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 1;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 1;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 1;
          A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = -1; A['Pr5', 'U5'] = 1;
        }
      }
    }
    
    productionSigns <- rep("=", 5)
    productionRHS <- rep(0,5)
    
    # Find the optimal solution
    aggregateProductionPlan <-  lp(direction = "min",
                                   objective.in = objFunc,
                                   const.mat = A,
                                   const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                                   const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                                   int.vec = c(4,13,22,31,40) # Wi = integers
    )
    
    #Print solution
    print(paste("The total cost is: ", aggregateProductionPlan$objval))
    best_sol <- aggregateProductionPlan$solution
    names(best_sol) <- variablesNames
    print(best_sol)
  }
  
  if(per == 6){
    
    variablesNames = c('W0','I0','B0', 
                       'W1','H1','F1','I1','B1','P1','S1','O1','U1',
                       'W2','H2','F2','I2','B2','P2','S2','O2','U2',
                       'W3','H3','F3','I3','B3','P3','S3','O3','U3',
                       'W4','H4','F4','I4','B4','P4','S4','O4','U4',
                       'W5','H5','F5','I5','B5','P5','S5','O5','U5',
                       'W6','H6','F6','I6','B6','P6','S6','O6','U6')
    
    constrainstNames = c('InitW', 'InitIn', 'InitBk',
                         'Wo1','Wo2', 'Wo3','Wo4','Wo5','Wo6',
                         'In1', 'In2','In3','In4','In5','In6',
                         'Pr1','Pr2','Pr3','Pr4','Pr5','Pr6')
    
    A <- matrix(0, nrow = numConstraints, ncol = numVariables, 
                dimnames = list(constrainstNames, variablesNames))
    
    A['InitW', 'W0'] = 1
    A['InitIn', 'I0'] = 1
    A['InitBk', 'B0'] = 1
    
    intialSigns <- rep("=", 3)
    
    initialsRHS <- c(Workers0,
                     Inv0,
                     Backord0)
    
    A['Wo1', 'W1'] = 1; A['Wo1', 'W0'] = -1; A['Wo1', 'H1'] = -1; A['Wo1', 'F1'] = 1;
    A['Wo2', 'W2'] = 1; A['Wo2', 'W1'] = -1; A['Wo2', 'H2'] = -1; A['Wo2', 'F2'] = 1;
    A['Wo3', 'W3'] = 1; A['Wo3', 'W2'] = -1; A['Wo3', 'H3'] = -1; A['Wo3', 'F3'] = 1;
    A['Wo4', 'W4'] = 1; A['Wo4', 'W3'] = -1; A['Wo4', 'H4'] = -1; A['Wo4', 'F4'] = 1;
    A['Wo5', 'W5'] = 1; A['Wo5', 'W4'] = -1; A['Wo5', 'H5'] = -1; A['Wo5', 'F5'] = 1;
    A['Wo6', 'W6'] = 1; A['Wo6', 'W5'] = -1; A['Wo6', 'H6'] = -1; A['Wo6', 'F6'] = 1;
    
    workersSigns <- rep("=", 6)
    
    workersRHS <- rep(0,6)
    
    if(subcontract){
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = -1;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = -1;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = -1;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = -1;
      A['In5', 'I5'] = 1; A['In5', 'B5'] = -1; A['In5', 'I4'] = -1; A['In5', 'B4'] = 1; A['In5', 'P5'] = -1; A['In5', 'S5'] = -1;
      A['In6', 'I6'] = 1; A['In6', 'B6'] = -1; A['In6', 'I5'] = -1; A['In6', 'B5'] = 1; A['In6', 'P6'] = -1; A['In6', 'S6'] = -1;
    }else{
      A['In1', 'I1'] = 1; A['In1', 'B1'] = -1; A['In1', 'I0'] = -1; A['In1', 'B0'] = 1; A['In1', 'P1'] = -1; A['In1', 'S1'] = 0;
      A['In2', 'I2'] = 1; A['In2', 'B2'] = -1; A['In2', 'I1'] = -1; A['In2', 'B1'] = 1; A['In2', 'P2'] = -1; A['In2', 'S2'] = 0;
      A['In3', 'I3'] = 1; A['In3', 'B3'] = -1; A['In3', 'I2'] = -1; A['In3', 'B2'] = 1; A['In3', 'P3'] = -1; A['In3', 'S3'] = 0;
      A['In4', 'I4'] = 1; A['In4', 'B4'] = -1; A['In4', 'I3'] = -1; A['In4', 'B3'] = 1; A['In4', 'P4'] = -1; A['In4', 'S4'] = 0;
      A['In5', 'I5'] = 1; A['In5', 'B5'] = -1; A['In5', 'I4'] = -1; A['In5', 'B4'] = 1; A['In5', 'P5'] = -1; A['In5', 'S5'] = 0;
      A['In6', 'I6'] = 1; A['In6', 'B6'] = -1; A['In6', 'I5'] = -1; A['In6', 'B5'] = 1; A['In6', 'P6'] = -1; A['In6', 'S6'] = 0;
    }
    
    inventorySigns <- rep("=", 6)
    
    inventoryRHS <- -forecast
    
    if(overtime & !idle){
      A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 0;
      A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 0;
      A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 0;
      A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 0;
      A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = -1; A['Pr5', 'U5'] = 0;
      A['Pr6', 'P6'] = 1; A['Pr6', 'W6'] = -k*vecDays[6]; A['Pr6', 'O6'] = -1; A['Pr6', 'U6'] = 0;
    }else{
      if(!overtime & idle){
        A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 1;
        A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 1;
        A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 1;
        A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 1;
        A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = 0; A['Pr5', 'U5'] = 1;
        A['Pr6', 'P6'] = 1; A['Pr6', 'W6'] = -k*vecDays[6]; A['Pr6', 'O6'] = 0; A['Pr6', 'U6'] = 1;
      }
      else{
        if(!overtime & !idle){
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = 0; A['Pr1', 'U1'] = 0;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = 0; A['Pr2', 'U2'] = 0;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = 0; A['Pr3', 'U3'] = 0;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = 0; A['Pr4', 'U4'] = 0;
          A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = 0; A['Pr5', 'U5'] = 0;
          A['Pr6', 'P6'] = 1; A['Pr6', 'W6'] = -k*vecDays[6]; A['Pr6', 'O6'] = 0; A['Pr6', 'U6'] = 0;
        }
        else{
          A['Pr1', 'P1'] = 1; A['Pr1', 'W1'] = -k*vecDays[1]; A['Pr1', 'O1'] = -1; A['Pr1', 'U1'] = 1;
          A['Pr2', 'P2'] = 1; A['Pr2', 'W2'] = -k*vecDays[2]; A['Pr2', 'O2'] = -1; A['Pr2', 'U2'] = 1;
          A['Pr3', 'P3'] = 1; A['Pr3', 'W3'] = -k*vecDays[3]; A['Pr3', 'O3'] = -1; A['Pr3', 'U3'] = 1;
          A['Pr4', 'P4'] = 1; A['Pr4', 'W4'] = -k*vecDays[4]; A['Pr4', 'O4'] = -1; A['Pr4', 'U4'] = 1;
          A['Pr5', 'P5'] = 1; A['Pr5', 'W5'] = -k*vecDays[5]; A['Pr5', 'O5'] = -1; A['Pr5', 'U5'] = 1;
          A['Pr6', 'P6'] = 1; A['Pr6', 'W6'] = -k*vecDays[6]; A['Pr6', 'O6'] = -1; A['Pr6', 'U6'] = 1;
        }
      }
    }
    
    productionSigns <- rep("=", 6)
    productionRHS <- rep(0,6)
    
    # Find the optimal solution
    aggregateProductionPlan <-  lp(direction = "min",
                                   objective.in = objFunc,
                                   const.mat = A,
                                   const.dir = c(intialSigns,workersSigns,inventorySigns,productionSigns),
                                   const.rhs = c(initialsRHS, workersRHS, inventoryRHS, productionRHS),
                                   int.vec = c(4,13,22,31,40,49) # Wi = integers
    )
    
    #Print solution
    print(paste("The total cost is: ", aggregateProductionPlan$objval))
    best_sol <- aggregateProductionPlan$solution
    names(best_sol) <- variablesNames
    print(best_sol)
  }
}

#########################PRUEBA DE FUNCIÓN##########################

objFunction <- c(0,0,0,
                 15*8*21,450,600,5,15,0,0,0,0,
                 15*8*20,450,600,5,15,0,0,0,0,
                 15*8*23,450,600,5,15,0,0,0,0,
                 15*8*21,450,600,5,15,0,0,0,0,
                 15*8*22,450,600,5,15,0,0,0,0,
                 15*8*22,450,600,5,15,0,0,0,0)

f=c(2760,3320,3970,3540,3180,2900)
p0=41383
d0=260
wkr0=40
vdy=c(21,20,23,21,22,22)


autoAPP(objFunction,f,p0,d0,vdy,wkr0,periodos = 6)
