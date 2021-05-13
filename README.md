# Plan de Producción Agregado

## Tabla de contenido
    1. Contexto del programa.
    2. Estructura del código.
    3. Explicación del código.
    4. Ejemplo gráfico y resultados.

### 1. Contexto del programa

En la segunda fila del siguiente diagrama de producción, se encuentra el plan agregado de producción, este es el primer elemento que debe obtenerse una vez iniciada la producción.

https://raw.githubusercontent.com/jaunjolj/Plan-de-Produccion-Agregado/master/DiagramaPlanning.png

Para conseguirlo, es necesario hacer muchos cálculos y previamente tener un pronóstico de la demanda. El programa en R de este repositorio fue creado para obtenerlo de manera más sencilla.

En este documento, se explicará la estructura del código, la explicación del mismo y se mostrarán los resultados correspondiente al plan de producción agregada.
Si tienes previos conocimientos sobre el control de la producción será más fácil para ti utilizar este programa.

### 2. Estructura del código

Utilizarás dos archivos en el siguiente orden: "ForecastEq.R" y "AutoAggrPP.R".

* Primero, deberás ejecutar el archivo "ForecastEq.R" para realizar un pronóstico de la demanda.
Para obtener el mismo, se utilizarán los datos que asocies a la variable 'trainingData'. **Si no ejecutas primero este archivo, no podrás continuar correctamente.**

* Posteriormente, en el archivo "AutoAggrPP.R" visualizarás una función que te mostrará el resultado del Plan Agregado de Producción, esta necesitará algunos parámetros:
  *  En la variable ´f´ deberás colocar el pronóstico que obtuviste en "ForecastEq.R". 
  *  En la variable 'objFunction' deberás colocar los valores de las variables de tu función objetivo (generada por ti).
  *  Si existían producción, trabajadores o días en el periodo anterior, deberás colocarlos en las variables 'p0', 'wkr0' y 'd0', respectivamente.

Ejemplo:
```
           //f=c(2760,3320,3970,3540,3180,2900)
           //objFunction <- c(0,0,0,
                 15*8*21,450,600,5,15,0,0,0,0,
                 15*8*20,450,600,5,15,0,0,0,0,
                 15*8*23,450,600,5,15,0,0,0,0,
                 15*8*21,450,600,5,15,0,0,0,0,
                 15*8*22,450,600,5,15,0,0,0,0,
                 15*8*22,450,600,5,15,0,0,0,0)
           //p0=41383
           //d0=260
           //wkr0=40
```
* Finalmente, sólo deberás ejecutar todo el código de "AutoAggrPP.R" y te dará el costo resultante junto con el valor de cada variable de la función objetivo.

### 3. Explicación del código

En el archivo "ForecastEq.R", se utilizan los datos asociados a la variable 'trainingData' para obtener cierta información estadística. Estos datos se utilizan en 6 procedimientos diferentes: naive, seasonal naive, ses, holt, hwMultiplicative y hwAdditive.

Estos procedimientos están de manera predeterminada en RStudio. Una vez realizados, el programa evalúa cuál de estos es el más eficiente y lo guarda en una variable. Esta última, obtiene el dato específico del pronóstico en otra variable. Al ejecutarla, mostrará los datos del pronóstico. 

Posteriormente, en el archivo "AutoAggrPP.R" se muestra una función que ejecutará diferentes procedimientos,  dependiendo del número de periodos establecidos. Dicha función contiene muchas matrices con diversas variables que favorecen la realización y obtención del Plan Agregado de Producción.

Al final del código de este archivo, es necesario modificar los parámetros de la función y al ejecutar su variable asociada te mostrará el resultado del plan.

### 4. Ejemplo gráfico y resultados.

#### Pronóstico

https://raw.githubusercontent.com/jaunjolj/Plan-de-Produccion-Agregado/master/ForecastExample.png

#### Resultados del Plan Agregado de Producción
```
> print(paste("The total cost is: ", aggregateProductionPlan$objval))
**[1] "The total cost is:  605184.363191945"**
> best_sol <- aggregateProductionPlan$solutio
> names(best_sol) <- variablesNames
> print(best_sol)
**         W0          I0          B0          W1          H1          F1          I1          B1          P1          W2 
  35.000000    0.000000    0.000000   33.029436    0.000000    1.970564    0.000000    0.000000 2760.000000   41.717614 
         H2          F2          I2          B2          P2          W3          H3          F3          I3          B3 
   8.688178    0.000000    0.000000    0.000000 3320.000000   43.378451    1.660837    0.000000    0.000000    0.000000 
         P3          W4          H4          F4          I4          B4          P4          W5          H5          F5 
3970.000000   42.363842    0.000000    1.014609    0.000000    0.000000 3540.000000   36.325852    0.000000    6.037990 
         I5          B5          P5          W6          H6          F6          I6          B6          P6 
   0.000000    0.000000 3180.000000   33.127349    0.000000    3.198503    0.000000    0.000000 2900.000000 **
> 
```
