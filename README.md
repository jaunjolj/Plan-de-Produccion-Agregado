# Plan de Producción Agregado

## Tabla de contenido
    1. Introducción.
    2. Estructura del código.
    3. Explicación del código.
    4. Ejemplo gráfico y resultados.

### 1. Introducción

La planeación y el control de la producción son actividades que sistematizan diversos métodos que se utilizan para optimizar la cadena de producción.
Previamente a esta actividad, ya existieron análisis de recursos humanos, materiales, maquinaria, planificación de instalaciones,etc. Estos procesos y actividades se centran en la demanda comercial.

Una actividad muy importante en la planeación y el control de la producción es la planeación agregada, esta contiene, como muchas otras, varias entradas, procesos intermedios y salidas, el siguiente diagrama nos muestra los elementos que contiene la planeación agregada.

https://raw.githubusercontent.com/jaunjolj/Plan-de-Produccion-Agregado/master/DiagramaPlanning.png

En este documento, se explicará la estructura del código, la explicación del mismo y se mostrará un ejemplo gráfico de un pronóstico de la demanda y los resultados de un plan agregado de producción.

### 2. Estructura del código.

Para realizar un plan agregado de producción, es necesario previamente realizar un pronóstico de la demanda, en el archivo "ForecastEq.R" podrás identificar dicho procedimiento.
Si requieres un pronóstico diferente al que se muestra en el archivo, tendrás que obtener o colocar los datos que necesitas para realizar dicho pronóstico y asociarlos a la variable 'trainingData'. Una vez hecho esto, es recomendable que ejecutes este código.

Posteriormente, en el archivo "AutoAggrPP.R" visualizarás una función que utilizará ciertos elementos, la cual te mostrará el resultado del Plan Agregado de Producción.
Después de dicha función, se mostrará un ejemplo que tomará el pronóstico inicial obtenido en el archivo "ForecastEq.R", si requieres obtener el Plan Agregado de Producción de otro pronóstico, será necesario que en la variable ´f´ coloques el nuevo pronóstico que obtuviste. También es necesario que en la variable 'objFunction' coloques una matriz con los nombres de las variables de una función objetivo.

Finalmente, sólo deberás ejecutar todo el código de "AutoAggrPP.R" y te dará el resultado.

### 3. Explicación del código

En el archivo "ForecastEq.R", al colocar la información necesaria para realizar un pronóstico de la demanda, se utiliza la variable 'trainingData' para obtener ciertos datos estadísticos. Estos datos se utilizan en 6 procedimientos diferentes: naive, seasonal naive, ses, holt, hwMultiplicative y hwAdditive.

Estos procedimientos están de manera predeterminada en RStudio. Una vez realizados, el programa evalúa cuál de estos procedimientos es el más eficiente y lo guarda en una variable. Esta última, obtiene el dato específico del pronóstico en otra variable. Al ejecutarla, mostrará los datos del pronóstico. 

En el archivo "AutoAggrPP.R" se muestra una función que, dependiendo del número de variables que abarque tú función objetivo, ejecutará un procedimiento distinto. Dicha función contiene muchas matrices con diversas variables que favorecen la realización y obtención del Plan Agregado de Producción.

Al final, en el ejemplo que se muestra, sólo es necesario modificar los parámetros de la función y al ejecutar su variable asociada te mostrará el resultado del plan.

### 4. Ejemplo gráfico y resultados.

#### Pronóstico

https://raw.githubusercontent.com/jaunjolj/Plan-de-Produccion-Agregado/master/ForecastExample.png

#### Resultados del Plan Agregado de Producción

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
