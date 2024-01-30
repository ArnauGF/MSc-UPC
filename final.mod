#Obs: en esta segunda parte resolvemos el problema usando
#la matriz triangular superior y adaptando el modelo para 
#trabajar con los datos de esta forma. 
#En el .run añadimos instrucciones que nos permiten ver las 
#variables básicas en cada solución.
param n;
set bares:=0..n;
#W,W1,W2,...,W7 son los pares de bares que violan SEC en la #solución devuelta por la primera relajación lineal
set W;
set W2;
set W3;
set W4;

#S1,S2,S3 SEC violadas tras añadir 2-match y comb ineq
set S1; 
set S2; 
set S3; 
set W3b; 
set W5;

#2-match ineq
set H;
set T1; 
set T2; 
set T3; 

#comb ineq 
set H1; 
set T1b; 
set T2b; 
set T3b;

param TIME{bares, bares};

#Variable x relajada
var x{i in bares, j in bares: j>i}>=0,<=1;

#Función objetivo
minimize TotalTime:
sum{i in bares, j in bares: j>i} TIME[i, j]*x[i, j]; 

#degree constraints
subject to degree_constraints {i in bares}:
sum{j in bares: j < i} x[j,i] + sum{k in bares: i < k} x[i,k] = 2;

#SEC
subject to sec:
sum{i in W, j in W: i<j} x[i,j] <= card(W)-1;

subject to sec2:
sum{i in W2, j in W2: i<j} x[i,j] <= card(W2)-1; 

subject to sec3:
sum{i in W3, j in W3: i<j} x[i,j] <= card(W3)-1; 

subject to sec4:
sum{i in W4, j in W4: i<j} x[i,j] <= card(W4)-1; 

subject to sec9:
sum{i in S1, j in S1: i<j} x[i,j] <= card(S1)-1; 

subject to sec10:
sum{i in S2, j in S2: i<j} x[i,j] <= card(S2)-1; 

subject to sec11:
sum{i in S3, j in S3: i<j} x[i,j] <= card(S3)-1; 

subject to sec5:
sum{i in W3b, j in W3b: i<j} x[i,j] <= card(W3b)-1; 

subject to sec6:
sum{i in W5, j in W5: i<j} x[i,j] <= card(W5)-1;

#2-match ineq
subject to 2match:
sum{i in H, j in H: i<j} x[i,j] + sum{i in T1, j in T1: i<j} x[i,j] + sum{i in T2, j in T2: i<j} x[i,j] + sum{i in T3, j in T3: i<j} x[i,j] <= card(H) + 1;

#Comb inequality
subject to comb:
sum{i in H1, j in H1: i<j} x[i,j] + sum{i in T1b, j in T1b: i<j} x[i,j] + sum{i in T2b, j in T2b: i<j} x[i,j] + sum{i in T3b, j in T3b: i<j} x[i,j] <= card(H1) + card(T1b) - 1 + card(T2b) - 1 + card(T3b) - 1 - 2;

