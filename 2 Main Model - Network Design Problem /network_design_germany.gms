*----Set Definition----*

*--Cities--*
Set v Sets of all selectable german cities
    /Aachen, Berlin, Bremen, Chemnitz, Dortmund, Dresden, Duesseldorf, Erfurt,
    Frankfurt, Freiburg, Goettingen, Hamburg, Hannover, Karlsruhe, Kiel, Koeln,
    Leipzig, Luebeck, Magdeburg, Mannheim, Muenchen, Muenster, Nuernberg, Osnabrueck,
    Potsdam, Regensburg, Saarbruecken, Stuttgart/;
        
Set v_subset(v) subset for user based city selection;

*Subset has been initilized as empty*
v_subset(v) = no;

*Alternitive indices for cities [v]*
Alias (v,i,j,u);
display v;

*--Connections--*
Set E(v, v) Set of all possible connections between the cities [v];
Set E_subset(v,v) subset for connections depending on user based city selection;
    
*Subset has been initilized as fully connected*
E(v, i) = yes;
sadf

*----Parameter & Scalar Definition----*

*--Adaptable Parameters & Sets--*

*Forwarding information between $onExternalInput and $offExternalInput to GAMS MIRO*
*Default value of 1 is set*
$onExternalInput 
Parameter B maximum total cost allowed /1/;
$offExternalInput

*Forwarding information between $onExternalInput and $offExternalInput to GAMS MIRO*
*Using sets & subsets in order to enable user based selection of train type an region by single select*
*Default value of West is set*
*Default value of ICE is set*
Set cityselection /West, North, South, East, Metropolises/;
Set traintype /ICE, RE/;

$onExternalInput
Singleton Set subcityselction(cityselection) /Metropolises/;
Singleton Set subtraintype(traintype) /ICE/;
$offExternalInput


*--Not diretly adaptable Parameters--*

Parameters
b_min_display min budget for display in GAMS Miro
b_min min budget to ensure a feasible solution
b_max budget for the fully connected
long(v) longitudal value of cities
lat(v) latitudal value of cities
totalcost final cummulated construction costs of all built connections
totaltraveltime final cummulated travel time of all passengers (objective value)
travpass(v,v) cummulated number of passengers for each edge 
totaltravelers cummulated number of pessengers 
buildedge (v,v) information if connection is built
t_total(v,v) travel time between cities
d_total(v,v) number of passengers travelling between cities 
c_total(v,v) fixed construction costs for connection between cities
c_display(v,v) additional parameter for displaying only the cost of constructed connectinos
t(v,v) subparameter [refering to selected subset of cities by user]
d(v,v) subparameter [refering to selected subset of cities by user]
c(v,v) subparameter [refering to selected subset of cities by user];


*----User based reginal selection----*

If (Subcityselction('East'),
    v_subset('Berlin') = yes;
    v_subset('Potsdam') = yes;
    v_subset('Magdeburg') = yes;
    v_subset('Leipzig') = yes;
    v_subset('Erfurt') = yes;
    v_subset('Chemnitz') = yes;
    v_subset('Dresden') = yes;
);    

If (Subcityselction('West'),
    v_subset('Saarbruecken') = yes;
    v_subset('Frankfurt') = yes;
    v_subset('Aachen') = yes;
    v_subset('Koeln') = yes;
    v_subset('Muenster') = yes;
    v_subset('Dortmund') = yes;
    v_subset('Duesseldorf') = yes;
);    

If (Subcityselction('South'),
    v_subset('Muenchen') = yes;
    v_subset('Mannheim') = yes;
    v_subset('Karlsruhe') = yes;
    v_subset('Stuttgart') = yes;
    v_subset('Freiburg') = yes;
    v_subset('Regensburg') = yes;
    v_subset('Nuernberg') = yes;
);    

If (Subcityselction('North'),
    v_subset('Kiel') = yes;
    v_subset('Hamburg') = yes;
    v_subset('Luebeck') = yes;
    v_subset('Bremen') = yes;
    v_subset('Osnabrueck') = yes;
    v_subset('Hannover') = yes;
    v_subset('Goettingen') = yes;
);    

If (Subcityselction('Metropolises'),
    v_subset('Hamburg') = yes;
    v_subset('Berlin') = yes;
    v_subset('Muenchen') = yes;
    v_subset('Koeln') = yes;
    v_subset('Frankfurt') = yes;
    v_subset('Stuttgart') = yes;
);  


*----Import Data via .gdx file----*

$gdxIn data_general.gdx
$load long, lat, t_total, c_total
$GDXIN

$onText
The gravity model, akin to Newton's theory of gravity, assumes that the number of trips between two locations
is directly proportional to the trip productions at the origin and the attractions at the destination.
It emphasizes the importance of balancing, ensuring that total trip productions and attractions in a study area are equal.
This balance maintains the proportional relationship between origins and destinations, reflecting realistic travel patterns.
$offText

$gdxIn data_gravety model.gdx 
$load d_total
$GDXIN


*----Preparing Input Data----*

*Ensure that no selected construction is built by setting of each construction higher than the available budget*
c(v,i) = 1000000000;

*Filter of the parameter t, c, d and the Set E based on the user selection [v_subset]*
c(v,i)$(v_subset(v) and v_subset(i)) = c_total(v,i);
c_display(v,i)$(v_subset(v) and v_subset(i)) = c_total(v,i);
t(v,i)$(v_subset(v) and v_subset(i)) = t_total(v,i);
d(v,i)$(v_subset(v) and v_subset(i)) = d_total(v,i);
E(v, i)$(v_subset(v) and v_subset(i)) = yes;

*Calculation of the budget for the fully connected network
b_max = (sum ((v,i)$(v_subset(v) and v_subset(i)), c(v,i))) - card(v_subset) * 0.5;
b_max = b_max / 2

*Adapting c and t according to the selection of the train type*
*Assuming that the travel time in a regional express (RE) doubles and the construction costs are halved*

If (subtraintype('RE'),
    c(v,i)$(v_subset(v) and v_subset(i)) = c(v,i)* 0.5;
    t(v,i)$(v_subset(v) and v_subset(i)) = t(v,i) * 2;
    b_max = b_max * 0.5;
);


*----Optimization Model----*

*--Variables--*

Binary variable
y(i,j) 1 if the connection i to j is built 0 otherwise;

Free variable
G value of the objective function;

Integer variable
x(u,i,j) number of passengers starting their trip in u;


*--Formulation of objective function and constraints--*

Equation objectivefunction;
objectivefunction..G=e=sum((i,j)$(E(i,j)),t(i,j)*sum(u,X(u,i,j)));

Equation objectivefunction2;
objectivefunction2..G=e=sum((i,j)$(E(i,j)),c(i,j)*y(i,j));

Equation flow;
flow(u,v)$(ord(u)<>ord(v))..sum((i)$(E(i,v)),X(u,i,v))-sum((j)$(E(v,j)),X(u,v,j))=e=d(u,v);

Equation coupling;
Coupling(u,i,j)$(E(i,j) and ord(i)< ord(j))..X(u,i,j)+x(u,j,i)=l=d(u,u)*y(i,j);

Equation budget;
budget..sum((i,j)$(E(i,j) and ord(i)<ord(j)),c(i,j)*y(i,j))=l=b_min;

Model firstModel /objectivefunction,flow,coupling, budget/;

Model approxspanningtree /objectivefunction2,flow,coupling/;

option optCR=0;

Solve approxspanningtree min G using mip;

b_min_display = G.l;
b_min = max(B,G.l*1);

Solve firstmodel min G using mip;


*----Output preparation----*

*Sum of all construction costs *
totalcost = sum((v,i)$(E(i,v)), c(v,i)*y.l(v,i));

*Sum of travelling passengers for each connection*
travpass(v,i)=sum(u, X.l(u,v,i));

*Sum of all travelling passengers*
totaltravelers = sum ((v,i), d(v,i));
totaltravelers = totaltravelers - sum(v,d(v,v));

*Copy objective value to parameter*
totaltraveltime = G.l;

*Copy decision variable to parameter*
buildedge (v,i)$(v_subset(v) and v_subset(i)) = y.l(v,i);

*Preparation of the in map required parameters*
Set mapHdr / lats, longs, latz, longz, travpass, inhabitants/;
Table map(v,i, mapHdr);

*Transfering information of geodata and travelling passengers to map *
map(v,i,'lats')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = lat(v);
map(v,i,'longs')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = long(v);
map(v,i,'latz')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = lat(i);
map(v,i,'longz')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = long(i);
map(v,i,'travpass')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = travpass(v,i);
map(v,i,'inhabitants')$(y.l (v,i) >= 0.9 or y.l (i,v) >= 0.9) = d(v,v);

*Display of important variables & parameters for error detection
display buildedge, y.l, totalcost, c_display;


*----MIRO Output----*

*Forwarding information between $onExternalOutput and $offExternalOutput to GAMS MIRO*
$onExternalOutput
Table     map(v,i, mapHdr);
Parameter totalcost;
Parameter totaltraveltime;
Parameter buildedge (v,i);
Parameter c_display;
Parameter d(v,i);
Parameter t(v,i);
Parameter travpass(i,j);
Parameter totaltravelers;
Parameter b_max;
Parameter b_min_display;
$offExternalOutput

execute_unload "model.gdx";