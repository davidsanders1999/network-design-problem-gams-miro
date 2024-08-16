Set
v set of notes /city1, city2, city3, city4, city5/
E(v, v) set of edges
/city1.city2
city1.city3
city2.city1
city2.city3
city2.city4
city3.city1
city3.city2
city3.city4
city3.city5
city4.city2
city4.city3
city4.city5
city5.city3
city5.city4/;

alias(v,i,j,u);

$onExternalInput
Parameters
d(u,v) expected number of passengers travelling from node u to v
B Budget
t(i,j) value of travel time for the direct trip from i to j
/
city1.city2 =  30
city2.city1 =  30
city1.city3 = 20
city3.city1 = 20
city2.city3 = 20
city3.city2 = 20
city2.city4 = 50
city4.city2 = 50
city3.city4 = 40
city4.city3 = 40
city3.city5 = 30
city5.city3 = 30
City4.City5 = 30
City5.City4 = 30/
;
Parameters
c(u,v) fixed cost
/
city1.city2 =  7000
city2.city1 =  7000
city1.city3 =  6000
city3.city1 =  6000
city2.city3 =  3000
city3.city2 =  3000
city2.city4 =  11000
city4.city2 =  11000
City3.City4 =  9000
City4.City3 =  9000
city3.city5 =  5000
city5.city3 =  5000
city4.city5 =  7000
city5.city4 =  7000/

B maximum total cost allowed /23000/;



Table
d(u,v)
         city1  city2  city3  city4  city5
city1    900     250     400     100     150
city2    250    1400     500     250     400
city3    400     500    1300     300     100
city4    100     250     300     800     150
city5    150     400     100     150     800;

$offExternalInput

$onExternalOutput
Binary variable
y(i,j) 1 if the connection i to j is built 0 otherwise;

Free variable
G value of the objective function;


Integer variable
x(u,i,j) number of passengers starting their trip in;

$offExternalOutput

Equation objectivefunction;
objectivefunction..G=e=sum((i,j)$(E(i,j)),t(i,j)*sum(u,X(u,i,j)));


Equation flow;
flow(u,v)$(ord(u)<>ord(v))..sum((i)$(E(i,v)),X(u,i,v))-sum((j)$(E(v,j)),X(u,v,j))=e=d(u,v);

Equation coupling;
Coupling(u,i,j)$(E(i,j) and ord(i)< ord(j))..X(u,i,j)+x(u,j,i)=l=d(u,u)*y(i,j);

Equation budget;
budget..sum((i,j)$(E(i,j) and ord(i)<ord(j)),c(i,j)*y(i,j))=l=B;

Model firstModel /all/;
Solve firstmodel min G using mip;

Display v,E,d,t,c, x.l, y.l;
execute_unload "model.gdx";
