PARAMETERS
    o(v)    Number of passengers entering station i
    Le(v)    Number of passengers leaving station j;
    

Parameter O(v) people entering that station (from the excel sheet) 
;
Parameter Le(j) people leaving that station (from the excel sheet) 
;


O(v)=inhabitants(v);
Le(v)=inhabitants(v);

FREE VARIABLE F;    
NONNEGATIVE VARIABLES
k, bo, bd, bt,  XO, XD, YO, YD ,ODV(i,j);

*INTEGER VARIABLES
*k, bo, bd, bt,  XO, XD, YO, YD ,ODV(i,j);

EQUATIONS
    matrix  od-matrix
    origin  satisfy supply
    dest    satisfy demand
    diag
    obj     objective
    ;
 
 
matrix(E(i, j)) $ (ord(i) <> ord(j) ) .. ODV(i,j) =E=  k * o(i)**bo  * Le(j)**bd  / t(i,j)**bt ;

origin(i).. sum(j$ (ord(j)<>ord(i)), ODV(i,j) ) =E= Le(i) + XO(i) - YO(i);
    
dest(j).. sum(i$ (ord(j)<>ord(i)), ODV(i,j) ) =E= o(j) + XD(j) - YD(j);

diag(i).. sum(j$ (ord(j)<>ord(i)), ODV(i,j) ) =E= inhabitants(i);

obj.. F =E= sum(i, XO(i) + XD(i) + YO(i) + YD(i) );

model odmatrix /matrix,origin,dest,diag,obj/;


solve odmatrix min F using NLP;


parameter OD_matrix(i, j), correctionPos(i),correctionNet(i), rnd;
OD_matrix(i, j) = ODV.l(i, j);
d(v,v)=inhabitants(v);
*d(v,i)=OD_matrix(v,i);
d(v,i)=ceil(OD_matrix(v,i));


loop(i,
    if(ord(i)<card(i),
        rnd=UniformInt(ord(i)+1,card(i));
    );
    
    if(ord(i)=card(i),
        rnd=UniformInt(1,card(i)-1);
    );
    
    correctionPos(i)=max(0,sum(j$ (ord(j)<>ord(i)),d(i,j))-inhabitants(i));
    d(i,j)$(ord(j)<>ord(i) and ord(j)=rnd)=d(i,j)-correctionPos(i);
    
    correctionNet(i)=max(0,inhabitants(i)-sum(j$ (ord(j)<>ord(i)),d(i,j)));
    d(i,j)$(ord(j)<>ord(i) and ord(j)=rnd)=d(i,j)+correctionNet(i);
);

d(v,v)=inhabitants(v);
display OD_matrix;

execute_unload "odmatrix.gdx" d;