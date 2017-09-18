% Smith Family Vacation
start(P):- P=[Dad,Mom,Matt,John,Tim],
    %If dad comes, mom will come too
    ((Dad=y;Mom=y), Dad=n),
    %At least one of Matt or John will come
    (Matt=y; John=y),
    %Either Mom or Tim will come, not both
    (   Mom=y; Tim=n),
    (   Mom=n; Tim=y),
    %either Tim and John will both come or neither will come
    Tim=John,
    %If Matt comes, so will John and Dad
    (   Matt=n;(   Matt=y; John=y; Dad=y)).