a 0 n = n + 1
a 1 n = n + 2
a 2 n = 2*n+3
a 3 n = 2^(n+3)-1

a m n = if m == 0 then  n+1
        else if m > 0 && n == 0 then a (m-1) 1
        else a (m-1) ( a (m) (n-1))

