fac     0 = 1
fac     n = n * fac(n-1)

digs 0 = []
digs x = digs (x `div` 10) ++ [ x `mod` 10]