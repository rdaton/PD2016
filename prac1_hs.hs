segEnAnyo x = x * 365 * 24 * 60 * 60
edadUniverso= 13700 * 10^6 
petaFlop = 10^15
prac1a = (petaFlop * 93 * segEnAnyo edadUniverso)::Double

prac1b x = x `div` segEnAnyo 1

segRestantesAnyo x = x - ((prac1b x) * segEnAnyo 1 )
diasEnSeg x = (segRestantesAnyo x) `div` (24 * 60 * 60)
diasRestantes x =   


prac1c x = (prac1b x, diasEnSeg () , 0, 0)