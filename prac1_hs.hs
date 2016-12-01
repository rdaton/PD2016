--ejercicio 1
segEnAnyo x = x * 31536000 -- 365 * 24 * 60 * 60
edadUniverso= 13700 * 10^6 
petaFlop = 10^15
segEnDia x = x * 86400 --(24 * 60 * 60)
prac1a = (petaFlop * 93 * segEnAnyo edadUniverso)::Double

anyosEnSeg x = x `div` segEnAnyo 1

prac1b x = anyosEnSeg x

segRestantesAnyo x = x - (segEnAnyo (anyosEnSeg x))

diasEnSeg x = (segRestantesAnyo x) `div` (segEnDia 1)
segRestantesDia x = segRestantesAnyo x - segEnDia (diasEnSeg x) 

horasEnSeg x = (segRestantesDia x) `div` (3600)
segRestantesHora x = segRestantesDia x - (3600 * horasEnSeg x)

minutosEnSeg x = (segRestantesHora x) `div` 60
segRestantesMinutos x = segRestantesHora x- (60* minutosEnSeg x)



prac1c x = (anyosEnSeg x, diasEnSeg x, horasEnSeg x,minutosEnSeg x,segRestantesMinutos x)

--ejercicio 2
f x y = 2*x - y*x
g x =  f (f 2 x)  (f x 1)
h x y z =  f (f (x+2*y) (g 3)) (5 - (g z) - y)
i x y = 
	if

