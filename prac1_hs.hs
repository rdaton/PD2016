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
f' x y =(-) ((*)2 x) ((*)y x)
g x =  f (f 2 x)  (f x 1)
h x y z =  f (f (x+2*y) (g 3)) (5 - (g z) - y)
i x y = 
       if (x>=y) && (y>0)  then x-y else
       if (y>x) && (y>0) then 0
       else y-x

i' x y 
       | (x>=y) && (y>0) = x-y 
       | (y>x) && (y>0) = 0
       | True = y-x
       
--ejercicio 3

tresIguales x y z 
	| (x==y) && (y==z) = True
	| True 			 = False
	
distintos x y z = not (tresIguales x y z)

--ejercicio 4

digitos x  = 
	if ((x `div` 10) > 0) then  
		(1 + digitos (x `div` 10))
	else 1

--ejercicio 5

fac:: (Eq a, Num a) => a -> a


fac n = 
	if n == 0 then 1 
	else n * fac (n-1)

perm n =
	fac n
	
var n m =
	perm n
	/
	perm (n-m)
	
--ejercicio 6
fib :: Int -> Integer
fib n
	| (n==0) = 1
	| (n==1) = 1
	| True 	 = fib (n-1) + fib (n-2)

--ejercicio 7
y :: Bool -> Bool -> Bool
y True a = a
y b True = b
y _ _ = False

--ejercicio 9

unaFuncLaxa :: a Bool -> a

unaFuncLaxa x y 
	| (y==True)
