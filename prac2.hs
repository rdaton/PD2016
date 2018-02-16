--ejercicio1
cuadrados = map (\x -> x^2) [0..50]

invertirLista [] = []
invertirLista(x:xs) = invertirLista xs ++ [x]

cuadradosInv=  zip (invertirLista ((map (\x -> x^2) [0..50]))) (invertirLista ([0..50]))
cuadradosInv2= zip (reverse ((map (\x -> x^2) [0..50]))) (reverse [0..50])


misum = foldr (+) 0
sumaSenosAbsolutos=  misum (map (\x ->x*(sin x)) [1..100])


acabaEn67 x 
	| ((x `mod` 10) == 7 && (((x div 10) `mod` 10)) ==6) = True
	| True = False

menoresQue= takeWhile (<10^10) (iterate (*3) 1)
sesentaYsiete= filter acabaEn67 menoresQue
