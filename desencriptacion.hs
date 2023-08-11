import Data.Char (ord, chr)

-- Aqui pongo funciones que son de utilidad
esPrimo :: Integer -> Bool
esPrimo 1 = False 
esPrimo n = n == menorDivisor n 

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n | esPrimo n = n
                       | otherwise = minimoPrimoDesde (n+1)

eNesimoPrimo :: Integer -> Integer
eNesimoPrimo 1 = 2
eNesimoPrimo n = minimoPrimoDesde (1 + eNesimoPrimo (n-1 ))

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                          | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

signo :: Integer -> Integer 
signo a = div a (abs a)

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (abs a, signo a, 0)
emcd a b = (g, s, t)
  where (g, s', t') = emcd b (mod a b) 
        q = div a b 
        s = t'
        t = s' - (q*t') 

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b | a < b = mcd b a 
        | otherwise = mcd b (mod a b)

ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

solucionEcConPropAdic :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m 

solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

--Recuerdo: String es lo mismo que [Char].
{- Aqui no hay mucho misterio, lanzo un mensaje de error (consulte por email y me dijeron que podia hacerlo) 
en dado caso que alguno de los dos valores no sea primo o que el producto entre ellos dos de un numero menor igual a 127. 
Tambien me parecio oportuno poner el error avisando que es necesario poner un p y un q distinitos. Luego lo demas es por definicion -}
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | not (esPrimo p) || not (esPrimo q) = error "Alguno de los numeros entregados no es primo"
                  | p == q = error "Debe darsele a la funcion dos primos diferentes"
                  | n <= 127 = error "La funcion necesita un p y un q donde su producto sea mayor a 127" 
                  | otherwise = ((n,d), (n,e)) 
 where n = p*q  
       e = numeroE p q 
       d = conseguirD p q 
 
{-Consigo m a partir de la definicion. m=(p-1)*(q-1) -}
conseguirM :: Integer -> Integer -> Integer
conseguirM p q = (p-1)*(q-1)

{- Para facilitarme el tramite de buscar un e, lo que hice fue ir buscando el primer coprimo con m menor a m-2, 
partiendo desde M, siendo entonces el coprimo mas grande de M que responde a la definicion dada -}

numeroE :: Integer -> Integer -> Integer
numeroE p q = evaluarEConAlgunM (conseguirM p q) (conseguirM p q) 

evaluarEConAlgunM :: Integer -> Integer -> Integer 
evaluarEConAlgunM x m | x > (m-2) = evaluarEConAlgunM (x-1) m 
                      | mcd m x == 1 = x
                      | otherwise =  evaluarEConAlgunM (x-1) m 

 {- Aqui use tambien la definicion, plantee la ecuacion con modulo, tambien simplifique pasos usando el where.
 La funcion hallarX va buscando el menor x que cumpla la ecuacion del modulo, o sea que cumpla el 
 resto y modulo que deje la funcion solucionEc (hecha en la clase 9), parto de 1 y voy buscando hasta encontrarlo. 
 Tambien creo oportuno agregar que el 1 representa a la ecuacion por def (eX=1 mod (m))-}
conseguirD :: Integer -> Integer -> Integer 
conseguirD p q = hallarXApartirDeEcuConModulo 1 (solucionEc(e,1,m))
 where e = numeroE p q 
       m = conseguirM p q 

hallarXApartirDeEcuConModulo :: Integer -> (Integer, Integer) -> Integer 
hallarXApartirDeEcuConModulo x (r,m) | mod x m == r = x 
                                     | otherwise = hallarXApartirDeEcuConModulo (x+1) (r,m) 
{-Ningun misterio, se sigue el orden por definicion. Lo que hace es traducir la letra y pasarla a numero 
con ord, pasarlo a Integer con fromIntegral, elevarlo a d, sacar el modulo de eso con n y agregar ese numero
a la recursion buscando las cabezas de las colas siguientes. Luego, cuando no quede mas mensaje que encriptar 
devuelvo la lista vacia.
Vease que '' representa a un solo char pero "" representa a una "lista" de char, que seria la String. 
Por eso usa head mensaje, porque al ser una lista de char, puedo buscar su cabeza y su cola-}
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar (n,d) [] = [] 
encriptar (n,d) mensaje = mod (fromIntegral ((ord (head mensaje))) ^ d) n : encriptar (n,d) (tail (mensaje))
{-Aqui lo mismo, es por definicion, buscando el modulo n de b elevado a e. Luego paso eso a char y lo agrego
a la cola de la lista de numeros codificados, hasta encontrar la lista vacia y devolver una string vacia ""-}
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar (n,e) [] = ""
desencriptar (n,e) (b:bs) =  chr (fromInteger (mod (b^e) n)) : desencriptar (n,e) bs 

{- A partir de aca comienza el metodo para romper el codigo que hice, el comentario encriptado esta debajo de la funcion romperCodigo-}

{- Me traje de regalo mas funciones de la clase 9 para ayudarme a buscar una factorizacion de primos. Esta basiscamente
busca todos los divisores de un numero -}
divisoresDesdeM :: Integer -> Integer -> [Integer] 
divisoresDesdeM n m    | m == 1 = [1] 
                       | mod n m == 0 =  m : divisoresDesdeM n (m-1)
                       | otherwise = divisoresDesdeM n (m-1)

divisores :: Integer -> [Integer] 
divisores n = divisoresDesdeM n n 
 {- Esta funcion apoya a factorizarEnPrimos porque filtra todo numero no primo de la lista de divisores, 
 devolviendo la lista con los factores primos -}
separadorDePrimos :: [Integer] -> [Integer]
separadorDePrimos [] = []
separadorDePrimos (x:xs) | esPrimo x = x : separadorDePrimos xs 
                         | otherwise = separadorDePrimos xs 
{- Robandole la gloria a sus dos compañeras, esta funcion organiza el trabajo de las otras. Separa los primos
de los divisores de N, encontrando asi los factores de n que son primos, vitales para romper el codigo -}
factorizarEnPrimos :: Integer -> [Integer] 
factorizarEnPrimos n = separadorDePrimos (divisores n) 
{- Luego de encontrar obtenerE (que se obtendra mas adelante) a partir de n y d, lo unico que hago es 
desencriptar, con la n otorgada por la llave publica y con la e obtenida, el mensaje encriptado  -}
romperCodigo :: (Integer,Integer) -> [Integer] -> String 
romperCodigo (n,d) (x1:xs) = desencriptar (n,(obtenerE (n,d))) (x1:xs)

{- [10069,18800,77756,78982,18800,91658,91658,58255,77756,96593,58255,438,22839,28700,18800,1606,58255,77756,23881,220,77756,99961,58255,77756,41209,38913,97660,58255,91658,91658,23881,1606,1606,58255] -} 
-- La deje toda larga sin enter para que les sea mas facil a ustedes copiar y pegar en la consola 
{-Esta ayuda a obtener P y Q, se me ocurrio hacer esta funcion que, dandole la lista de factoresPrimosDeN
va buscando que factores de ese N, multiplicados entre si dan n. Si x1 y x2 (los dos primeros primos), son 
distintos a n, tiro a la basura x2, y busco en la cola si alguno multiplicado con x1 me da N. Si llego al final 
y x1 queda solita (significando que es igual a x2) entonces tiro x1, y ahora busco a partir de x2 de la lista original si hay algun primo
que multiplicado con el funcione. 
Si obtengo tal combinacion, devuelvo al x1 y x2. -}
obtenerPyQConFactores :: Integer -> [Integer] -> (Integer,Integer)
obtenerPyQConFactores n (x1:x2:xs) | x1==x2 = obtenerPyQConFactores n (x2:xs) 
                                   | x1*x2 /= n = obtenerPyQConFactores n (x1:xs) 
                                   | x1*x2 == n = (x1,x2) 
{- Esta lo que hace es completar el trabajo, le da la lista de factores primos de N a obtenerPyQConFactores, 
para que asi evalue con los factores primos en cuestion  -}
obtenerPyQ :: Integer -> (Integer, Integer)
obtenerPyQ n = obtenerPyQConFactores n (factorizarEnPrimos n)
{- Estas dos mini funciones lo que hace es darme el p o el q de la terna (p,q) -}
laP :: (Integer, Integer) -> Integer
laP (p,q) = p 

laQ :: (Integer, Integer) -> Integer
laQ (p,q) = q 

{- Por definicion saco M a partir de obtener p y q de N. Basicamente multiplico la p menos 1 con la q menos 1 -}
obtenerMAPartirDeN :: Integer -> Integer
obtenerMAPartirDeN n = ((laP (obtenerPyQ n))-1)*((laQ (obtenerPyQ n)-1))
{- Aqui obtengo E planteando una ecuacion parecida a como planteo con D antes. dX=1 mod(m)
Como ya tengo la M y la D la tengo por la llave publica, planteo la ecuacion similar 
a como busque D en "conseguirD" (siendo E la x en Dx=1 mod(m), y luego de despejar la X en el modulo, 
hallar una X que cumpla con lo requerido) -}
obtenerE :: (Integer,Integer) -> Integer 
obtenerE (n,d) =  hallarXApartirDeEcuConModulo 1 (solucionEc(d,1,m))
 where m = obtenerMAPartirDeN n 

primerosNPrimos :: Integer -> [Integer]
primerosNPrimos 0 = [] 
primerosNPrimos n = eNesimoPrimo n : primerosNPrimos (n-1)