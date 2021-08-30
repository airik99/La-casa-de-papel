import Text.Show.Functions
import Data.List

data Ladron = Ladron {
    nombreLadron :: String,
    habilidades :: [String],
    armas :: [Arma]
} deriving (Show)

type Arma = Rehen -> Rehen

data Rehen = Rehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    planes :: [String]
} deriving (Show, Ord, Eq)

modificarNombreLadron :: (String -> String) -> Ladron -> Ladron
modificarNombreLadron f ladron = ladron {nombreLadron = f . nombreLadron $ ladron} 

modificarHabilidades :: ([String] -> [String]) -> Ladron -> Ladron
modificarHabilidades f ladron = ladron {habilidades = f . habilidades $ ladron}

modificarArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
modificarArmas f ladron = ladron {armas = f . armas $ ladron}

modificarNombreRehen :: (String -> String) -> Rehen -> Rehen
modificarNombreRehen f rehen = rehen {nombreRehen = f . nombreRehen $ rehen}

modificarNivelComplot :: (Int -> Int) -> Rehen -> Rehen
modificarNivelComplot f rehen = rehen {nivelComplot = f . nivelComplot $ rehen}

modificarNivelMiedo :: (Int -> Int) -> Rehen -> Rehen
modificarNivelMiedo f rehen = rehen {nivelMiedo = f . nivelMiedo $ rehen}

modificarPlanes :: ([String] -> [String]) -> Rehen -> Rehen
modificarPlanes f rehen = rehen {planes = f . planes $ rehen}

---------- PUNTO 1 ---------- 

tokio :: Ladron
tokio = Ladron "Tokio" ["hacer trabajo psicologico", "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 ["esconderse"]

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 ["esconderse", "atacar con pablo"]

-- ARMAS --

pistola :: Int -> Arma
pistola calibre rehen = (aumentarMiedoEn (3 * cantidadLetrasNombreRehen rehen)) . reducirNivelComplot (subtract calibre) $ rehen

ametralladora :: Int -> Arma
ametralladora balas = (aumentarMiedoEn balas) . reducirNivelComplot (div 2)

reducirNivelComplot :: (Int -> Int) -> Rehen -> Rehen
reducirNivelComplot unaCantidad = modificarNivelComplot unaCantidad

aumentarMiedoEn :: Int -> Rehen -> Rehen
aumentarMiedoEn unaCantidad rehen = modificarNivelMiedo (+ unaCantidad) rehen

cantidadLetrasNombreRehen :: Rehen -> Int
cantidadLetrasNombreRehen = length . nombreRehen

---------- PUNTO 2 ---------- 

esInteligente :: Ladron -> Bool
esInteligente ladron = ((> 2) . cantidadDeHabilidades $ ladron) || (esElProfesor . nombreLadron $ ladron)

cantidadDeHabilidades :: Ladron -> Int
cantidadDeHabilidades = length . habilidades

esElProfesor :: String -> Bool
esElProfesor "Profesor" = True
esElProfesor _          = False

---------- PUNTO 3 ---------- 

agregarArma :: Arma -> Ladron -> Ladron
agregarArma arma = modificarArmas (arma :)

---------- PUNTO 4 ---------- 
---------- DISPAROS ---------- 

disparos :: Ladron -> Rehen -> Rehen
disparos ladron rehen = ($ rehen) . armaQueLeDaMasMiedo rehen $ ladron

armaQueLeDaMasMiedo :: Rehen -> Ladron -> Rehen -> Rehen
armaQueLeDaMasMiedo rehen = maximoSegun (nivelMiedo . ($ rehen)) . armas

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun condicion = foldl1 (maximoAplicandoCondicion condicion)

maximoAplicandoCondicion :: Ord b => (a -> b) -> a -> a -> a
maximoAplicandoCondicion f x y | f x > f y = x
                               | otherwise = y

---------- HACERSE EL MALO ---------- 

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo ladron | esBerlin (nombreLadron ladron) = aumentarMiedoEn (cantidadDeLetrasDeHabilidades ladron)
                     | esRio (nombreLadron ladron)    = aumentarComplotEn 20
                     | otherwise                      = aumentarMiedoEn 10

esBerlin :: String -> Bool
esBerlin "Berlin" = True
esBerlin _        = False

esRio :: String -> Bool
esRio "Rio" = True
esRio _     = False

cantidadDeLetrasDeHabilidades :: Ladron -> Int
cantidadDeLetrasDeHabilidades = length . concat . habilidades

aumentarComplotEn :: Int -> Rehen -> Rehen
aumentarComplotEn unaCantidad = modificarNivelComplot (+ unaCantidad)

---------- PUNTO 5 ---------- 

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron = losQueTienenComplotMasDe 60 . map (disparos ladron)

losQueTienenComplotMasDe :: Int -> [Rehen] -> [Rehen]
losQueTienenComplotMasDe unaCantidad = filter ((> unaCantidad) . nivelComplot)

rehenes :: [Rehen]
rehenes = [pablo, arturito, facundo]

facundo :: Rehen
facundo = Rehen "Facundo" 200 100 []

camila :: Ladron
camila = Ladron "Camila" [] [pistola 2]

---------- PUNTO 6 ---------- 

puedeEscaparseDeLaPolicia :: Ladron -> Bool
puedeEscaparseDeLaPolicia = habilidadEmpiezaCon "disfrazarse de" . habilidades

habilidadEmpiezaCon :: String -> [String] -> Bool
habilidadEmpiezaCon unaFrase habilidades = elem unaFrase (map (take 14) habilidades)
