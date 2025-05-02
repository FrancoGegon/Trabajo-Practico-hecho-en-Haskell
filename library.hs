module Library where
import PdePreludat

data Ciudad = UnaCiudad {
                        nombre :: String,
                        añoFundacion :: Number,
                        atraccionesPrincipales :: [String],
                        costoDeVida :: Number
} deriving(Show, Eq, Ord)

baradero :: Ciudad
baradero = UnaCiudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150

nullish :: Ciudad
nullish = UnaCiudad "Nullish" 1800 [] 140

caletaOlivia :: Ciudad
caletaOlivia = UnaCiudad "Caleta Olivia" 1901 ["El Gorosito", "Faro Costanera"] 120

maipu :: Ciudad
maipu = UnaCiudad "Maipú" 1878 ["Fortín Kakel"] 115

azul :: Ciudad
azul = UnaCiudad "Azul" 1832 ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"] 190

-- Punto 1: Valor de una ciudad

noHayAtracciones :: Ciudad -> Bool
noHayAtracciones ciudad = null (atraccionesPrincipales ciudad)

valorDeUnaCiudad :: Ciudad -> Number
valorDeUnaCiudad ciudad
    | añoFundacion ciudad < 1800 = 5 * (1800 - añoFundacion ciudad)
    | noHayAtracciones ciudad = 2 * costoDeVida ciudad
    | otherwise = 3 * costoDeVida ciudad

-- Punto 2: Características de las ciudades

--Alguna atracción copada

comienzaConUnaVocal :: String -> Bool
comienzaConUnaVocal atraccion = head atraccion `elem` "aeiouAEIOU"

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada ciudad = any comienzaConUnaVocal (atraccionesPrincipales ciudad)

--Ciudad sobria

esCiudadSobria :: Number -> Ciudad -> Bool
esCiudadSobria numero ciudad
    | noHayAtracciones ciudad = False
    | otherwise = all (esMayor numero) (atraccionesPrincipales ciudad)

esMayor :: Number -> String -> Bool
esMayor numero palabra = length palabra > numero

--Ciudad Con Nombre Raro

tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro ciudad = length (nombre ciudad) < 5

-- Punto 3: Eventos

type Eventos = Ciudad -> Ciudad

sumarNuevaAtraccion :: String -> Eventos
sumarNuevaAtraccion atraccionNueva ciudad =
    ciudad {costoDeVida = costoDeVida ciudad * 1.2,
            atraccionesPrincipales = atraccionesPrincipales ciudad ++ [atraccionNueva]}

crisis :: Eventos
crisis ciudad
    | noHayAtracciones ciudad = ciudad {costoDeVida = costoDeVida ciudad - costoDeVida ciudad * 0.1}
    | otherwise = ciudad {costoDeVida = costoDeVida ciudad - costoDeVida ciudad * 0.1,
            atraccionesPrincipales = init (atraccionesPrincipales ciudad)}

remodelacion :: Number -> Eventos
remodelacion number ciudad =
    ciudad {nombre = "New " ++ nombre ciudad,
            costoDeVida = costoDeVida ciudad + costoDeVida ciudad * (number / 100)}

reevaluacion :: Number -> Eventos
reevaluacion numero ciudad
    | esCiudadSobria numero ciudad = ciudad { costoDeVida = costoDeVida ciudad * 1.10}
    | otherwise = ciudad {costoDeVida = costoDeVida ciudad - 3 }

-- Punto 4: La transformación no para

unaTransformacion:: Number -> Number -> Eventos
unaTransformacion nletras porcentaje = reevaluacion nletras . crisis . remodelacion porcentaje

-- Punto 4.1: Los años pasan...
data Año = UnAño{
    año :: Number,
    eventos :: [Eventos]
}

año2022 :: Año
año2022 = UnAño 2022 [crisis, remodelacion 5, reevaluacion 7]

año2015 :: Año
año2015 = UnAño 2015 []

reflejarAño :: Año -> Ciudad -> Ciudad
reflejarAño año ciudad = foldl aplicarEvento ciudad (eventos año)

aplicarEvento :: Ciudad -> Eventos -> Ciudad
aplicarEvento ciudad evento = evento ciudad

type Criterio = Ciudad -> Number

--Punto 4.2: Algo mejor

huboMejora :: Ciudad -> Criterio -> Eventos -> Bool
huboMejora ciudad criterio evento = criterio (evento ciudad) > criterio ciudad

cantidadAtracciones :: Ciudad -> Number
cantidadAtracciones = length.atraccionesPrincipales --para usarla como criterio

--Punto 4.3: Costo de vida que suba

costoDeVidaQueSuba :: Año -> Ciudad -> Ciudad
costoDeVidaQueSuba año ciudad = foldl (aplicarSiSube costoDeVida) ciudad (eventos año)

--Punto 4.4: Costo de vida que baje

costoDeVidaQueBaje :: Año -> Ciudad -> Ciudad
costoDeVidaQueBaje año ciudad = foldl (aplicarSiSube (negate.costoDeVida)) ciudad (eventos año)

--Punto 4.5: Valor que suba

subeValor :: Año -> Ciudad -> Ciudad
subeValor año ciudad = foldl (aplicarSiSube valorDeUnaCiudad) ciudad (eventos año)

aplicarSiSube :: Criterio -> Ciudad -> Eventos -> Ciudad
aplicarSiSube criterio ciudad evento
    | criterio (evento ciudad) > criterio ciudad = evento ciudad
    | otherwise = ciudad

--Punto 5: Funciones a la orden

{-ACLARACION: EL EJERCICIO 5 ESTÁ RESUELTO BAJO LA INTERPRETACIÓN DE QUE LOS EVENTOS SE VAN APLICANDO SUCESIVAMENTE.
ESTO SIGNIFICA QUE, POR EJEMPLO, DADA UNA LISTA DE AÑOS APLICADA A UNA CIUDAD, SE VA A IR CALCULANDO EL COSTO DE VIDA DE
DICHA CIUDAD SOBRE EL RESULTADO DE APLICAR EL EVENTO ANTERIOR.-}

estaOrdenada :: [Number] -> Bool
estaOrdenada [] = False
estaOrdenada [lista] = True
estaOrdenada (lista:listaMenosUno) | lista < head listaMenosUno = estaOrdenada listaMenosUno | lista > head listaMenosUno = False

--Punto 5.1: Eventos ordenados

estanOrdenadosEventos :: Año -> Ciudad -> Bool
estanOrdenadosEventos año ciudad = estaOrdenada (scanl (\costo evento -> costoDeVida (evento ciudad)) (costoDeVida ciudad) (eventos año)) 

--Punto 5.2: Ciudades ordenadas

estanOrdenadasCiudades :: [Ciudad] -> Eventos -> Bool
estanOrdenadasCiudades ciudades evento = estaOrdenada (map (costoDeVida . evento ) ciudades)

listaDeCiudades1 = [baradero, caletaOlivia, maipu, azul]
listaDeCiudades2 = [maipu, caletaOlivia, baradero]

--Punto 5.3: Años ordenados

estanOrdenadosAños :: [Año] -> Ciudad -> Bool
estanOrdenadosAños años ciudad = estaOrdenada (map costoDeVida (scanl (flip reflejarAño) ciudad años))

año2021 :: Año
año2021 = UnAño 2021 [crisis, sumarNuevaAtraccion "playa"]

año2023 :: Año
año2023 = UnAño 2023 [crisis, sumarNuevaAtraccion "parque", remodelacion 10, remodelacion 20]

listaAños1 = [año2021, año2022, año2023]
listaAños2 = [año2021]
listaAños3 = []

{- Punto 6: Al infinito y más allá... 

Definimos un año con eventos infinitos: -}

año2024 :: Año
año2024 = UnAño {año = 2024, eventos = repeat crisis} 

{- 6.1) sí, puede haber un resultado si, por ejemplo, el costo de vida de la ciudad tras el primer evento 
(de la lista infinita de eventos) es mayor que el costo de vida tras el segundo evento; con lo cual la lista
NO está ordenada, y la función devuelve False sin necesidad de recorrer la lista entera.

6.2) sí podría dar un resultado ya que la función 5.2 no necesita de un año para funcionar, sólo de un evento, 
por lo tanto no sería necesario usar el año 2024 para hacer lo que el punto pide.

6.3) no habría un resultado para este ya que no podría terminar de comparar los valores de los costos de vida de la ciudad 
pues son infinitos; sin embargo, como haskell tiene lazy evaluation es posible arrojar un resultado únicamente en el caso de que, 
como en el 6.1), los costos de vida no estén ordenados, por lo que devolvería False sin necesidad de recorrer la lista entera. -}
