import Text.Show.Functions

type IPC = Float
type EmpleadosPublicos = Float
type EmpleadosPrivados = Float
type RecursoNatural = String
type Deuda = Float

-- Punto 1

data Pais = Pais {
    ipc :: IPC,
    empleadosPublicos :: EmpleadosPublicos,
    empleadosPrivados :: EmpleadosPrivados,
    recursosNaturales :: [RecursoNatural],
    deuda :: Deuda
} deriving (Show)

namibia = Pais{
    ipc = 4140.0,
    empleadosPublicos = 400000,
    empleadosPrivados = 650000,
    recursosNaturales = ["mineria","ecoturismo"],
    deuda = 50
}

argentina = Pais{
    ipc = 4140.0,
    empleadosPublicos = 400000,
    empleadosPrivados = 650000,
    recursosNaturales = ["mineria","petroleo"],
    deuda = 50
}

-- Punto 2

type Receta = (Pais -> Pais)

aFloat :: Int -> Float
aFloat = fromIntegral

disminuirPorcentaje :: Int -> Float -> Float
disminuirPorcentaje porcentaje numero = numero - (\ porcentaje numero -> (aFloat(porcentaje) * 100) / numero ) porcentaje numero

recesion :: Float -> Pais -> Float
recesion despedidos pais | despedidos > 100 = disminuirPorcentaje 20 (ipc pais)
                                           | otherwise = disminuirPorcentaje 15 (ipc pais)

prestar :: Float -> Receta
prestar millones pais = pais {deuda = (deuda pais) + (*1.5) millones}

reducirEmpleadosPublicos :: Float -> Receta
reducirEmpleadosPublicos despedidos pais = pais {empleadosPublicos = (empleadosPublicos pais) - despedidos,
    ipc = recesion despedidos pais }

darExplotacion :: RecursoNatural -> Receta
darExplotacion recursoNatural pais = pais {deuda = (deuda pais) - 2, 
    recursosNaturales = filter (/=recursoNatural) (recursosNaturales pais)}   

pbi :: Pais -> Float
pbi pais = (ipc pais) * (empleadosPublicos pais + empleadosPrivados pais)

blindaje :: Receta
blindaje pais = pais {deuda = (deuda pais) + (pbi pais)/2,empleadosPublicos = (empleadosPublicos pais) - 500 }

receta1 :: Receta
receta1 = (darExplotacion "mineria").(prestar 200)

receta1ANamibia :: Pais
receta1ANamibia = receta1 namibia

-- Punto 4

seSalvan :: [Pais] -> [Pais]
seSalvan listaPaises = filter ((elem "petroleo").recursosNaturales)  listaPaises   

deudaTotal :: [Pais] -> Float
deudaTotal = sum.(map deuda)

-- Orden superior: en seSalvan, filter es una funcion de Orden Superior
-- Composicion: en deudaTotal
-- Aplicación Parcial: en (elem "petroleo")


-- Punto 5

aplicarReceta :: [Receta] -> Pais -> Pais
aplicarReceta listaRecetas pais = foldl (\pais receta -> receta pais) pais listaRecetas
--                                                               namibia  [prestar 2,darExplotacion "petroleo"]


recetasOrdenadas :: Pais -> [Receta] -> Bool
recetasOrdenadas pais [receta] = True

recetasOrdenadas pais (receta1:receta2:restoDeRecetas) = 
    (checkearPBI receta2 pais >= checkearPBI receta1 pais) && recetasOrdenadas pais restoDeRecetas

checkearPBI :: Receta -> Pais-> Float
checkearPBI receta = pbi.receta

destruirPBI pais = pais {ipc = 0}
