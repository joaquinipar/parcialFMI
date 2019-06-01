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

-- Punto 2

aFloat :: Int -> Float
aFloat = fromIntegral

disminuirPorcentaje :: Int -> Float -> Float
disminuirPorcentaje porcentaje numero = numero - (\ porcentaje numero -> (aFloat(porcentaje) * 100) / numero ) porcentaje numero

recesion :: Float -> IPC -> Float
recesion despedidos ipc | despedidos > 100 = disminuirPorcentaje 20 ipc
                                           | otherwise = disminuirPorcentaje 15 ipc

prestar :: Float -> Pais -> Pais
prestar millones pais = pais {deuda = (deuda pais) + (*1.5) millones}

reducirEmpleadosPublicos :: Float -> Pais -> Pais
reducirEmpleadosPublicos despedidos pais = pais {empleadosPublicos = (empleadosPublicos pais) - despedidos,
    ipc = recesion despedidos (ipc pais) }

darExplotacion :: RecursoNatural -> Pais -> Pais 
darExplotacion recursoNatural pais = pais {deuda = (deuda pais) - 2, 
    recursosNaturales = filter (/=recursoNatural) (recursosNaturales pais)}   

pbi :: Pais -> Float
pbi pais = (ipc pais) * (empleadosPublicos pais + empleadosPrivados pais)

blindaje :: Pais -> Pais
blindaje pais = pais {deuda = (deuda pais) + (pbi pais)/2,empleadosPublicos = (empleadosPublicos pais) - 500 }

receta1 :: Pais -> Pais
receta1 = (darExplotacion "mineria").(prestar 200)

receta1ANamibia = receta1 namibia

