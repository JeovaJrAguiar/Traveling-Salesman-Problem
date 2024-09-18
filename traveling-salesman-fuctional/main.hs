import Data.List (permutations, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

-- Definição dos tipos de dados, no caso, vértice, aresta e grafo
-- Definição do vértice
data Vertex = Vertex {
    idVertex :: Int,
    name :: String,
    xAxis :: Int,
    yAxis :: Int
} deriving (Show)

-- Definição da aresta
data Edge = Edge {
    weight :: Double,
    prev :: Vertex,
    next :: Vertex
} deriving (Show)

-- O grafo consiste de uma lista de vértices e uma lista de arestas
data Graph = Graph {
    vertices :: [Vertex],
    edges :: [Edge]
} deriving (Show)




-- Insere vértice
insertVertex :: Graph -> Vertex -> Graph
insertVertex (Graph vs es) v = Graph (v : vs) es

-- Insere aresta
insertEdge :: Graph -> Edge -> Graph
insertEdge (Graph vs es) e = Graph vs (e : es)

-- Distância euclidiana entre dois vértices
euclideanDistance :: Vertex -> Vertex -> Double
euclideanDistance v1 v2 = sqrt $ fromIntegral ((xAxis v2 - xAxis v1)^2 + (yAxis v2 - yAxis v1)^2)

-- Aresta com distância euclidiana
createEdge :: Vertex -> Vertex -> Edge
createEdge v1 v2 = Edge (euclideanDistance v1 v2) v1 v2

-- Encontrz o ciclo hamiltoniano mais curto, aqui usa a estratégia de força bruta
findHamiltonianCycle :: [Vertex] -> [Edge]
findHamiltonianCycle vs = minimumBy (comparing totalWeight) cycles
  where
    allPermutations = permutations vs
    cycles = map (\p -> zipWith createEdge p (tail p ++ [head p])) allPermutations
    totalWeight edges = sum (map weight edges)

main :: IO ()
main = do
    -- Cria os vértices
    let v1 = Vertex {idVertex = 1, name = "Cidade1", xAxis = 10, yAxis = 50}
    let v2 = Vertex {idVertex = 2, name = "Cidade2", xAxis = 50, yAxis = 50}
    let v3 = Vertex {idVertex = 3, name = "Cidade3", xAxis = 50, yAxis = 20}
    let v4 = Vertex {idVertex = 4, name = "Cidade4", xAxis = 50, yAxis = 20}
    let v5 = Vertex {idVertex = 5, name = "Cidade5", xAxis = 10, yAxis = 10}

    -- Cria o grafo e inserindo elementos
    let g = Graph {vertices = [], edges = []}
    let g1 = insertVertex g v1
    let g2 = insertVertex g1 v2
    let g3 = insertVertex g2 v3
    let g4 = insertVertex g3 v4
    let g5 = insertVertex g4 v5

    -- Cria as arestas
    let e1 = createEdge v1 v2
    let e2 = createEdge v2 v3
    let e3 = createEdge v3 v4
    let e4 = createEdge v4 v5
    let e5 = createEdge v5 v1

    -- Insere arestas no grafo
    let gWithEdges = insertEdge (insertEdge (insertEdge (insertEdge (insertEdge g5 e1) e2) e3) e4) e5

    -- Encontra o ciclo hamiltoniano mais curto
    let verticesList = vertices gWithEdges
    let cycle = findHamiltonianCycle verticesList

    -- Esibindo o grafo e o ciclo hamiltoniano mais curto
    print gWithEdges
    print cycle
