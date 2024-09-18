-- Definição do vértice
data Vertex = Vertex {
    idVertex :: Int,
    name :: String,
    xAxis :: Int,
    yAxis :: Int
} deriving (Show)

-- Definição da aresta
data Edge = Edge {
    weight :: Int,
    prev :: Vertex,
    next :: Vertex
} deriving (Show)

-- O grafo consiste de uma lista de vértices e uma lista de arestas
data Graph = Graph {
    vertices :: [Vertex],
    edges :: [Edge]
} deriving (Show)

-- Inserir vértice
insertVertex :: Graph -> Vertex -> Graph
insertVertex (Graph vs es) v = Graph (v : vs) es

-- Inserir aresta
insertEdge :: Graph -> Edge -> Graph
insertEdge (Graph vs es) e = Graph vs (e : es)

main :: IO ()
main = do
    -- Criando alguns vértices
    let v1 = Vertex {idVertex = 1, name = "A", xAxis = 0, yAxis = 0}
    let v2 = Vertex {idVertex = 2, name = "B", xAxis = 1, yAxis = 1}

    -- Criando uma aresta
    let e1 = Edge {weight = 10, prev = v1, next = v2}

    -- Criando o grafo e inserindo elementos
    let g = Graph {vertices = [], edges = []}
    let g1 = insertVertex g v1
    let g2 = insertVertex g1 v2
    let g3 = insertEdge g2 e1

    -- Exibindo o grafo
    print g3
