#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef enum {
    VERTEX,
    EDGE
} Type;

typedef struct {
    int id;
    char name[100];
    int xAxis;
    int yAxis;
} Vertex;

typedef struct {
    int weight;
    Vertex* prev;
    Vertex* next;
} Edge;

typedef union {
    Vertex vertice;
    Edge aresta;
} Data;

struct Element;

typedef struct {
    Type type;
    Data data;
    struct Element* next; 
} Element;

typedef struct {
    int size;
    Type type;
    Element* init;
    Element* end;
} List;

typedef struct {
    List vertex; // Lista de Vertices
    List edge; // Lista de arestas
} Graph;

#define MAX 100

List* createList(Type type) {
    List* list = (List*) malloc(sizeof(List));

    if (list != NULL) {
        list->init = NULL;
        list->end = NULL;
        list->size = 0;
        list->type = type;
    }

    return list;
}

Element* createElement(Type type, void* data) {
    Element* elem = (Element*) malloc(sizeof(Element));

    if (elem == NULL) return NULL;

    elem->type = type;
    elem->next = NULL;

    if (type == VERTEX)
        elem->data.vertice = *(Vertex*)data;
    else if (type == EDGE)
        elem->data.aresta = *(Edge*)data;

    return elem;
}

int insertElement(List* list, Element* element) {
    if (list == NULL || element == NULL) return -1; 

    if (list->init == NULL) {
        list->init = element;
        list->end = element;
    } else {
        list->end->next = element;
        list->end = element;
    }

    list->size++;
    return 0;
}


int removeElement(List* list, Type type, void* data) {
    if (list == NULL || list->init == NULL) return 0;

    Element* current = list->init;
    Element* previous = NULL;

    while (current != NULL) {
        int match = 0;

        // Verifica se o tipo corresponde
        if (current->type == type) {
            if (type == VERTEX) {
                match = memcmp(&current->data.vertice, data, sizeof(Vertex)) == 0;
            } else if (type == EDGE) {
                match = memcmp(&current->data.aresta, data, sizeof(Edge)) == 0;
            }
        }

        if (match) {
            // Se for o primeiro elemento
            if (previous == NULL) {  
                list->init = current->next;
            } else {
                previous->next = current->next;
            }

            // Se for o último elemento
            if (current == list->end) {  
                list->end = previous;
            }

            list->size--;  // Atualiza o tamanho da lista
            free(current);  // Libera a memória
            return 1;
        }

        previous = current;  // Atualiza o elemento anterior
        current = current->next;  // Avança para o próximo
    }

    return 0;  // Elemento não encontrado
}

int isEmpty(List* list){
	return list->size <= 0;
}

void printListaEncadeada(List* list) {
    if (list == NULL) {
        printf("Lista não inicializada.\n");
        return;
    }

    printf("\n---------- IMPRIMINDO LISTA -------------\n\n");
    printf("Lista [tamanho: %d]\n\n", list->size);

    if (list->init == NULL) {
        printf("Lista está vazia.\n");
        return;
    }

    Element* current = list->init;
    while (current != NULL) {
        if (current->type == VERTEX) {
            printVertex(&current->data.vertice);
        } else if (current->type == EDGE) {
            printEdge(&current->data.aresta);
        }
        printf("\n");
        current = current->next;
    }
}

Graph* createGraph() {
    Graph* graph = (Graph*) malloc(sizeof(Graph));

    if (graph != NULL) {
        graph->vertex = *createList(VERTEX);
        graph->edge = *createList(EDGE);
    }

    return graph;
}

int insertVertex(Graph* graph, Vertex* v) {
    if (graph == NULL || v == NULL) return 0;

    Element* newElement = createElement(VERTEX, v);
    if (newElement == NULL) return 0;

    if (graph->vertex.init == NULL) {
        graph->vertex.init = newElement;
    } else {
        graph->vertex.end->next = newElement;
    }
    graph->vertex.end = newElement;
    graph->vertex.size++;
    return 1;
}

int insertEdge(Graph* graph, Edge* e) {
    if (graph == NULL || e == NULL) return 0;

    Element* newElement = createElement(EDGE, e);
    if (newElement == NULL) return 0;

    if (graph->edge.init == NULL) {
        graph->edge.init = newElement;
    } else {
        graph->edge.end->next = newElement;
    }
    graph->edge.end = newElement;
    graph->edge.size++;
    return 1;
}

int removeVertex(Graph* graph, Vertex* v) {
    if (graph == NULL || v == NULL) return 0;

    Element *current = graph->vertex.init;
    Element *previous = NULL;

    while (current != NULL) {
        if (current->type == VERTEX && memcmp(&current->data.vertice, v, sizeof(Vertex)) == 0) {
            if (previous == NULL) {
                graph->vertex.init = current->next;
            } else {
                previous->next = current->next;
            }
            if (current == graph->vertex.end) {
                graph->vertex.end = previous;
            }
            free(current);
            graph->vertex.size--;
            return 1;
        }
        previous = current;
        current = current->next;
    }

    return 0; // Vértice não encontrado
}

int removeEdge(Graph* graph, Edge* e) {
    if (graph == NULL || e == NULL) return 0;

    Element *current = graph->edge.init;
    Element *previous = NULL;

    while (current != NULL) {
        if (current->type == EDGE && memcmp(&current->data.aresta, e, sizeof(Edge)) == 0) {
            if (previous == NULL) {
                graph->edge.init = current->next;
            } else {
                previous->next = current->next;
            }
            if (current == graph->edge.end) {
                graph->edge.end = previous;
            }
            free(current);
            graph->edge.size--;
            return 1;
        }
        previous = current;
        current = current->next;
    }

    return 0;
}

int isEmptyGraph(Graph* graph) {
    if (graph == NULL) return 1;
    return (graph->vertex.size == 0 && graph->edge.size == 0);
}

void printVertex(Vertex* v) {
    if (v) {
        printf("Vertex ID: %d\n", v->id);
        printf("Name: %s\n", v->name);
        printf("X-axis: %d\n", v->xAxis);
        printf("Y-axis: %d\n", v->yAxis);
    }
}

void printEdge(Edge* e) {
    if (e) {
        printf("Weight: %d\n", e->weight);
        printf("Previous Vertex ID: %d\n", e->prev ? e->prev->id : -1);
        printf("Next Vertex ID: %d\n", e->next ? e->next->id : -1);
    }
}

void printGraph(Graph* graph) {
    if (graph == NULL) {
        printf("Grafo não inicializado.\n");
        return;
    }

    printf("\n---------- IMPRIMINDO GRAFO -------------\n\n");

    printf("Vértices:\n");
    Element* current = graph->vertex.init;
    while (current != NULL) {
        if (current->type == VERTEX) printVertex(&current->data.vertice);

        current = current->next;
        printf("\n");
    }
    
    printf("Arestas:\n");
    current = graph->edge.init;
    while (current != NULL) {
        if (current->type == EDGE) printEdge(&current->data.aresta);
        
        current = current->next;
        printf("\n");
    }
}

void generateRandomVertices(Graph* graph, int numVertices) {
    srand(time(NULL));

    for (int i = 0; i < numVertices; i++) {
        Vertex* v = (Vertex*) malloc(sizeof(Vertex));
        v->id = i + 1;
        sprintf(v->name, "Cidade%d", v->id);
        v->xAxis = rand() % 101;
        v->yAxis = rand() % 101;
        insertVertex(graph, v);
    }
}

void generateCompleteGraphEdges(Graph* graph) {
    if (graph == NULL) return;

    Element* vertexElem1 = graph->vertex.init;

    while (vertexElem1 != NULL) {
        Vertex* v1 = &vertexElem1->data.vertice;
        Element* vertexElem2 = vertexElem1->next;

        while (vertexElem2 != NULL) {
            Vertex* v2 = &vertexElem2->data.vertice;

            // Calcula a distância euclidiana entre os vértices
            int weight = (int)calculateDistance(v1, v2);

            // Cria uma aresta com peso
            Edge* e = (Edge*) malloc(sizeof(Edge));
            e->prev = v1;
            e->next = v2;
            e->weight = weight;

            // Insere a aresta na lista de arestas do grafo
            insertEdge(graph, e);

            vertexElem2 = vertexElem2->next;
        }

        vertexElem1 = vertexElem1->next;
    }
}


/*double calculateDistance(Vertex* v1, Vertex* v2) {
    int dx = v1->xAxis - v2->xAxis;
    int dy = v1->yAxis - v2->yAxis;
    double distance = sqrt(dx * dx + dy * dy); 
    return distance * 0.1;
}*/

double calculateDistance(Vertex* v1, Vertex* v2) {
    int dx = v1->xAxis - v2->xAxis;
    int dy = v1->yAxis - v2->yAxis;
    return sqrt(dx * dx + dy * dy);
}


int isInPath(List* path, Vertex* v) {
    Element* elem = path->init;
    while (elem != NULL) {
        if (elem->type == VERTEX && memcmp(&elem->data.vertice, v, sizeof(Vertex)) == 0) {
            return 1; // Vértice já está no caminho
        }
        elem = elem->next;
    }
    return 0; // Quando o vértice não está no caminho
}

int* initializeVisitedArray(int numVertices) {
    int* visited = (int*) malloc(numVertices * sizeof(int));
    if (visited != NULL) {
        for (int i = 0; i < numVertices; i++) {
            visited[i] = 0; // Inicializa todos os vértices como não visitados
        }
    }
    return visited;
}

void findShortestPath(Graph* graph) {
    if (graph == NULL || graph->vertex.size < 2) return;

    // Criação do caminho e array de vértices visitados
    List* path = createList(VERTEX);
    int* visited = initializeVisitedArray(graph->vertex.size);

    Vertex* start = &graph->vertex.init->data.vertice;
    insertElement(path, createElement(VERTEX, start));
    visited[start->id - 1] = 1; // Marca o vértice inicial como visitado

    while (path->size < graph->vertex.size) {
        Element* current = path->end; // Último vértice adicionado ao caminho
        Vertex* lastVertex = &current->data.vertice;
        double minDistance = INFINITY;
        Vertex* nextVertex = NULL;

        // Encontra o vértice mais próximo que ainda não está no caminho
        Element* elem = graph->vertex.init;
        while (elem != NULL) {
            Vertex* candidate = &elem->data.vertice;

            // Verifica se o vértice já foi visitado
            if (!visited[candidate->id - 1]) {
                double distance = calculateDistance(lastVertex, candidate);
                if (distance < minDistance) {
                    minDistance = distance;
                    nextVertex = candidate;
                }
            }
            elem = elem->next;
        }

        if (nextVertex != NULL) {
            insertElement(path, createElement(VERTEX, nextVertex)); // Adiciona o próximo vértice ao caminho
            visited[nextVertex->id - 1] = 1; // Marca o vértice como visitado
        }
    }

    // Voltar ao vértice inicial
    insertElement(path, createElement(VERTEX, start));

    // Exibe o caminho encontrado
    printf("Caminho fechado mais curto:\n");
    printListaEncadeada(path);

    // Libera a memória
    free(visited);
}

void freeList(List* list) {
    Element* current = list->init;
    while (current != NULL) {
        Element* temp = current;
        current = current->next;
        free(temp);
    }
    list->init = NULL;
    list->end = NULL;
    list->size = 0;
}

void freeGraph(Graph* graph) {
    freeList(&graph->vertex);
    freeList(&graph->edge);
    free(graph);
}

void generateDotFile(Graph* graph, const char* filename) {
    FILE* file = fopen(filename, "w");
    if (file == NULL) {
        printf("Erro ao abrir o arquivo %s\n", filename);
        return;
    }

    fprintf(file, "digraph G {\n");

    // Adiciona os vértices
    Element* vertexElem = graph->vertex.init;
    while (vertexElem != NULL) {
        Vertex* v = &vertexElem->data.vertice;
        fprintf(file, "    %d [label=\"%s\"];\n", v->id, v->name);
        vertexElem = vertexElem->next;
    }

    // Adiciona as arestas com pesos
    Element* edgeElem = graph->edge.init;
    while (edgeElem != NULL) {
        Edge* e = &edgeElem->data.aresta;
        if (e->prev != NULL && e->next != NULL) {
            fprintf(file, "    %d -> %d [label=\"%d\"];\n", e->prev->id, e->next->id, e->weight);
        }
        edgeElem = edgeElem->next;
    }

    fprintf(file, "}\n");
    fclose(file);
}

void printShortestPath(Graph* graph) {
    if (graph == NULL || graph->vertex.size < 2) return;

    // Criação do caminho e array de vértices visitados
    List* path = createList(VERTEX);
    int* visited = initializeVisitedArray(graph->vertex.size);

    Vertex* start = &graph->vertex.init->data.vertice;
    insertElement(path, createElement(VERTEX, start));
    visited[start->id - 1] = 1; // Marca o vértice inicial como visitado

    while (path->size < graph->vertex.size) {
        Element* current = path->end; // Último vértice adicionado ao caminho
        Vertex* lastVertex = &current->data.vertice;
        double minDistance = INFINITY;
        Vertex* nextVertex = NULL;

        // Encontra o vértice mais próximo que ainda não está no caminho
        Element* elem = graph->vertex.init;
        while (elem != NULL) {
            Vertex* candidate = &elem->data.vertice;

            // Verifica se o vértice já foi visitado
            if (!visited[candidate->id - 1]) {
                double distance = calculateDistance(lastVertex, candidate);
                if (distance < minDistance) {
                    minDistance = distance;
                    nextVertex = candidate;
                }
            }
            elem = elem->next;
        }

        if (nextVertex != NULL) {
            insertElement(path, createElement(VERTEX, nextVertex)); // Adiciona o próximo vértice ao caminho
            visited[nextVertex->id - 1] = 1; // Marca o vértice como visitado
        }
    }

    // Voltar ao vértice inicial
    insertElement(path, createElement(VERTEX, start));

    // Imprime o caminho encontrado
    printf("Caminho fechado mais curto:\n");

    // Itera sobre o caminho para imprimir as arestas e pesos
    Element* pathElem = path->init;
    Vertex* prevVertex = NULL;

    while (pathElem != NULL) {
        Vertex* currentVertex = &pathElem->data.vertice;
        if (prevVertex != NULL) {
            // Encontra a aresta entre o vértice anterior e o atual
            Element* edgeElem = graph->edge.init;
            while (edgeElem != NULL) {
                Edge* edge = &edgeElem->data.aresta;
                if ((edge->prev == prevVertex && edge->next == currentVertex) ||
                    (edge->prev == currentVertex && edge->next == prevVertex)) {
                    printf("Aresta de %d para %d com peso %d\n",
                           edge->prev->id, edge->next->id, edge->weight);
                    break;
                }
                edgeElem = edgeElem->next;
            }
        }
        prevVertex = currentVertex;
        pathElem = pathElem->next;
    }

    // Libera a memória
    free(visited);
}

void main() {
    Graph* graph = createGraph();

    // Gera 5 vértices aleatórios
    generateRandomVertices(graph, 5);

    // Imprime o grafo com os vértices gerados
    printGraph(graph);

    generateCompleteGraphEdges(graph);

    // Encontra o caminho fechado mais curto que passa por todos os vértices
    //findShortestPath(graph);
    printShortestPath(graph);

    exportGraphToDOT(graph, "graph.dot");

    freeGraph(graph);
}