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

List* createList(Type type);

Element* createElement(Type type, void* data);

int insertElement(List* list, Element* element);

int removeElement(List* list, Type type, void* data);

void printListaEncadeada(List* list);

Graph* createGraph();

int insertVertex(Graph* graph, Vertex* v);

int insertEdge(Graph* graph, Edge* e);

int removeVertex(Graph* graph, Vertex* v);

int removeEdge(Graph* graph, Edge* e);

int isEmptyGraph(Graph* graph);

void printVertex(Vertex* v);

void printEdge(Edge* e);

void printGraph(Graph* graph);

void generateRandomVertices(Graph* graph, int numVertices);