#include <stdio.h>
#include <stdlib.h>
#include "lib.h"

int main() {
    Graph* graph = createGraph();

    // Gera 5 vértices aleatórios
    generateRandomVertices(graph, 5);

    // Imprime o grafo com os vértices gerados
    printGraph(graph);

    // Encontra o caminho fechado mais curto que passa por todos os vértices
    findShortestPath(graph);

    return 0;
}