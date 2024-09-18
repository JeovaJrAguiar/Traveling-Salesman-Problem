import entity.Vertex;
import util.Util;
import util.dfs.DFS;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int op;
        int[][] cities;
        List<Vertex> graph = null;
        do{
            System.out.println("\n\n\n\t\t\t\tPROBLEMA DO CAIXEIRO VIAJANTE\n");
            System.out.println("\t\t\t\t1  - CRIAR CIDADES\n\t\t\t\t2  - INICIAR\n\t\t\t\t3  - VER CONFIGURAÇÃO ATUAL\n\t\t\t\t0  - SAIR");
            System.out.print("\nINFORME SUA OPCAO: ");
            op = scanner.nextInt();

            switch(op) {
                case 1:
                    cities = Util.createCities();
                    graph = Util.createVertices(cities);
                    break;
                case 2:
                    if (graph == null){
                        System.out.println("\n\n\tCIDADES NÃO CRIADAS! APERTE 1 PARA CRIAR.\n\n");
                        break;
                    }
                    start(graph.get(Util.numberGenerator(graph.size())));
                    break;
                case 3:
                    if (graph == null){
                        System.out.println("\n\n\tCIDADES NÃO CRIADAS! APERTE 1 PARA CRIAR.\n\n");
                        break;
                    }
                    Util.printGraph(graph);
                    break;
                case 0:
                    System.out.println("\nENCERRANDO PROGRAMA");
                    break;
                default:
                    System.out.println("\nOPCAO INCORRETA, TENTAR NOVAMENTE.");
                    break;
            }
        }
        while(op != 0);
    }

    public static void start(Vertex s){
        if (s == null){
            System.out.println("\n\n\tCIDADES NÃO CRIADAS! APERTE 1 PARA CRIAR.\n\n");
            return;
        }

        List<Vertex> shortestPath = new ArrayList<>();

        DFS.dfsVisit(shortestPath, s);

        Util.print(shortestPath);
    }
}