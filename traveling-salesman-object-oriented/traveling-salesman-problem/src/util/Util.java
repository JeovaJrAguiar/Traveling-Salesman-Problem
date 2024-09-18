package util;

import entity.Vertex;

import java.util.*;

public class Util {

    public static List<Vertex> createVertices(int[][] cities){
        List<Vertex> graph = new ArrayList<>();

        for (int[] p : cities) {
            Vertex v = new Vertex("( " + p[0] + ", " + p[1] + " )", p[0], p[1]);
            graph.add(v);
        }

        for (Vertex v : graph){
            for (Vertex u : graph){
                if (v.name.equals(u.name))
                    continue;

                v.adj.add(u);
                v.adjValues.put(u, Util.calculateDistance(v.xCoordinate, v.yCoordinate, u.xCoordinate, u.yCoordinate));
            }
        }

        return graph;
    }

    public static int[][] createCities(){
        Scanner scanner = new Scanner(System.in);
        System.out.print("\nINFORME A QUANTIDADE DE CIDADES: ");
        int numberOfCities = scanner.nextInt();

        int[][] cities = new int[numberOfCities][2];

        for (int i = 0; i < cities.length; i++) {
            for (int j = 0; j < cities[i].length; j++) {
                cities[i][j] = Util.numberGenerator(100);
            }
        }

        return cities;
    }

    public static void print(List<Vertex> shortestPath){
        double distance = 0.0;
        System.out.print("\nMelhor caminho encontrado: ");
        for (Vertex u : shortestPath){
            distance += u.d;
            System.out.print(u.name + " -> ");
        }
        System.out.print(shortestPath.get(0).name + ".");

        distance += shortestPath.get(0).adjValues.get(shortestPath.get(shortestPath.size() - 1));

        System.out.println("\nDistância total: " + String.format("%.1f", distance) + " Km.");
    }

    public static boolean shortestDistance(Vertex u, Vertex v){
        return u.adjValues.entrySet().stream().sorted(Map.Entry.comparingByValue()).filter(x -> !x.getKey().visit).findFirst().get().getKey().name.equals(v.name);
    }

    public static int numberGenerator(int bound){
        return new Random().nextInt(bound);
    }

    public static double calculateDistance(int x1, int y1, int x2, int y2){
        return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
    }

    public static void printGraph(List<Vertex> graph) {
        for (Vertex u : graph){
            System.out.print("\n\nCoordenadas: " + u.name);
            System.out.print("\nAdjacentes: ");
            u.adjValues.entrySet().stream().forEach(x -> {
                System.out.print("\t\t\t\t\t\t\t\t\n Cidades: " + u.name + " -> " + x.getKey().name);
                System.out.print("\t\t Distância: " + String.format("%.1f", x.getValue()) + " Km");
            });
        }
    }
}
