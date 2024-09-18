package util.dfs;

import entity.Vertex;
import util.Util;

import java.util.List;

public class DFS {
    public static void dfsVisit(List<Vertex> newGraph, Vertex u){
        u.visit = true;

        for (Vertex v : u.adj)
            if (!v.visit && Util.shortestDistance(u, v)){
                u.d += u.adjValues.get(v);
                dfsVisit(newGraph, v);
            }

        newGraph.add(0, u);
    }
}
